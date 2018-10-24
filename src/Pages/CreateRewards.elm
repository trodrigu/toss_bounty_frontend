module Pages.CreateRewards exposing (Error, ExternalMsg(..), Field(..), Model, Msg(..), createRewardForm, deletePlan, deleteReward, filterPersistedRewards, ifZero, init, planHasId, postPlan, postReward, putPlan, putReward, renderDashButton, rewardHasId, update, updateRewardForm, view, viewErrors)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Plan as Plan exposing (Plan)
import Data.Reward as Reward exposing (Reward)
import Html exposing (..)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router exposing (Route(..))
import SelectList as SelectList exposing (SelectList)
import Validate exposing (ifBlank, fromErrors, validate, Validator, fromValid)
import Browser.Navigation as Navigation exposing (Key)


init : Key -> Maybe String -> AuthToken -> Int -> Model
init key apiUrl token campaignId =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just matchedUrl ->
                    matchedUrl
    in
    { rewards = SelectList.singleton Reward.default
    , plans = SelectList.singleton Plan.default
    , rewardId = ""
    , description = ""
    , donationLevel = 0.0
    , apiUrl = url
    , token = token
    , campaignId = campaignId
    , errors = []
    , isEditing = False
    , key = key
    }


type alias Model =
    { rewardId : String
    , description : String
    , donationLevel : Float
    , rewards : SelectList Reward
    , plans : SelectList Plan
    , apiUrl : String
    , token : AuthToken
    , campaignId : Int
    , errors : List Error
    , isEditing : Bool
    , key : Key
    }



-- All Msg


type Msg
    = SaveRewardForm
    | SaveUpdateForm
    | UpdateDescriptionField String
    | UpdateDonateLevelField String
    | HandlePlan (WebData Plan)
    | HandleReward (WebData Reward)
    | HandlePutReward (WebData Reward)
    | HandleDeleteReward (WebData String)
    | HandleDeletePlan (WebData String)
    | SelectReward Int
    | DeletePlan Int
    | DiscoverPage
    | HandlePutPlan (WebData Plan)


type ExternalMsg
    = NoOp
    | MakeMainFetchCampaigns


view : Model -> Html Msg
view model =
    let
        rewards =
            SelectList.toList model.rewards

        persistedRewards =
            filterPersistedRewards rewards
    in
    case model.isEditing of
        True ->
            div []
                [ div
                    []
                    (List.indexedMap
                        (\index reward ->
                            case SelectList.selected model.rewards == reward of
                                True ->
                                    div []
                                        [ updateRewardForm model
                                        ]

                                False ->
                                    div []
                                        [ div
                                            []
                                            [ section [ class "section" ]
                                                [ div [ class "container" ]
                                                    [ div [ class "columns" ]
                                                        [ div [ class "column is-half is-offset-one-quarter" ]
                                                            [ div [ class "card" ]
                                                                [ div [ class "card-header" ]
                                                                    [ p [ class "card-header-title" ]
                                                                        [ text ("Reward #" ++ String.fromInt (index + 1)) ]
                                                                    , a [ class "card-header-icon", onClick (SelectReward reward.id) ]
                                                                        [ span [] [ text "edit" ] ]
                                                                    , a [ class "card-header-icon", onClick (DeletePlan reward.id) ]
                                                                        [ span [] [ text "delete" ] ]
                                                                    ]
                                                                , div [ class "card-content" ]
                                                                    [ label [ class "label" ]
                                                                        [ text "Donation Level" ]
                                                                    , p [ class "control" ]
                                                                        [ text (String.fromFloat reward.donationLevel) ]
                                                                    , label [ class "label" ]
                                                                        [ text "Description" ]
                                                                    , p []
                                                                        [ text reward.description ]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                        )
                        persistedRewards
                    )
                ]

        False ->
            div []
                [ createRewardForm model
                , div
                    []
                    (List.indexedMap
                        (\index reward ->
                            div []
                                [ div
                                    []
                                    [ section [ class "section" ]
                                        [ div [ class "container" ]
                                            [ div [ class "columns" ]
                                                [ div [ class "column is-half is-offset-one-quarter" ]
                                                    [ div [ class "card" ]
                                                        [ div [ class "card-header" ]
                                                            [ p [ class "card-header-title" ]
                                                                [ text ("Reward #" ++ String.fromInt (index + 1)) ]
                                                            , a [ class "card-header-icon", onClick (SelectReward reward.id) ]
                                                                [ span [] [ text "edit" ] ]
                                                            , a [ class "card-header-icon", onClick (DeletePlan reward.id) ]
                                                                [ span [] [ text "delete" ] ]
                                                            ]
                                                        , div [ class "card-content" ]
                                                            [ label [ class "label" ]
                                                                [ text "Donation Level" ]
                                                            , p [ class "control" ]
                                                                [ text (String.fromFloat reward.donationLevel) ]
                                                            , label [ class "label" ]
                                                                [ text "Description" ]
                                                            , p []
                                                                [ text reward.description ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                        )
                        persistedRewards
                    )
                , renderDashButton persistedRewards
                ]


renderDashButton : List Reward -> Html Msg
renderDashButton rewards =
    let
        rewardsLength =
            List.length rewards

        updatedButton =
            case rewardsLength > 0 of
                True ->
                    section [ class "section" ]
                        [ div [ class "container" ]
                            [ div [ class "level" ]
                                [ div [ class "level-item" ]
                                    [ button [ class "button is-primary is-large", onClick DiscoverPage ] [ text "Finish And Go To Dash" ]
                                    ]
                                ]
                            ]
                        ]

                False ->
                    div [] []
    in
    updatedButton


filterPersistedRewards : List Reward -> List Reward
filterPersistedRewards rewardList =
    List.filter rewardHasId rewardList


planHasId : Plan -> Bool
planHasId plan =
    not (plan.id == "")


rewardHasId : Reward -> Bool
rewardHasId reward =
    not (reward.id == 0)


updateRewardForm : Model -> Html Msg
updateRewardForm model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ div [ class "card" ]
                        [ div [ class "card-content" ]
                            [ viewErrors model.errors
                            , div [ class "field" ]
                                [ label [ class "label" ]
                                    [ text "Donation Level" ]
                                , p [ class "control" ]
                                    [ input
                                        [ class "input"
                                        , Html.Attributes.type_ "number"
                                        , onInput UpdateDonateLevelField
                                        , value (String.fromFloat model.donationLevel)
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field" ]
                                [ label [ class "label" ]
                                    [ text "Description of reward" ]
                                , p [ class "control" ]
                                    [ input
                                        [ class "input"
                                        , onInput UpdateDescriptionField
                                        , value model.description
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "field is-grouped" ]
                                [ p [ class "control" ]
                                    [ button [ class "button is-primary", onClick SaveUpdateForm ]
                                        [ text "Update Reward" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


createRewardForm : Model -> Html Msg
createRewardForm model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ h1 [ class "title" ] [ text "What kind of rewards will you have?" ]
                    , viewErrors model.errors
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Donation Amount" ]
                        , p [ class "control has-icons-left" ]
                            [ input
                                [ class "input"
                                , Html.Attributes.type_ "number"
                                , onInput UpdateDonateLevelField
                                , value (String.fromFloat model.donationLevel)
                                ]
                                []
                            , span [ class "icon is-left" ]
                                [ i [ class "fas fa-money-bill-alt" ] []
                                ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Description of reward" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , onInput UpdateDescriptionField
                                , value model.description
                                ]
                                []
                            ]
                        ]
                    , div [ class "field is-grouped" ]
                        [ p [ class "control" ]
                            [ button [ class "button is-primary", onClick SaveRewardForm ]
                                [ text "Create Reward" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdateDonateLevelField str ->
            let
                updatedDonateLevel =
                    case String.toFloat str of
                        Just donationLevel ->
                            donationLevel

                        Nothing ->
                            0.0
            in
            (( { model | donationLevel = updatedDonateLevel }, Cmd.none ) , NoOp)

        SelectReward rewardId ->
            let
                updatedRewards =
                    SelectList.select (\u -> u.id == rewardId) model.rewards

                selectedReward =
                    SelectList.selected updatedRewards

                updatedPlans =
                    SelectList.select (\u -> u.rewardId == rewardId) model.plans

                selectedPlan =
                    SelectList.selected updatedPlans
            in
            (( { model
                | rewards = updatedRewards
                , isEditing = True
                , description = selectedReward.description
                , donationLevel = selectedReward.donationLevel
                , plans = updatedPlans
              }
            , Cmd.none
            )
                , NoOp)

        UpdateDescriptionField str ->
            (( { model | description = str }, Cmd.none ) , NoOp)

        HandlePutPlan data ->
            case data of
                Success plan ->
                    let
                        currentSelectedPlan =
                            SelectList.selected model.plans

                        updatedPlan =
                            { currentSelectedPlan
                                | amount = plan.amount
                                , name = plan.name
                            }

                        befores =
                            SelectList.before model.plans

                        afters =
                            SelectList.after model.plans

                        updatedPlans =
                            SelectList.singleton updatedPlan
                                |> SelectList.prepend befores
                                |> SelectList.append afters

                        updatedModel =
                            { model | plans = updatedPlans }
                    in
                    (( updatedModel, putReward updatedModel )
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        HandlePutReward data ->
            case data of
                Success reward ->
                    let
                        currentSelectedReward =
                            SelectList.selected model.rewards

                        updatedReward =
                            { currentSelectedReward
                                | description = reward.description
                                , donationLevel = reward.donationLevel
                            }

                        befores =
                            SelectList.before model.rewards

                        afters =
                            SelectList.after model.rewards

                        updatedRewards =
                            SelectList.singleton updatedReward
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    (( { model | rewards = updatedRewards, description = "", donationLevel = 0.0, isEditing = False }, Cmd.none )
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        HandleDeletePlan data ->
            case data of
                Success _ ->
                    let
                        selectedPlan =
                            SelectList.selected model.plans

                        planBefores =
                            SelectList.before model.plans

                        planAfters =
                            SelectList.after model.plans

                        planBeforesAndAfters =
                            planBefores ++ planAfters

                        defaultPlan =
                            List.filter (\innerPlan -> not (planHasId innerPlan)) planBeforesAndAfters
                                |> List.head

                        plan =
                            case defaultPlan of
                                Just innerPlan ->
                                    innerPlan

                                _ ->
                                    Plan.default

                        planAsSelectList =
                            SelectList.singleton plan

                        updatedPlans =
                            planAsSelectList
                                |> SelectList.prepend planBefores
                                |> SelectList.append planAfters

                        updatedModel =
                            { model
                                | plans = updatedPlans
                            }
                    in
                    ((updatedModel
                        , deleteReward updatedModel)
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        HandleDeleteReward data ->
            case data of
                Success _ ->
                    let
                        selectedReward =
                            SelectList.selected model.rewards

                        rewardBefores =
                            SelectList.before model.rewards

                        rewardAfters =
                            SelectList.after model.rewards

                        rewardBeforesAndAfters =
                            rewardBefores ++ rewardAfters

                        defaultReward =
                            List.filter (\innerReward -> not (rewardHasId innerReward)) rewardBeforesAndAfters
                                |> List.head

                        reward =
                            case defaultReward of
                                Just innerReward ->
                                    innerReward

                                _ ->
                                    Reward.default

                        rewardAsSelectList =
                            SelectList.singleton reward

                        updatedRewards =
                            rewardAsSelectList
                                |> SelectList.prepend rewardBefores
                                |> SelectList.append rewardAfters

                        updatedModel =
                            { model
                                | rewards = updatedRewards
                                , description = ""
                                , donationLevel = 0.0
                                , isEditing = False
                            }
                    in
                    ((updatedModel
                        , Cmd.none)
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        HandlePlan data ->
            case data of
                Success plan ->
                    let
                        currentSelectedPlan =
                            SelectList.selected model.plans

                        newPlan =
                            SelectList.singleton plan

                        befores =
                            SelectList.before model.plans

                        afters =
                            SelectList.after model.plans

                        aftersWithCurrentSelectedPlan =
                            case planHasId currentSelectedPlan of
                                True ->
                                    currentSelectedPlan :: afters

                                False ->
                                    afters

                        updatedPlans =
                            newPlan
                                |> SelectList.prepend befores
                                |> SelectList.append aftersWithCurrentSelectedPlan
                    in
                    (({ model | plans = updatedPlans }
                        , Cmd.none)
                        , NoOp)

                error ->
                    (( model, Cmd.none )
                        , NoOp)

        HandleReward data ->
            case data of
                Success reward ->
                    let
                        currentSelectedReward =
                            SelectList.selected model.rewards

                        newReward =
                            SelectList.singleton reward

                        befores =
                            SelectList.before model.rewards

                        afters =
                            SelectList.after model.rewards

                        aftersWithCurrentSelectedReward =
                            case rewardHasId currentSelectedReward of
                                True ->
                                    currentSelectedReward :: afters

                                False ->
                                    afters

                        updatedRewards =
                            newReward
                                |> SelectList.prepend befores
                                |> SelectList.append aftersWithCurrentSelectedReward

                        updatedModel =
                            { model | rewards = updatedRewards, description = "", donationLevel = 0.0, isEditing = False }
                    in
                    ((updatedModel
                        , postPlan updatedModel)
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        SaveUpdateForm ->
            case validate formValidator model of
                Ok matchedSubject ->
                    let
                        updatedModel =
                            fromValid matchedSubject

                        newModel =
                            { updatedModel | errors = [] }
                    in
                    (( newModel, putPlan newModel ) , NoOp)

                Err errors ->
                    (({ model | errors = errors }
                        , Cmd.none)
                        , NoOp)

        DeletePlan rewardId ->
            let
                updatedPlans =
                    SelectList.select (\u -> u.rewardId == rewardId) model.plans

                updatedModel =
                    { model
                        | plans = updatedPlans
                    }
            in
            ((updatedModel
                , deletePlan updatedModel)
                , NoOp)

        SaveRewardForm ->
            case validate formValidator model of
                Ok matchedSubject ->
                    let
                        updatedModel =
                            fromValid matchedSubject

                        newModel =
                            { model | errors = [] }
                    in
                    (( newModel, postReward newModel ) , NoOp)

                Err errors ->
                    (({ model | errors = errors }
                        , Cmd.none)
                        , NoOp)

        DiscoverPage ->
            ((model , Router.modifyUrl model.key Router.DashRoute), MakeMainFetchCampaigns)


postReward : Model -> Cmd Msg
postReward model =
    let
        rewardUrl =
            model.apiUrl ++ "/rewards"

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = model.campaignId
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) rewardUrl HandleReward Reward.showDecoder (Reward.encode data)


putPlan : Model -> Cmd Msg
putPlan model =
    let
        selectedReward =
            SelectList.selected model.rewards

        selectedPlan =
            SelectList.selected model.plans

        planUrl =
            model.apiUrl ++ "/plans/" ++ selectedPlan.id

        data =
            { amount = selectedReward.donationLevel
            , interval = "month"
            , name = selectedReward.description
            , currency = "usd"
            , rewardId = selectedReward.id
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) planUrl HandlePutPlan Plan.showDecoder (Plan.encode data)


putReward : Model -> Cmd Msg
putReward model =
    let
        selectedReward =
            SelectList.selected model.rewards

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ String.fromInt selectedReward.id

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = model.campaignId
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) rewardUrl HandlePutReward Reward.showDecoder (Reward.encode data)


deleteReward : Model -> Cmd Msg
deleteReward model =
    let
        selectedReward =
            SelectList.selected model.rewards

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ String.fromInt selectedReward.id

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = model.campaignId
            }
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteReward (Reward.encode data)


postPlan : Model -> Cmd Msg
postPlan model =
    let
        planUrl =
            model.apiUrl ++ "/plans"

        selectedReward =
            model.rewards
                |> SelectList.selected

        data =
            { amount = selectedReward.donationLevel
            , interval = "month"
            , name = selectedReward.description
            , currency = "usd"
            , rewardId = selectedReward.id
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) planUrl HandlePlan Plan.showDecoder (Plan.encode data)


deletePlan : Model -> Cmd Msg
deletePlan model =
    let
        selectedPlan =
            SelectList.selected model.plans

        planUrl =
            model.apiUrl ++ "/plans/" ++ selectedPlan.id

        selectedReward =
            model.rewards
                |> SelectList.selected

        data =
            { amount = model.donationLevel
            , interval = "month"
            , name = model.description
            , currency = "usd"
            , rewardId = selectedReward.id
            }
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) planUrl HandleDeletePlan (Plan.encode data)


type Field
    = Form
    | Description
    | DonateLevel


type alias Error =
    ( Field, String )


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "help is-danger" ]


formValidator : Validator (Field, String) Model
formValidator =
    Validate.all
        [ ifBlank .description (Description , "Description can't be blank.")
        , ifZero
        ]


ifZero : Validator (Field, String) Model
ifZero =
    fromErrors modelToIfZero

modelToIfZero : Model -> List (Field, String)
modelToIfZero model =
    let
        fundingGoal =
            model.donationLevel
    in
    if fundingGoal > 0.0 then
        [(DonateLevel, "Donation level can't be zero.")]
    else 
        []

