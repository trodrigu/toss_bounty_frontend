module Pages.CreateRewards exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Reward as Reward exposing (Reward)
import Html exposing (..)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import SelectList as SelectList exposing (SelectList)
import Util exposing ((=>))
import Validate exposing (ifBlank)


init : Maybe String -> AuthToken -> String -> Model
init apiUrl token campaignId =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { rewards = SelectList.singleton { id = "", donationLevel = 100.0, description = "" }
    , rewardId = ""
    , description = ""
    , donationLevel = 0.0
    , apiUrl = url
    , token = token
    , campaignId = campaignId
    , errors = []
    , isEditing = False
    }


type alias Model =
    { rewardId : String
    , description : String
    , donationLevel : Float
    , rewards : SelectList Reward
    , apiUrl : String
    , token : AuthToken
    , campaignId : String
    , errors : List Error
    , isEditing : Bool
    }


type Msg
    = SaveRewardForm
    | SaveUpdateForm
    | UpdateDescriptionField String
    | UpdateDonateLevelField String
    | HandleReward (WebData Reward)
    | HandlePutReward (WebData Reward)
    | HandleDeleteReward (WebData String)
    | SelectReward String
    | DeleteReward String


type ExternalMsg
    = NoOp


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
                                                                        [ text ("Reward #" ++ toString (index + 1)) ]
                                                                    , a [ class "card-header-icon", onClick (SelectReward reward.id) ]
                                                                        [ span [] [ text "edit" ] ]
                                                                    , a [ class "card-header-icon", onClick (DeleteReward reward.id) ]
                                                                        [ span [] [ text "delete" ] ]
                                                                    ]
                                                                , div [ class "card-content" ]
                                                                    [ label [ class "label" ]
                                                                        [ text "Donation Level" ]
                                                                    , p [ class "control" ]
                                                                        [ text (toString reward.donationLevel) ]
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
                [ div
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
                                                                [ text ("Reward #" ++ toString (index + 1)) ]
                                                            , a [ class "card-header-icon", onClick (SelectReward reward.id) ]
                                                                [ span [] [ text "edit" ] ]
                                                            , a [ class "card-header-icon", onClick (DeleteReward reward.id) ]
                                                                [ span [] [ text "delete" ] ]
                                                            ]
                                                        , div [ class "card-content" ]
                                                            [ label [ class "label" ]
                                                                [ text "Donation Level" ]
                                                            , p [ class "control" ]
                                                                [ text (toString reward.donationLevel) ]
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
                , createRewardForm model
                ]


filterPersistedRewards : List Reward -> List Reward
filterPersistedRewards rewardList =
    List.filter hasId rewardList


hasId : Reward -> Bool
hasId reward =
    not (reward.id == "")


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
                                        , value (toString model.donationLevel)
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
                            [ text "Donation Level" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , Html.Attributes.type_ "number"
                                , onInput UpdateDonateLevelField
                                , value (toString model.donationLevel)
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
                        Ok donationLevel ->
                            donationLevel

                        Err error ->
                            0.0
            in
            ( { model | donationLevel = updatedDonateLevel }, Cmd.none ) => NoOp

        SelectReward rewardId ->
            let
                updatedRewards =
                    SelectList.select (\u -> u.id == rewardId) model.rewards

                selectedReward =
                    SelectList.selected updatedRewards
            in
            ( { model | rewards = updatedRewards, isEditing = True, description = selectedReward.description, donationLevel = selectedReward.donationLevel }, Cmd.none ) => NoOp

        UpdateDescriptionField str ->
            ( { model | description = str }, Cmd.none ) => NoOp

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
                    { model | rewards = updatedRewards, description = "", donationLevel = 0.0, isEditing = False }
                        => Cmd.none
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        HandleDeleteReward data ->
            case data of
                Success _ ->
                    let
                        selectedReward =
                            SelectList.selected model.rewards

                        befores =
                            SelectList.before model.rewards

                        afters =
                            SelectList.after model.rewards

                        beforesAndAfters =
                            befores ++ afters

                        defaultReward =
                            List.filter (\reward -> not (hasId reward)) beforesAndAfters
                                |> List.head

                        reward =
                            case defaultReward of
                                Just reward ->
                                    reward

                                _ ->
                                    Reward "" "" 0.0

                        rewardAsSelectList =
                            SelectList.singleton reward

                        updatedRewards =
                            rewardAsSelectList
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    { model | rewards = updatedRewards, description = "", donationLevel = 0.0, isEditing = False }
                        => Cmd.none
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

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
                            case hasId currentSelectedReward of
                                True ->
                                    currentSelectedReward :: afters

                                False ->
                                    afters

                        updatedRewards =
                            newReward
                                |> SelectList.prepend befores
                                |> SelectList.append aftersWithCurrentSelectedReward
                    in
                    { model | rewards = updatedRewards, description = "", donationLevel = 0.0, isEditing = False }
                        => Cmd.none
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        SaveUpdateForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    ( model, putReward model ) => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        DeleteReward rewardId ->
            let
                updatedRewards =
                    SelectList.select (\u -> u.id == rewardId) model.rewards

                updatedModel =
                    { model | rewards = updatedRewards }
            in
            ( updatedModel, deleteReward updatedModel ) => NoOp

        SaveRewardForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    ( model, postReward model ) => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp


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
    RemoteData.Http.postWithConfig (Auth.config model.token) rewardUrl HandleReward Reward.decoder (Reward.encode data)


putReward : Model -> Cmd Msg
putReward model =
    let
        selectedReward =
            SelectList.selected model.rewards

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ selectedReward.id

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = model.campaignId
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) rewardUrl HandlePutReward Reward.decoder (Reward.encode data)


deleteReward : Model -> Cmd Msg
deleteReward model =
    let
        selectedReward =
            SelectList.selected model.rewards

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ selectedReward.id

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = model.campaignId
            }
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteReward (Reward.encode data)


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


validate : Model -> List Error
validate =
    Validate.all
        [ .description >> ifBlank (Description => "Description can't be blank.")
        , .donationLevel >> ifZero (DonateLevel => "Donate Level can't be zero.")
        ]


ifZero : error -> Validate.Validator error Float
ifZero error subject =
    let
        errors =
            case subject > 0.0 of
                True ->
                    []

                False ->
                    [ error ]
    in
    errors