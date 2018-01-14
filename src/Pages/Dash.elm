module Pages.Dash exposing (..)

import Data.AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, defaultCampaign, defaultDate, encode, showDecoder)
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import SelectList exposing (SelectList, fromLists, select, selected, singleton)
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))
import Validate exposing (ifBlank)


type Field
    = Form
    | ShortDescription
    | LongDescription
    | FundingGoal


type alias Error =
    ( Field, String )


type alias Model =
    { campaignId : String
    , currentFunding : Float
    , fundingEndDate : DateTime
    , fundingGoal : Float
    , longDescription : String
    , shortDescription : String
    , yourCampaigns : SelectList Campaign
    , campaignsContributedTo : List Campaign
    , apiUrl : String
    , token : AuthToken
    , errors : List Error
    , isEditingYourCampaigns : Bool
    }


init : Maybe String -> AuthToken -> List Campaign -> List Campaign -> Model
init apiUrl token yourCampaigns campaignsContributedTo =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url

        defaultYourCampaign =
            SelectList.singleton Campaign.defaultCampaign

        updatedYourCampaigns =
            defaultYourCampaign
                |> SelectList.append yourCampaigns
    in
    { campaignId = ""
    , currentFunding = 0.0
    , fundingEndDate =
        DateTime.dateTime
            { year = 1992
            , month = 5
            , day = 29
            , hour = 0
            , minute = 0
            , second = 0
            , millisecond = 0
            }
    , fundingGoal = 0.0
    , longDescription = ""
    , shortDescription = ""
    , yourCampaigns = updatedYourCampaigns
    , campaignsContributedTo = campaignsContributedTo
    , apiUrl = url
    , token = token
    , errors = []
    , isEditingYourCampaigns = False
    }


type Msg
    = SelectYourCampaign String
    | SaveUpdateCampaignForm
    | UpdateFundingGoalField String
    | UpdateLongDescriptionField String
    | UpdateShortDescriptionField String
    | HandlePutCampaign (WebData Campaign)
    | DeleteCampaign String
    | HandleDeleteCampaign (WebData String)


type ExternalMsg
    = NoOp


validate : Model -> List Error
validate =
    Validate.all
        [ .shortDescription >> ifBlank (ShortDescription => "Short Description can't be blank.")
        , .longDescription >> ifBlank (LongDescription => "Long Description can't be blank.")
        , .fundingGoal >> ifZero (FundingGoal => "Funding Goal can't be zero.")
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        HandleDeleteCampaign data ->
            case data of
                Success _ ->
                    let
                        selectedCampaign =
                            SelectList.selected model.yourCampaigns

                        befores =
                            SelectList.before model.yourCampaigns

                        afters =
                            SelectList.after model.yourCampaigns

                        beforesAndAfters =
                            befores ++ afters

                        defaultSelectedCampaign =
                            List.filter (\campaign -> not (hasId campaign)) beforesAndAfters
                                |> List.head
                                |> Maybe.withDefault defaultCampaign

                        campaignAsSelectList =
                            SelectList.singleton defaultSelectedCampaign

                        updatedCampaigns =
                            campaignAsSelectList
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    { model | yourCampaigns = updatedCampaigns, shortDescription = "", longDescription = "", fundingGoal = 0.0, fundingEndDate = Campaign.defaultDate }
                        => Cmd.none
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        SaveUpdateCampaignForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    ( model, putCampaign model ) => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        DeleteCampaign rewardId ->
            let
                updatedCampaigns =
                    SelectList.select (\u -> u.id == rewardId) model.yourCampaigns

                updatedModel =
                    { model | yourCampaigns = updatedCampaigns }
            in
            ( updatedModel, deleteCampaign updatedModel ) => NoOp

        SelectYourCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\campaign -> campaign.id == campaignId) model.yourCampaigns

                selectedCampaign =
                    SelectList.selected updatedCampaigns
            in
            ( { model
                | yourCampaigns = updatedCampaigns
                , isEditingYourCampaigns = True
                , shortDescription = selectedCampaign.shortDescription
                , longDescription = selectedCampaign.longDescription
                , fundingGoal = selectedCampaign.fundingGoal
              }
            , Cmd.none
            )
                => NoOp

        UpdateFundingGoalField updatedFundingGoal ->
            let
                updatedFundingGoalFloat =
                    case String.toFloat updatedFundingGoal of
                        Ok updatedFundingGoalFloat ->
                            updatedFundingGoalFloat

                        Err error ->
                            0.0
            in
            ( { model | fundingGoal = updatedFundingGoalFloat }, Cmd.none ) => NoOp

        UpdateLongDescriptionField updatedLongDescription ->
            ( { model | longDescription = updatedLongDescription }, Cmd.none ) => NoOp

        UpdateShortDescriptionField updatedShortDescription ->
            ( { model | shortDescription = updatedShortDescription }, Cmd.none ) => NoOp

        HandlePutCampaign data ->
            case data of
                Success campaign ->
                    let
                        currentSelectedCampaign =
                            SelectList.selected model.yourCampaigns

                        updatedCampaign =
                            { currentSelectedCampaign
                                | longDescription = campaign.longDescription
                                , shortDescription = campaign.shortDescription
                                , fundingGoal = campaign.fundingGoal
                                , fundingEndDate = campaign.fundingEndDate
                            }

                        befores =
                            SelectList.before model.yourCampaigns

                        afters =
                            SelectList.after model.yourCampaigns

                        updatedCampaigns =
                            SelectList.singleton updatedCampaign
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    { model
                        | yourCampaigns = updatedCampaigns
                        , currentFunding = 0.0
                        , fundingEndDate =
                            DateTime.dateTime
                                { year = 1992
                                , month = 5
                                , day = 29
                                , hour = 0
                                , minute = 0
                                , second = 0
                                , millisecond = 0
                                }
                        , fundingGoal = 0.0
                        , longDescription = ""
                        , shortDescription = ""
                        , isEditingYourCampaigns = False
                    }
                        => Cmd.none
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp


putCampaign : Model -> Cmd Msg
putCampaign model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        rewardUrl =
            model.apiUrl ++ "/campaigns/" ++ selectedCampaign.id

        data =
            { longDescription = selectedCampaign.longDescription
            , shortDescription = selectedCampaign.shortDescription
            , fundingEndDate = selectedCampaign.fundingEndDate
            , fundingGoal = selectedCampaign.fundingGoal
            , userId = selectedCampaign.userId
            , githubRepoId = selectedCampaign.githubRepoId
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) rewardUrl HandlePutCampaign Campaign.showDecoder (Campaign.encode data)


view : Model -> Html Msg
view model =
    div
        []
        [ yourBounties model
        , campaignsYouContributedTo model.campaignsContributedTo
        ]


yourBounties : Model -> Html Msg
yourBounties model =
    let
        campaigns =
            model.yourCampaigns

        campaignsAsList =
            SelectList.toList campaigns

        persistedCampaigns =
            filterPersistedCampaigns campaignsAsList
    in
    case model.isEditingYourCampaigns of
        True ->
            div [] <|
                List.indexedMap
                    (\index campaign ->
                        case SelectList.selected model.yourCampaigns == campaign of
                            True ->
                                div []
                                    [ updateCampaignForm model campaign ]

                            False ->
                                showYourCampaign campaign
                    )
                    persistedCampaigns

        False ->
            div [] <|
                List.map
                    (\campaign ->
                        showYourCampaign campaign
                    )
                    persistedCampaigns


updateCampaignForm : Model -> Campaign -> Html Msg
updateCampaignForm model campaign =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ div [ class "card" ]
                        [ div [ class "card-content" ]
                            [ viewErrors model.errors
                            , displayUpdateShortDescription model
                            , displayUpdateLongDescription model
                            , displayUpdateFundingGoal model
                            , displayUpdateButton
                            ]
                        ]
                    ]
                ]
            ]
        ]


filterPersistedCampaigns : List Campaign -> List Campaign
filterPersistedCampaigns campaignList =
    List.filter hasId campaignList


hasId : Campaign -> Bool
hasId campaign =
    not (campaign.id == "")


campaignsYouContributedTo : List Campaign -> Html Msg
campaignsYouContributedTo campaigns =
    div [] <|
        List.map
            (\campaign ->
                showCampaignYouContributedTo campaign
            )
            campaigns


showYourCampaign : Campaign -> Html Msg
showYourCampaign campaign =
    section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ div
                [ class "columns" ]
                [ div
                    [ class "column is-half is-offset-one-quarter" ]
                    [ div [ class "card" ]
                        [ displayFormHeader campaign
                        , displayFormContent campaign
                        ]
                    ]
                ]
            ]
        ]


displayUpdateButton : Html Msg
displayUpdateButton =
    div [ class "field is-grouped" ]
        [ p [ class "control" ]
            [ button [ class "button is-primary", onClick SaveUpdateCampaignForm ]
                [ text "Update Campaign" ]
            ]
        ]


displayUpdateShortDescription : Model -> Html Msg
displayUpdateShortDescription model =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Short Description" ]
        , p [ class "control" ]
            [ input
                [ class "input"
                , onInput UpdateShortDescriptionField
                , Html.Attributes.value model.shortDescription
                ]
                []
            ]
        ]


displayUpdateLongDescription : Model -> Html Msg
displayUpdateLongDescription model =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Long Description" ]
        , p [ class "control" ]
            [ input
                [ class "input"
                , onInput UpdateLongDescriptionField
                , Html.Attributes.value model.longDescription
                ]
                []
            ]
        ]


displayUpdateFundingGoal : Model -> Html Msg
displayUpdateFundingGoal model =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Funding Goal" ]
        , p [ class "control" ]
            [ input
                [ class "input"
                , Html.Attributes.type_ "number"
                , onInput UpdateFundingGoalField
                , Html.Attributes.value (toString model.fundingGoal)
                ]
                []
            ]
        ]


displayFormHeader : Campaign -> Html Msg
displayFormHeader campaign =
    div [ class "card-header" ]
        [ p [ class "card-header-title" ]
            [ text (toString campaign.shortDescription) ]
        , a [ class "card-header-icon", onClick (SelectYourCampaign campaign.id) ]
            [ span [] [ text "edit" ] ]
        , a [ class "card-header-icon", onClick (DeleteCampaign campaign.id) ]
            [ span [] [ text "delete" ] ]
        ]


displayFormContent : Campaign -> Html Msg
displayFormContent campaign =
    div [ class "card-content" ]
        [ label [ class "label" ]
            [ text "Long Description" ]
        , p [ class "control" ]
            [ text (toString campaign.longDescription) ]
        , label [ class "label" ]
            [ text "Funding Goal" ]
        , p []
            [ text (toString campaign.fundingGoal) ]
        , label [ class "label" ]
            [ text "Funding End Date" ]
        , p []
            [ text (formatDateTime campaign) ]
        ]


formatDateTime : Campaign -> String
formatDateTime campaign =
    let
        fundingEndDate =
            campaign.fundingEndDate

        year =
            DateTime.year fundingEndDate
                |> toString

        month =
            DateTime.month fundingEndDate
                |> toString

        day =
            DateTime.day fundingEndDate
                |> toString
    in
    year ++ "-" ++ month ++ "-" ++ day


showCampaignYouContributedTo : Campaign -> Html Msg
showCampaignYouContributedTo campaign =
    div
        [ class "box" ]
        [ article
            [ class "media" ]
            [ div
                [ class "media-left" ]
                [ figure
                    [ class "image" ]
                    [ img
                        [ src "http://placekitten.com.s3.amazonaws.com/homepage-samples/96/139.jpg" ]
                        []
                    ]
                ]
            , div
                [ class "media-content" ]
                [ div
                    [ class "content" ]
                    [ p
                        []
                        [ h1
                            []
                            [ text "Elixir Lang" ]
                        , strong
                            []
                            [ text "Infinite loop when compiling eex template" ]
                        , small
                            []
                            [ text " #6607" ]
                        , br [] []
                        , strong
                            []
                            [ text "$225" ]
                        ]
                    ]
                , nav
                    [ class "level is-mobile" ]
                    [ div
                        [ class "level-left" ]
                        [ a
                            [ class "level-item" ]
                            [ span
                                [ class "icon is-medium" ]
                                [ i
                                    [ class "fa fa-gift" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


deleteCampaign : Model -> Cmd Msg
deleteCampaign model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        rewardUrl =
            model.apiUrl ++ "/campaigns/" ++ selectedCampaign.id

        data =
            { id = selectedCampaign.id
            , currentFunding = selectedCampaign.currentFunding
            , longDescription = selectedCampaign.longDescription
            , shortDescription = selectedCampaign.shortDescription
            , fundingGoal = selectedCampaign.fundingGoal
            , fundingEndDate = selectedCampaign.fundingEndDate
            , userId = selectedCampaign.userId
            , githubRepoId = selectedCampaign.githubRepoId
            }
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteCampaign (Campaign.encode data)


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "help is-danger" ]


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
