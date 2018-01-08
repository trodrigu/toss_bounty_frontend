module Pages.Dash exposing (..)

import Data.AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, encode, showDecoder)
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onInput)
import Navigation
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
    , isEditing : Bool
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
            SelectList.singleton
                { id = ""
                , currentFunding = 0.0
                , shortDescription = ""
                , longDescription = ""
                , fundingGoal = 0.0
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
                , userId = ""
                }

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
    , isEditing = False
    }


type Msg
    = SelectCampaign String
    | SaveUpdateCampaignForm
    | UpdateFundingGoalField String
    | UpdateLongDescriptionField String
    | UpdateShortDescriptionField String
    | HandlePutCampaign (WebData Campaign)


type ExternalMsg
    = NoOp


validate : Model -> List Error
validate =
    Validate.all
        [ .shortDescription >> ifBlank (ShortDescription => "Short Description can't be blank.")
        , .longDescription >> ifBlank (LongDescription => "Long Description can't be blank.")
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
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

        SelectCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\campaign -> campaign.id == campaignId) model.yourCampaigns

                selectedCampaign =
                    SelectList.selected updatedCampaigns
            in
            ( { model | yourCampaigns = updatedCampaigns }, Cmd.none ) => NoOp

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
                        , isEditing = False
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
            model.apiUrl ++ "/rewards/" ++ selectedCampaign.id

        data =
            { longDescription = selectedCampaign.longDescription
            , shortDescription = selectedCampaign.shortDescription
            , fundingEndDate = selectedCampaign.fundingEndDate
            , fundingGoal = selectedCampaign.fundingGoal
            , userId = selectedCampaign.userId
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) rewardUrl HandlePutCampaign Campaign.showDecoder (Campaign.encode data)


view : Model -> Html Msg
view model =
    div
        []
        [ yourBounties model.yourCampaigns
        , campaignsYouContributedTo model.campaignsContributedTo
        ]


yourBounties : SelectList Campaign -> Html Msg
yourBounties campaigns =
    let
        campaignsAsList =
            SelectList.toList campaigns

        persistedCampaigns =
            filterPersistedCampaigns campaignsAsList
    in
    section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ h1
                []
                [ text "Your Campaigns" ]
            , div
                [ class "columns" ]
                [ div
                    [ class "column" ]
                    (List.map
                        (\campaign ->
                            showYourCampaign campaign
                        )
                        persistedCampaigns
                    )
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
    section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ h1
                []
                [ text "Campaigns You Contributed To" ]
            , div
                [ class "columns" ]
                [ div
                    [ class "column" ]
                    (List.map
                        (\campaign ->
                            showCampaignYouContributedTo campaign
                        )
                        campaigns
                    )
                ]
            ]
        ]


showYourCampaign : Campaign -> Html Msg
showYourCampaign campaign =
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
                            [ text campaign.shortDescription ]
                        , small
                            []
                            [ text campaign.longDescription ]
                        , br [] []
                        , strong
                            []
                            [ text (toString campaign.currentFunding) ]
                        , strong
                            []
                            [ text (toString campaign.fundingGoal) ]
                        , strong
                            []
                            [ text (toString campaign.fundingEndDate) ]
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
