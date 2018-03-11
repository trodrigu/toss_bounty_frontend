module Pages.Discover exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default, defaultDate)
import Data.Campaigns as Campaigns exposing (Campaigns, IncludedStuff(..), includedRepoDefault)
import Data.Repo as Repo exposing (Repo)
import Data.Rewards as Rewards exposing (Rewards, decoder)
import Data.User as User exposing (User, decoder)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, src, style)
import Html.Events exposing (onClick, onInput)
import List.Extra as ListExtra exposing (greedyGroupsOf)
import Ports exposing (createStripeElement)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router exposing (Route(..))
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))


init : AuthToken -> Maybe String -> WebData Campaigns -> Model
init token apiUrl campaigns =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { campaigns = campaigns
    , token = token
    , apiUrl = url
    , showForm = False
    , user = NotAsked
    }


type alias Model =
    { campaigns : WebData Campaigns
    , token : AuthToken
    , apiUrl : String
    , showForm : Bool
    , user : WebData User
    }


type Msg
    = SelectYourCampaign String
    | HandleFetchAllCampaigns (WebData Campaigns)
    | GetCampaigns
    | HandleFetchUser (WebData User)


type ExternalMsg
    = NoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        HandleFetchUser updatedUser ->
            ( { model | user = updatedUser }, Cmd.none ) => NoOp

        GetCampaigns ->
            ( { model | campaigns = Loading }, fetchAllCampaigns model.apiUrl model.token ) => NoOp

        HandleFetchAllCampaigns data ->
            ( { model | campaigns = data }, Cmd.none ) => NoOp

        SelectYourCampaign campaignId ->
            let
                campaignIdAsInt =
                    case String.toInt campaignId of
                        Ok id ->
                            id

                        Err message ->
                            0
            in
            ( model, Router.modifyUrl (ContributeRoute campaignIdAsInt) ) => NoOp


view : Model -> Html Msg
view model =
    let
        campaigns =
            model.campaigns

        allCampaigns =
            model.campaigns
    in
    case model.campaigns of
        NotAsked ->
            div [ class "pageloader is-active" ] [ span [ class "title" ] [ text "Loading..." ] ]

        Loading ->
            div [ class "pageloader is-active" ] [ span [ class "title" ] [ text "Loading..." ] ]

        Failure error ->
            div [] [ text ("Failed" ++ toString error) ]

        Success campaigns ->
            let
                renderedCampaigns =
                    List.map (\campaign -> showYourCampaign campaign campaigns.included) campaigns.campaigns

                campaignsGrouped =
                    ListExtra.greedyGroupsOf 2 renderedCampaigns

                campaignsWithColumnsWrapper =
                    List.map (\campaign -> columnsWrapper campaign) campaignsGrouped
            in
            section [ class "section" ]
                [ div [ class "container" ]
                    ([ h1 [ class "title" ] [ text "Discover Campaigns" ]
                     ]
                        ++ campaignsWithColumnsWrapper
                    )
                ]


showYourCampaign : Campaign -> List IncludedStuff -> Html Msg
showYourCampaign campaign included =
    let
        repoForCampaign =
            List.filter
                (\included ->
                    case included of
                        IncludedGithub includedRepo ->
                            campaign.githubRepoId == includedRepo.id

                        _ ->
                            False
                )
                included
                |> List.head
                |> Maybe.withDefault Campaigns.includedRepoDefault
    in
    div [ class "card" ]
        [ displayCampaignFormHeader repoForCampaign
        , displayFormContent campaign
        ]


displayCampaignFormHeader : IncludedStuff -> Html Msg
displayCampaignFormHeader included =
    let
        repo =
            case included of
                IncludedGithub includedRepo ->
                    includedRepo

                IncludedStripe _ ->
                    { id = ""
                    , name = ""
                    , image = ""
                    , bountifulScore = 0
                    , owner = ""
                    }
    in
    div [ class "card-header" ]
        [ p [ class "card-header-title" ] [ text repo.name ]
        ]


displayStripePayment : Model -> Html Msg
displayStripePayment model =
    let
        formStyle =
            if model.showForm then
                ( "display", "block" )
            else
                ( "display", "none" )
    in
    div [ class "card-content" ]
        [ form
            [ action "/charge", id "payment-form", method "post", style [ formStyle ] ]
            [ div [ class "form-row field" ]
                [ label [ class "label" ]
                    [ text "Credit or debit card    " ]
                , div [ id "card-element" ]
                    [ text "    " ]
                , div [ id "card-errors" ]
                    []
                ]
            , button [ class "button is-success" ]
                [ text "Submit Payment" ]
            ]
        ]


displayFormContent : Campaign -> Html Msg
displayFormContent campaign =
    div [ class "card-content" ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Summary" ]
            , p [ class "control" ]
                [ text (toString campaign.longDescription) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding Goal" ]
            , p []
                [ text (toString campaign.fundingGoal) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding End Date" ]
            , p []
                [ text (formatDateTime campaign) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding Progress" ]
            , progress [ class "progress", Html.Attributes.value (toString campaign.currentFunding), Html.Attributes.max (toString campaign.fundingGoal) ] [ text (toString campaign.currentFunding) ]
            ]
        , a [ id "card-element", class "", onClick (SelectYourCampaign campaign.id) ]
            [ span [] [ text "Select Campaign" ] ]
        ]


displayFormContentWithoutButton : Campaign -> Html Msg
displayFormContentWithoutButton campaign =
    div [ class "card-content" ]
        [ label [ class "label" ]
            [ text "Summary" ]
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


fetchAllCampaigns : String -> AuthToken -> Cmd Msg
fetchAllCampaigns apiUrl token =
    let
        updatedUrl =
            apiUrl ++ "/campaigns"
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchAllCampaigns Campaigns.decoder


columnsWrapper : List (Html Msg) -> Html Msg
columnsWrapper campaigns =
    let
        ( firstCampaign, otherCampaigns ) =
            ListExtra.uncons campaigns
                |> Maybe.withDefault ( div [] [], [] )

        ( secondCampaign, emptyCampaigns ) =
            ListExtra.uncons otherCampaigns
                |> Maybe.withDefault ( div [] [], [] )
    in
    div [ class "columns" ]
        [ div [ class "column is-half" ]
            [ firstCampaign
            ]
        , div [ class "column is-half" ]
            [ secondCampaign
            ]
        ]
