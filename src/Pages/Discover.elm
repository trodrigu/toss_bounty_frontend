module Pages.Discover exposing (..)

import CreditCard
import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, defaultCampaign, defaultDate)
import Data.Campaigns as Campaigns exposing (Campaigns)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, src, style)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Ports exposing (createStripeElement)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))


init : AuthToken -> Maybe String -> Model
init token apiUrl =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url

        defaultCampaignInDiscover =
            SelectList.singleton Campaign.defaultCampaign
    in
    { campaignsWebData = NotAsked
    , campaigns = defaultCampaignInDiscover
    , token = token
    , apiUrl = url
    , number = Nothing
    , name = Nothing
    , month = Nothing
    , year = Nothing
    , cvv = Nothing
    , state = CreditCard.initialState
    , isContributing = False
    , showForm = False
    }


type alias Model =
    { campaignsWebData : WebData Campaigns
    , campaigns : SelectList Campaign
    , token : AuthToken
    , apiUrl : String
    , number : Maybe String
    , name : Maybe String
    , month : Maybe String
    , year : Maybe String
    , cvv : Maybe String
    , state : CreditCard.State
    , isContributing : Bool
    , showForm : Bool
    }


type Msg
    = UpdateCardData Model
    | SelectYourCampaign String
    | HandleFetchAllCampaigns (WebData Campaigns)
    | GetCampaigns
    | Contribute


type ExternalMsg
    = NoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        GetCampaigns ->
            ( { model | campaignsWebData = Loading }, fetchAllCampaigns model.apiUrl model.token ) => NoOp

        HandleFetchAllCampaigns data ->
            case data of
                Success campaigns ->
                    let
                        updatedCampaigns =
                            model.campaigns
                                |> SelectList.append campaigns.campaigns
                    in
                    ( { model | campaignsWebData = data, campaigns = updatedCampaigns }, Cmd.none ) => NoOp

                _ ->
                    ( model, Cmd.none ) => NoOp

        UpdateCardData updatedModel ->
            ( updatedModel, Cmd.none ) => NoOp

        Contribute ->
            ( { model | showForm = True }, Ports.createStripeElement "#card-element" ) => NoOp

        SelectYourCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\campaign -> campaign.id == campaignId) model.campaigns

                updatedModel =
                    { model
                        | campaigns = updatedCampaigns
                        , isContributing = True
                    }
            in
            updatedModel => Cmd.none => NoOp


view : Model -> Html Msg
view model =
    let
        campaigns =
            model.campaigns

        allCampaigns =
            model.campaigns
                |> SelectList.toList

        persistedCampaigns =
            filterPersistedCampaigns allCampaigns
    in
    case model.campaignsWebData of
        NotAsked ->
            section [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-half is-offset-one-quarter" ]
                            [ button [ class "button is-primary", onClick GetCampaigns ]
                                [ text "Search Campaigns" ]
                            ]
                        ]
                    ]
                ]

        Loading ->
            div [ class "is-loading" ] []

        Failure error ->
            div [] [ text ("Failed" ++ toString error) ]

        Success campaigns ->
            case model.isContributing of
                True ->
                    div [] <|
                        List.map
                            (\campaign ->
                                case SelectList.selected model.campaigns == campaign of
                                    True ->
                                        div []
                                            [ paymentForm model campaign
                                            ]

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


filterPersistedCampaigns : List Campaign -> List Campaign
filterPersistedCampaigns campaignList =
    List.filter hasId campaignList


hasId : Campaign -> Bool
hasId campaign =
    not (campaign.id == "")


paymentForm : Model -> Campaign -> Html Msg
paymentForm model campaign =
    if model.showForm then
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
                            , displayFormContentWithoutButton campaign
                            , displayStripePayment model
                            ]
                        ]
                    ]
                ]
            ]
    else
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
                            , displayFormContentWithoutButton campaign
                            , displayStripePayment model
                            , div [ class "card-content" ]
                                [ button [ class "button", onClick Contribute ]
                                    [ text "Contribute" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]


displayCardNumber : Html Msg
displayCardNumber =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Card Number" ]
        , p [ class "control has-icons-left" ]
            [ input
                [ class "input"
                ]
                []
            , span [ class "icon is-left" ]
                [ i [ class "fas fa-credit-card" ] []
                ]
            ]
        ]


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


displayFormHeader : Campaign -> Html Msg
displayFormHeader campaign =
    div [ class "card-header" ]
        [ p [ class "card-header-title" ]
            [ text (toString campaign.shortDescription) ]
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
        , a [ id "card-element", class "", onClick (SelectYourCampaign campaign.id) ]
            [ span [] [ text "Select Campaign" ] ]
        ]


displayFormContentWithoutButton : Campaign -> Html Msg
displayFormContentWithoutButton campaign =
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


fetchAllCampaigns : String -> AuthToken -> Cmd Msg
fetchAllCampaigns apiUrl token =
    let
        updatedUrl =
            apiUrl ++ "/campaigns"
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchAllCampaigns Campaigns.decoder
