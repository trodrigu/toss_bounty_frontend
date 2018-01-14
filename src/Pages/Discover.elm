module Pages.Discover exposing (..)

import CreditCard
import CreditCard.Config
import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, defaultCampaign, defaultDate)
import Data.Campaigns as Campaigns exposing (Campaigns)
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onInput)
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
    }


type Msg
    = UpdateCardData Model
    | SelectYourCampaign String
    | HandleFetchAllCampaigns (WebData Campaigns)
    | GetCampaigns


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

        SelectYourCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\campaign -> campaign.id == campaignId) model.campaigns

                selectedCampaign =
                    SelectList.selected updatedCampaigns
            in
            ( { model
                | campaigns = updatedCampaigns
                , isContributing = True
              }
            , Cmd.none
            )
                => NoOp


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
                        , CreditCard.card CreditCard.Config.defaultConfig model
                        , CreditCard.form (CreditCard.Config.defaultFormConfig UpdateCardData) model
                        ]
                    ]
                ]
            ]
        ]



-- section [ class "section" ]
--     [ div [ class "container" ]
--         [ div [ class "columns" ]
--             [ div [ class "column is-half is-offset-one-quarter" ]
--                 [ div [ class "card" ]
--                     [ div [ class "card-content" ]
--                         [ viewErrors model.errors
--                         , displayCardNumber model
--                         , displayExpiration model
--                         , displayCVC model
--                         ]
--                     ]
--                 ]
--             ]
--         ]
--     ]
-- displayCardNumber : Model -> Html Msg
-- displayCardNumber model =
--     div [ class "field" ]
--         [ label [ class "label" ]
--             [ text "Card Number" ]
--         , p [ class "control has-icons-left" ]
--             [ input
--                 [ class "input"
--                 , onInput UpdateCardNumber
--                 , Html.Attributes.value model.cardNumber
--                 ]
--                 []
--             , span [ class "icon is-left" ]
--                 [ i [ class "fas fa-credit-card" ] []
--                 ]
--             ]
--         ]
-- displayExpiration : Model -> Html Msg
-- displayExpiration model =
--     div [ class "field" ]
--         [ label [ class "label" ]
--             [ text "Expiration" ]
--         , p [ class "control" ]
--             [ input
--                 [ class "input"
--                 , onInput UpdateExpiration
--                 , Html.Attributes.value model.expiration
--                 ]
--                 []
--             ]
--         ]
-- displayCVC : Model -> Html Msg
-- displayCVC model =
--     div [ class "field" ]
--         [ label [ class "label" ]
--             [ text "CVC" ]
--         , p [ class "control" ]
--             [ input
--                 [ class "input"
--                 , onInput UpdateCVC
--                 , Html.Attributes.value model.cvc
--                 ]
--                 []
--             ]
--         ]


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
        , a [ class "", onClick (SelectYourCampaign campaign.id) ]
            [ span [] [ text "Contribute" ] ]
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
