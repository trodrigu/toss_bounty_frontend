module Pages.Contribute exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default)
import Data.Customer as Customer exposing (Customer)
import Data.Plan as Plan exposing (Plan, decoder)
import Data.Repo as Repo exposing (Repo)
import Data.Reward as Reward exposing (Reward, default)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Subscription as Subscription exposing (Subscription)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, src, style)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))


init : AuthToken -> Maybe String -> WebData Campaign -> WebData Repo -> WebData Rewards -> Model
init token apiUrl campaign repo rewards =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url

        defaultReward =
            SelectList.singleton Reward.default

        updatedRepo =
            case repo of
                Success repo ->
                    repo

                _ ->
                    Repo.default

        updatedCampaign =
            case campaign of
                Success campaign ->
                    campaign

                _ ->
                    Campaign.default

        rewardsAsSelectList =
            case rewards of
                Success updatedRewards ->
                    defaultReward
                        |> SelectList.append updatedRewards.rewards

                _ ->
                    defaultReward
    in
    { campaign = updatedCampaign
    , repo = updatedRepo
    , rewards = rewardsAsSelectList
    , plan = NotAsked
    , customer = NotAsked
    , subscription = NotAsked
    }


type alias Model =
    { campaign : Campaign
    , repo : Repo
    , rewards : SelectList Reward
    , plan : WebData Plan
    , customer : WebData Customer
    , subscription : WebData Subscription
    }


type Msg
    = Pay
    | HandleCreateCustomer (WebData Customer)
    | HandleCreateSubscription (WebData Subscription)
    | SelectReward
    | HandleFetchPlan (WebData Plan)


type ExternalMsg
    = NoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        HandleFetchPlan updatedPlan ->
            ( { model | plan = updatedPlan }, Cmd.none ) => NoOp

        Pay ->
            ( model, Cmd.none ) => NoOp

        HandleCreateCustomer customer ->
            ( model, Cmd.none ) => NoOp

        HandleCreateSubscription subscription ->
            ( model, Cmd.none ) => NoOp

        SelectReward ->
            ( model, Cmd.none ) => NoOp


view : Model -> Html Msg
view model =
    let
        rewards =
            model.rewards

        name =
            model.repo.name
    in
    section [ class "hero" ]
        [ div [ class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ h1 [ class "title" ] [ text name ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Funding Goal" ]
                        , p [ class "control has-icons-left" ]
                            [ text (toString model.campaign.fundingGoal)
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "A cool headline" ]
                        , p [ class "control" ]
                            [ text model.campaign.shortDescription
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Summary" ]
                        , p [ class "control" ]
                            [ text model.campaign.longDescription
                            ]
                        ]
                    , div [ class "field is-grouped" ]
                        [ p [ class "control" ]
                            [ button [ class "button is-primary", onClick Pay ]
                                [ text "Pay" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


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
                        , displayFormContentWithoutButton campaign
                        , displayStripePayment model
                        ]
                    ]
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
    div [ class "card-content" ]
        [ form
            [ action "/charge", id "payment-form", method "post" ]
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
        , a [ id "card-element", class "", onClick Pay ]
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
