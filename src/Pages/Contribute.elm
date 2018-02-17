module Pages.Contribute exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default)
import Data.Customer as Customer exposing (Customer, encode)
import Data.Plan as Plan exposing (Plan, decoder)
import Data.Repo as Repo exposing (Repo)
import Data.Reward as Reward exposing (Reward, default)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Stripe as Stripe exposing (Stripe, decoder)
import Data.Subscription as Subscription exposing (Subscription)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, src, style)
import Html.Events exposing (on, onClick, onInput)
import Html.Events.Extra exposing (targetSelectedIndex)
import Json.Decode
import List.Extra exposing (getAt)
import Ports exposing (createStripeElement)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))


init : AuthToken -> Maybe String -> WebData Campaign -> WebData Repo -> WebData Rewards -> User -> Model
init token apiUrl campaign repo rewards user =
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
    , isPaying = False
    , stripe = Nothing
    , apiUrl = url
    , token = token
    , user = user
    , rewardOfInterest = NotAsked
    }


type alias Model =
    { campaign : Campaign
    , repo : Repo
    , rewards : SelectList Reward
    , plan : WebData Plan
    , customer : WebData Customer
    , subscription : WebData Subscription
    , isPaying : Bool
    , stripe : Maybe Stripe
    , apiUrl : String
    , token : AuthToken
    , user : User
    , rewardOfInterest : WebData Reward
    }


type Msg
    = Pay
    | HandleCreateCustomer (WebData Customer)
    | HandleCreateSubscription (WebData Subscription)
    | SelectReward (Maybe Int)
    | HandleFetchPlan (WebData Plan)
    | HandleCustomer (WebData Customer)
    | HandleStripe (WebData Stripe)
    | MakeSubscription
    | HandleSubscription (WebData Subscription)
    | HandleFetchReward (WebData Reward)


type ExternalMsg
    = NoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        MakeSubscription ->
            ( model, postToken model ) => NoOp

        HandleFetchPlan updatedPlan ->
            let
                updatedModel =
                    { model | plan = updatedPlan }
            in
            ( updatedModel, Cmd.none ) => NoOp

        Pay ->
            ( { model | isPaying = True }, Ports.createStripeElement "placeholder" ) => NoOp

        HandleCreateCustomer customer ->
            ( model, Cmd.none ) => NoOp

        HandleCreateSubscription subscription ->
            ( model, Cmd.none ) => NoOp

        HandleStripe data ->
            let
                updatedStripe =
                    case data of
                        Success stripe ->
                            stripe

                        _ ->
                            Stripe.default

                updatedModel =
                    { model | stripe = Just updatedStripe }
            in
            ( updatedModel, postCustomer updatedModel ) => NoOp

        HandleCustomer data ->
            let
                updatedModel =
                    { model | customer = data }
            in
            ( updatedModel, postSubscription updatedModel ) => NoOp

        HandleSubscription data ->
            let
                updatedModel =
                    { model | subscription = data }
            in
            ( updatedModel, Cmd.none ) => NoOp

        SelectReward Nothing ->
            ( model, Cmd.none ) => NoOp

        HandleFetchReward data ->
            let
                updatedModel =
                    { model | rewardOfInterest = data }

                _ =
                    Debug.log "data" data
            in
            ( updatedModel, getPlan updatedModel ) => NoOp

        SelectReward (Just index) ->
            let
                rewards =
                    model.rewards
                        |> SelectList.toList

                splitList =
                    List.Extra.splitAt index rewards

                beforesList =
                    splitList
                        |> Tuple.first

                aftersList =
                    splitList
                        |> Tuple.second

                foundReward =
                    aftersList
                        |> List.head
                        |> Maybe.withDefault Reward.default

                aftersWithoutFoundReward =
                    aftersList
                        |> List.Extra.uncons
                        |> Maybe.withDefault ( Reward.default, [] )
                        |> Tuple.second

                updatedRewards =
                    SelectList.singleton foundReward
                        |> SelectList.prepend beforesList
                        |> SelectList.append aftersWithoutFoundReward
            in
            ( { model | rewards = updatedRewards }, getReward model foundReward.id ) => NoOp


view : Model -> Html Msg
view model =
    let
        rewards =
            model.rewards
                |> SelectList.toList

        name =
            model.repo.name
    in
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
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
                        , div [ class "feild" ]
                            [ label [ class "label" ] [ text "Pick your level" ]
                            , div [ class "control" ]
                                [ div [ class "select" ]
                                    [ Html.select [ onChange ] (List.map (\el -> makeOption el.description) rewards)
                                    ]
                                ]
                            ]
                        , renderPay model
                        ]
                    ]
                , paymentForm model
                ]
            ]
        ]


makeOption : String -> Html Msg
makeOption description =
    option [] [ text description ]


onChange : Attribute Msg
onChange =
    on "change" (Json.Decode.map SelectReward Html.Events.Extra.targetSelectedIndex)


renderPay : Model -> Html Msg
renderPay model =
    case model.isPaying of
        True ->
            div [] []

        False ->
            div [ class "field is-grouped" ]
                [ p [ class "control" ]
                    [ button [ class "button is-primary", onClick Pay ]
                        [ text "Pay" ]
                    ]
                ]


paymentForm : Model -> Html Msg
paymentForm model =
    let
        campaign =
            model.campaign

        styles =
            case model.isPaying of
                True ->
                    style [ ( "display", "block" ) ]

                False ->
                    style [ ( "display", "none" ) ]
    in
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div
                [ class "column is-half is-offset-one-quarter", styles ]
                [ displayStripePayment model
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
    form
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


postCustomer : Model -> Cmd Msg
postCustomer model =
    let
        customerUrl =
            model.apiUrl ++ "/customers"

        stripeFromModel =
            case model.stripe of
                Just stripe ->
                    stripe

                _ ->
                    Stripe.default

        data =
            { tokenId = stripeFromModel.id
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) customerUrl HandleCustomer Customer.decoder (Customer.encode data)


postToken : Model -> Cmd Msg
postToken model =
    let
        tokenUrl =
            model.apiUrl ++ "/tokens"

        stripeFromModel =
            case model.stripe of
                Just stripe ->
                    stripe

                _ ->
                    Stripe.default

        data =
            { uuid = stripeFromModel.uuid
            , userId = model.user.userId
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) tokenUrl HandleStripe Stripe.showDecoder (Stripe.encode data)


postSubscription : Model -> Cmd Msg
postSubscription model =
    let
        subscriptionUrl =
            model.apiUrl ++ "/subscriptions"

        customerFromModel =
            case model.customer of
                Success customer ->
                    customer

                _ ->
                    Customer.default

        planFromModel =
            case model.plan of
                Success plan ->
                    plan

                _ ->
                    Plan.default

        _ =
            Debug.log "planFromModel" planFromModel

        data =
            { customerId = customerFromModel.id
            , planId = planFromModel.id
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) subscriptionUrl HandleSubscription Stripe.decoder (Subscription.encode data)


getPlan : Model -> Cmd Msg
getPlan model =
    let
        _ =
            Debug.log "model.rewardOfInterest" model.rewardOfInterest
    in
    let
        apiUrl =
            model.apiUrl

        token =
            model.token

        reposUrl =
            apiUrl ++ "/plans/" ++ planId

        rewardOfInterest =
            case model.rewardOfInterest of
                Success reward ->
                    reward

                _ ->
                    Reward.default

        planId =
            rewardOfInterest.planId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleFetchPlan Plan.decoder


getReward : Model -> String -> Cmd Msg
getReward model rewardId =
    let
        apiUrl =
            model.apiUrl

        token =
            model.token

        updatedUrl =
            apiUrl ++ "/rewards/" ++ rewardId
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchReward Reward.showDecoder
