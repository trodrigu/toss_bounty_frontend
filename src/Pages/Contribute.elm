module Pages.Contribute exposing (ExternalMsg(..), Model, Msg(..), displayStripePayment, filterPersistedRewards, getPlan, getReward, hasRewardId, init, makeOption, onChange, paymentForm, postCustomer, postSubscription, postToken, renderFundingGoal, renderPay, renderPaySetup, renderRedirect, update, view)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default)
import Data.Customer as Customer exposing (Customer, encode)
import Data.Plan as Plan exposing (Plan, showDecoder)
import Data.Repo as Repo exposing (Repo)
import Data.Reward as Reward exposing (Reward, default)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Stripe as Stripe exposing (Stripe, decoder)
import Data.Subscription as Subscription exposing (Subscription)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, src, style)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder, map)
import List.Extra exposing (getAt)
import Ports exposing (createStripeElement)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router exposing (Route, modifyUrl)
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)


init : AuthToken -> Maybe String -> WebData Campaign -> WebData Repo -> WebData Rewards -> User -> Model
init token apiUrl campaign repo rewards user =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just matchedUrl ->
                    matchedUrl

        defaultReward =
            SelectList.singleton Reward.default

        updatedRepo =
            case repo of
                Success matchedRepo ->
                    matchedRepo

                _ ->
                    Repo.default

        updatedCampaign =
            case campaign of
                Success matchedCampaign ->
                    matchedCampaign

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
    , paid = False
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
    , paid : Bool
    , stripe : Maybe Stripe
    , apiUrl : String
    , token : AuthToken
    , user : User
    , rewardOfInterest : WebData Reward
    }


type Msg
    = Pay
    | SelectReward (Maybe Int)
    | HandleFetchPlan (WebData Plan)
    | HandleCustomer (WebData Customer)
    | HandleStripe (WebData Stripe)
    | MakeSubscription
    | HandleSubscription (WebData Subscription)
    | HandleFetchReward (WebData Reward)
    | RedirectDiscover


type ExternalMsg
    = NoOp
    | Sync


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
    -- TODO: Actually hook up redirect
        RedirectDiscover ->
            (( model, Cmd.none ), NoOp)

        MakeSubscription ->
            (( model, postToken model ), NoOp)

        HandleFetchPlan updatedPlan ->
            let
                updatedModel =
                    { model | plan = updatedPlan }
            in
            (( updatedModel, Cmd.none ), NoOp)

        Pay ->
            let
                selectedReward =
                    SelectList.selected model.rewards
            in
            case selectedReward.id == 0 of
                True ->
                    (( { model | isPaying = False }, Cmd.none ), NoOp)

                False ->
                    (( { model | isPaying = True }, Ports.createStripeElement "placeholder" ), NoOp)

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
            (( updatedModel, postCustomer updatedModel ), NoOp)

        HandleCustomer data ->
            let
                updatedModel =
                    { model
                        | customer = data
                    }
            in
            (( updatedModel, postSubscription updatedModel ), NoOp)

        HandleSubscription data ->
            let
                updatedModel =
                    { model | subscription = data }
            in
            (( updatedModel, Cmd.none ), Sync)

        SelectReward Nothing ->
            (( model, Cmd.none ), NoOp)

        HandleFetchReward data ->
            let
                updatedModel =
                    { model | rewardOfInterest = data }
            in
            (( updatedModel, getPlan updatedModel ), NoOp)

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
            (( { model | rewards = updatedRewards }, getReward model foundReward.id ), NoOp)


view : Model -> Html Msg
view model =
    let
        rewards =
            model.rewards
                |> SelectList.toList

        name =
            model.repo.name

        subscriptionResult =
            case model.subscription of
                Success subscription ->
                    let
                        selectedReward =
                            SelectList.selected model.rewards

                        description =
                            selectedReward.description

                        fullMessage =
                            "You have just subscribed to " ++ description ++ "!"
                    in
                    div [ class "has-text-centered notification is-success" ]
                        [ text fullMessage
                        ]

                Failure error ->
                    let
                        fullMessage =
                            "The interwebs had an accident :("
                    in
                    div [ class "has-text-centered notification is-danger" ]
                        [ text fullMessage
                        ]

                NotAsked ->
                    div [] []

                Loading ->
                    div [] []
    in
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ subscriptionResult
                , div [ class "columns" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ h1 [ class "title" ] [ text name ]
                        , div [ class "field" ]
                            [ label [ class "label" ]
                                [ text "Funding Goal" ]
                            , renderFundingGoal model
                            ]
                        , div [ class "field" ]
                            [ label [ class "label" ]
                                [ text "Summary" ]
                            , p [ class "control" ]
                                [ text model.campaign.longDescription
                                ]
                            ]
                        , renderPaySetup model rewards
                        , renderPay model
                        , renderRedirect model
                        ]
                    ]
                , paymentForm model
                ]
            ]
        ]


renderFundingGoal : Model -> Html Msg
renderFundingGoal model =
    p [ class "control has-icons-left" ]
        [ text ("$" ++ String.fromFloat model.campaign.fundingGoal ++ "/month")
        ]


renderRedirect : Model -> Html Msg
renderRedirect model =
    case model.subscription of
        Success subscription ->
            div []
                [ a [ onClick RedirectDiscover ] [ text "Go Back" ] ]

        Failure error ->
            div [] []

        NotAsked ->
            div [] []

        Loading ->
            div [] []


filterPersistedRewards : List Reward -> List Reward
filterPersistedRewards rewardList =
    List.filter hasRewardId rewardList


hasRewardId : Reward -> Bool
hasRewardId reward =
    not (reward.id == 0)


renderPaySetup : Model -> List Reward -> Html Msg
renderPaySetup model rewards =
    case model.paid of
        True ->
            div [] []

        False ->
            div [ class "field" ]
                [ label [ class "label" ] [ text "Pick your level" ]
                , div [ class "control" ]
                    [ div [ class "select" ]
                        [ Html.select [ onChange ] (List.map (\el -> makeOption el) rewards)
                        ]
                    ]
                ]


makeOption : Reward -> Html Msg
makeOption reward =
    let
        amountAndDescription =
            if reward.id == 0 then
                reward.description

            else
                let
                    description =
                        reward.description

                    amount =
                        reward.donationLevel
                            |> String.fromFloat
                in
                "$ " ++ amount ++ " | " ++ description
    in
    option [] [ text amountAndDescription ]

targetSelectedIndex : Decoder (Maybe Int)
targetSelectedIndex =
    Decode.at [ "target", "selectedIndex" ]
        (Decode.map
            (\int ->
                if int == -1 then
                    Nothing
                else
                    Just int
            )
            Decode.int
        )


onChange : Attribute Msg
onChange =
    on "change" (Decode.map SelectReward targetSelectedIndex)


renderPay : Model -> Html Msg
renderPay model =
    case model.isPaying of
        True ->
            div [] []

        False ->
            case model.subscription of
                NotAsked ->
                    div [ class "field is-grouped" ]
                        [ p [ class "control" ]
                            [ button [ class "button is-primary", onClick Pay ]
                                [ text "Pay" ]
                            ]
                        ]

                _ ->
                    div [] []


paymentForm : Model -> Html Msg
paymentForm model =
    let
        campaign =
            model.campaign

        styles =
            case model.isPaying of
                True ->
                    style "display" "block"

                False ->
                    style "display" "none"
    in
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div
                [ class "column is-half is-offset-one-quarter", styles ]
                [ displayStripePayment model
                ]
            ]
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
            , campaignId = model.campaign.id
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

        data =
            { customerId = customerFromModel.id
            , planId = planFromModel.id
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) subscriptionUrl HandleSubscription Subscription.showDecoder (Subscription.encode data)


getPlan : Model -> Cmd Msg
getPlan model =
    let
        apiUrl =
            model.apiUrl

        token =
            model.token

        planUrl =
            apiUrl ++ "/plans/" ++ String.fromInt planId

        rewardOfInterest =
            case model.rewardOfInterest of
                Success reward ->
                    reward

                _ ->
                    Reward.default

        planId =
            rewardOfInterest.planId
    in
    RemoteData.Http.getWithConfig (Auth.config token) planUrl HandleFetchPlan Plan.showDecoder


getReward : Model -> Int -> Cmd Msg
getReward model rewardId =
    let
        apiUrl =
            model.apiUrl

        token =
            model.token

        updatedUrl =
            apiUrl ++ "/rewards/" ++ String.fromInt rewardId
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchReward Reward.showDecoder
