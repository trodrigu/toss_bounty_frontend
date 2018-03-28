module Pages.Dash exposing (..)

import Data.AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default, defaultDate, encode, showDecoder)
import Data.Campaigns as Campaigns exposing (IncludedStuff(..))
import Data.Plan as Plan exposing (Plan, updateEncode)
import Data.Plans as Plans exposing (Plans)
import Data.Reward as Reward exposing (Reward, updateEncode)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Subscription as Subscription exposing (Subscription)
import Html exposing (..)
import Html.Attributes exposing (class, src, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode exposing (Value)
import List.Extra as ListExtra exposing (greedyGroupsOf)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router exposing (href)
import SelectList exposing (Position(..), SelectList, fromLists, select, selected, singleton)
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


type alias SubscriptionWrapper =
    { subscription : Subscription
    , showConfirmation : Bool
    }


subscriptionWrapperDefault : SubscriptionWrapper
subscriptionWrapperDefault =
    { subscription = Subscription.default
    , showConfirmation = False
    }


type alias Model =
    { campaignId : Int
    , currentFunding : Float
    , fundingEndDate : DateTime
    , fundingGoal : Float
    , longDescription : String
    , yourCampaigns : SelectList Campaign
    , yourRepos : List IncludedStuff
    , yourSubscriptions : SelectList SubscriptionWrapper
    , yourSubscribedPlans : SelectList Plan
    , apiUrl : String
    , token : AuthToken
    , errors : List Error
    , isEditingYourCampaigns : Bool
    , showConfirmation : Bool
    , rewards : WebData Rewards
    , rewardsAsSelectList : SelectList Reward
    , plans : WebData Plan
    , plansAsSelectList : SelectList Plan
    , description : String
    , isEditingReward : Bool
    , isAddingReward : Bool
    , donationLevel : Float
    }


wrapSubscriptions : List Subscription -> List SubscriptionWrapper
wrapSubscriptions subscriptions =
    List.map (\subscription -> { subscription = subscription, showConfirmation = False }) subscriptions


init : Maybe String -> AuthToken -> List Campaign -> List IncludedStuff -> List Subscription -> List Plan -> Model
init apiUrl token yourCampaigns yourRepos yourSubscriptions yourSubscribedPlans =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url

        defaultYourCampaign =
            SelectList.singleton Campaign.default

        updatedYourCampaigns =
            defaultYourCampaign
                |> SelectList.append yourCampaigns

        defaultYourSubscription =
            SelectList.singleton { subscription = Subscription.default, showConfirmation = False }

        wrappedSubscriptions =
            wrapSubscriptions yourSubscriptions

        updatedYourSubscriptions =
            defaultYourSubscription
                |> SelectList.append wrappedSubscriptions

        defaultYourPlan =
            SelectList.singleton Plan.default

        updatedYourPlans =
            defaultYourPlan
                |> SelectList.append yourSubscribedPlans

        defaultRewardsAsSelectList =
            SelectList.singleton Reward.default
    in
    { campaignId = 0
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
    , yourCampaigns = updatedYourCampaigns
    , yourRepos = yourRepos
    , yourSubscriptions = updatedYourSubscriptions
    , yourSubscribedPlans = updatedYourPlans
    , apiUrl = url
    , token = token
    , errors = []
    , isEditingYourCampaigns = False
    , showConfirmation = False
    , rewards = NotAsked
    , rewardsAsSelectList = defaultRewardsAsSelectList
    , description = ""
    , plans = NotAsked
    , plansAsSelectList = SelectList.singleton Plan.default
    , isEditingReward = False
    , isAddingReward = False
    , donationLevel = 0.0
    }


type Msg
    = SelectYourCampaign Int
    | DeleteYourSubscription String
    | SaveUpdateCampaignForm
    | UpdateFundingGoalField String
    | UpdateLongDescriptionField String
    | HandlePutCampaign (WebData Campaign)
    | DeleteCampaign Int
    | HandleDeleteCampaign (WebData String)
    | HandleDeleteSubscription (WebData String)
    | ShowConfirmation String
    | HideConfirmation
    | HandleFetchRewards (WebData Rewards)
    | UpdateDescriptionField String
    | SelectReward Int
    | DeletePlan Int
    | HandlePutPlan (WebData Plan)
    | HandlePutReward (WebData Reward)
    | SaveUpdateForm
    | HandleDeletePlan (WebData String)
    | HandleDeleteReward (WebData String)
    | HandleFetchPlans (WebData Plans)
    | ShowRewardForm
    | SaveRewardForm
    | UpdateDonateLevelField String
    | HandleReward (WebData Reward)
    | HandlePlan (WebData Plan)
    | HandleDeleteRewards (WebData String)
    | HandleDeletePlans (WebData String)
    | HandleGetPlansForDeletion (WebData Plans)


type ExternalMsg
    = NoOp


validate : Model -> List Error
validate =
    Validate.all
        [ .longDescription >> ifBlank (LongDescription => "Summary can't be blank.")
        , .fundingGoal >> ifZero (FundingGoal => "Funding Goal can't be zero.")
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        HandleGetPlansForDeletion data ->
            let
                plansList =
                    case data of
                        Success plans ->
                            plans.plans

                        _ ->
                            Plans.default
                                |> .plans

                ( headPlan, tail ) =
                    case ListExtra.uncons plansList of
                        Just ( headPlan, tail ) ->
                            ( headPlan, tail )

                        Nothing ->
                            ( Plan.default, [] )

                plansAsSelectList =
                    SelectList.fromLists [] headPlan tail

                updatedModel =
                    { model | plansAsSelectList = plansAsSelectList }
            in
            ( updatedModel, deletePlans updatedModel ) => NoOp

        HandlePlan data ->
            case data of
                Success plan ->
                    let
                        currentSelectedPlan =
                            SelectList.selected model.plansAsSelectList

                        newPlan =
                            SelectList.singleton plan

                        befores =
                            SelectList.before model.plansAsSelectList

                        afters =
                            SelectList.after model.plansAsSelectList

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
                    { model
                        | plansAsSelectList = updatedPlans
                        , isAddingReward = False
                    }
                        => Cmd.none
                        => NoOp

                error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )
                        => NoOp

        HandleReward data ->
            case data of
                Success reward ->
                    let
                        currentSelectedReward =
                            SelectList.selected model.rewardsAsSelectList

                        newReward =
                            SelectList.singleton reward

                        befores =
                            SelectList.before model.rewardsAsSelectList

                        afters =
                            SelectList.after model.rewardsAsSelectList

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
                            { model | rewardsAsSelectList = updatedRewards, description = "", donationLevel = 0.0 }
                    in
                    updatedModel
                        => postPlan updatedModel
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

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

        ShowRewardForm ->
            ( { model | isAddingReward = True }, Cmd.none ) => NoOp

        HandleFetchPlans data ->
            let
                plansList =
                    case data of
                        Success plans ->
                            plans.plans

                        _ ->
                            Plans.default
                                |> .plans

                ( headPlan, tail ) =
                    case ListExtra.uncons plansList of
                        Just ( headPlan, tail ) ->
                            ( headPlan, tail )

                        Nothing ->
                            ( Plan.default, [] )

                plansAsSelectList =
                    SelectList.fromLists [] headPlan tail
            in
            ( { model | plansAsSelectList = plansAsSelectList }, Cmd.none ) => NoOp

        HandleDeleteReward data ->
            case data of
                Success _ ->
                    let
                        befores =
                            SelectList.before model.rewardsAsSelectList

                        afters =
                            SelectList.after model.rewardsAsSelectList

                        rewards =
                            befores ++ afters

                        ( headReward, tail ) =
                            case ListExtra.uncons rewards of
                                Just ( headReward, tail ) ->
                                    ( headReward, tail )

                                Nothing ->
                                    ( Reward.default, [] )

                        updatedRewards =
                            SelectList.fromLists [] headReward tail

                        updatedModel =
                            { model
                                | rewardsAsSelectList = updatedRewards
                                , description = ""
                                , isEditingReward = False
                            }
                    in
                    updatedModel
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
                    ( model, putPlan model ) => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        HandlePutReward data ->
            case data of
                Success reward ->
                    let
                        currentSelectedReward =
                            SelectList.selected model.rewardsAsSelectList

                        updatedReward =
                            { currentSelectedReward
                                | description = reward.description
                            }

                        befores =
                            SelectList.before model.rewardsAsSelectList

                        afters =
                            SelectList.after model.rewardsAsSelectList

                        updatedRewards =
                            SelectList.singleton updatedReward
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    ( { model | rewardsAsSelectList = updatedRewards, description = "", isEditingReward = False }, Cmd.none )
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        HandlePutPlan data ->
            case data of
                Success plan ->
                    let
                        currentSelectedPlan =
                            SelectList.selected model.plansAsSelectList

                        updatedPlan =
                            { currentSelectedPlan
                                | amount = plan.amount
                                , name = plan.name
                            }

                        befores =
                            SelectList.before model.plansAsSelectList

                        afters =
                            SelectList.after model.plansAsSelectList

                        updatedPlans =
                            SelectList.singleton updatedPlan
                                |> SelectList.prepend befores
                                |> SelectList.append afters

                        updatedModel =
                            { model | plansAsSelectList = updatedPlans }
                    in
                    ( updatedModel, putReward updatedModel )
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        HandleDeleteRewards data ->
            case data of
                Success _ ->
                    let
                        selectedReward =
                            SelectList.selected model.rewardsAsSelectList

                        befores =
                            SelectList.before model.rewardsAsSelectList

                        afters =
                            SelectList.after model.rewardsAsSelectList

                        rewards =
                            befores ++ afters

                        modelAndCmd =
                            case ListExtra.uncons rewards of
                                Just ( headReward, tail ) ->
                                    let
                                        updatedRewards =
                                            SelectList.fromLists [] headReward tail

                                        updatedModel =
                                            { model
                                                | rewardsAsSelectList = updatedRewards
                                            }
                                    in
                                    updatedModel
                                        => deleteRewards updatedModel
                                        => NoOp

                                Nothing ->
                                    let
                                        updatedRewards =
                                            SelectList.fromLists [] Reward.default []

                                        updatedModel =
                                            { model
                                                | rewardsAsSelectList = updatedRewards
                                            }
                                    in
                                    updatedModel
                                        => deleteCampaign updatedModel
                                        => NoOp
                    in
                    modelAndCmd

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        HandleDeletePlans data ->
            case data of
                Success _ ->
                    let
                        selectedPlan =
                            SelectList.selected model.plansAsSelectList

                        befores =
                            SelectList.before model.plansAsSelectList

                        afters =
                            SelectList.after model.plansAsSelectList

                        plans =
                            befores ++ afters

                        modelAndCmd =
                            case ListExtra.uncons plans of
                                Just ( headPlan, tail ) ->
                                    let
                                        updatedPlans =
                                            SelectList.fromLists [] headPlan tail

                                        updatedModel =
                                            { model
                                                | plansAsSelectList = updatedPlans
                                            }
                                    in
                                    updatedModel
                                        => deletePlans updatedModel
                                        => NoOp

                                Nothing ->
                                    let
                                        updatedPlans =
                                            SelectList.fromLists [] Plan.default []

                                        updatedModel =
                                            { model
                                                | plansAsSelectList = updatedPlans
                                            }
                                    in
                                    updatedModel
                                        => deleteCampaign updatedModel
                                        => NoOp
                    in
                    modelAndCmd

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        HandleDeletePlan data ->
            case data of
                Success _ ->
                    let
                        selectedPlan =
                            SelectList.selected model.plansAsSelectList

                        befores =
                            SelectList.before model.plansAsSelectList

                        afters =
                            SelectList.after model.plansAsSelectList

                        plans =
                            befores ++ afters

                        ( headPlan, tail ) =
                            case ListExtra.uncons plans of
                                Just ( headPlan, tail ) ->
                                    ( headPlan, tail )

                                Nothing ->
                                    ( Plan.default, [] )

                        updatedPlans =
                            SelectList.fromLists [] headPlan tail

                        updatedModel =
                            { model
                                | plansAsSelectList = updatedPlans
                            }
                    in
                    updatedModel
                        => deleteReward updatedModel
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        DeletePlan rewardId ->
            let
                _ =
                    Debug.log "rewardId in DeletePlan" rewardId

                updatedRewards =
                    SelectList.select (\u -> u.id == rewardId) model.rewardsAsSelectList

                updatedPlans =
                    SelectList.select (\u -> u.rewardId == rewardId) model.plansAsSelectList

                updatedModel =
                    { model
                        | plansAsSelectList = updatedPlans
                        , rewardsAsSelectList = updatedRewards
                    }
            in
            updatedModel
                => deletePlan updatedModel
                => NoOp

        SelectReward rewardId ->
            let
                updatedRewards =
                    SelectList.select (\u -> u.id == rewardId) model.rewardsAsSelectList

                selectedReward =
                    SelectList.selected updatedRewards

                updatedPlans =
                    SelectList.select (\u -> u.rewardId == rewardId) model.plansAsSelectList

                selectedPlan =
                    SelectList.selected updatedPlans
            in
            ( { model
                | rewardsAsSelectList = updatedRewards
                , isEditingReward = True
                , description = selectedReward.description
                , plansAsSelectList = updatedPlans
              }
            , Cmd.none
            )
                => NoOp

        UpdateDescriptionField str ->
            let
                reward =
                    SelectList.selected model.rewardsAsSelectList

                updatedReward =
                    { reward | description = str }

                befores =
                    SelectList.before model.rewardsAsSelectList

                afters =
                    SelectList.after model.rewardsAsSelectList

                updatedRewards =
                    SelectList.singleton updatedReward
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            ( { model | rewardsAsSelectList = updatedRewards, description = str }, Cmd.none ) => NoOp

        HandleFetchRewards rewards ->
            let
                rewardsList =
                    case rewards of
                        Success rewards ->
                            rewards.rewards

                        _ ->
                            Rewards.default
                                |> .rewards

                ( headReward, tail ) =
                    case ListExtra.uncons rewardsList of
                        Just ( headReward, tail ) ->
                            ( headReward, tail )

                        Nothing ->
                            ( Reward.default, [] )

                rewardsAsSelectList =
                    SelectList.fromLists [] headReward tail

                updatedModel =
                    { model | rewards = rewards, rewardsAsSelectList = rewardsAsSelectList }
            in
            ( updatedModel, getPlans updatedModel ) => NoOp

        HideConfirmation ->
            let
                selectedSubscription =
                    SelectList.selected model.yourSubscriptions

                updatedSelectedSubscription =
                    { selectedSubscription | showConfirmation = False }

                befores =
                    SelectList.before model.yourSubscriptions

                afters =
                    SelectList.after model.yourSubscriptions

                beforesAndAfters =
                    befores ++ afters

                defaultSelectedSubscription =
                    List.filter (\subscription -> not (hasSubscriptionId subscription)) beforesAndAfters
                        |> List.head
                        |> Maybe.withDefault subscriptionWrapperDefault

                subscriptionAsSelectList =
                    SelectList.singleton defaultSelectedSubscription

                updatedSubscriptions =
                    subscriptionAsSelectList
                        |> SelectList.append [ updatedSelectedSubscription ]
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            ( { model | yourSubscriptions = updatedSubscriptions }, Cmd.none ) => NoOp

        HandleDeleteSubscription data ->
            case data of
                Success _ ->
                    let
                        selectedSubscription =
                            SelectList.selected model.yourSubscriptions

                        befores =
                            SelectList.before model.yourSubscriptions

                        afters =
                            SelectList.after model.yourSubscriptions

                        beforesAndAfters =
                            befores ++ afters

                        defaultSelectedSubscription =
                            List.filter (\subscription -> not (hasSubscriptionId subscription)) beforesAndAfters
                                |> List.head
                                |> Maybe.withDefault subscriptionWrapperDefault

                        subscriptionAsSelectList =
                            SelectList.singleton defaultSelectedSubscription

                        updatedSubscriptions =
                            subscriptionAsSelectList
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    { model | yourSubscriptions = updatedSubscriptions }
                        => Cmd.none
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

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
                            List.filter (\campaign -> not (hasCampaignId campaign)) beforesAndAfters
                                |> List.head
                                |> Maybe.withDefault default

                        campaignAsSelectList =
                            SelectList.singleton defaultSelectedCampaign

                        updatedCampaigns =
                            campaignAsSelectList
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    { model | yourCampaigns = updatedCampaigns, longDescription = "", fundingGoal = 0.0, fundingEndDate = Campaign.defaultDate }
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

        DeleteCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\u -> u.id == campaignId) model.yourCampaigns

                updatedModel =
                    { model
                        | yourCampaigns = updatedCampaigns
                    }
            in
            ( updatedModel, getPlansForDeletion updatedModel ) => NoOp

        ShowConfirmation subscriptionId ->
            let
                updatedWrapperSubscriptions =
                    SelectList.select (\subscriptionWrapper -> subscriptionWrapper.subscription.id == subscriptionId) model.yourSubscriptions

                selectedWrapperSubscription =
                    SelectList.selected updatedWrapperSubscriptions

                updatedWrapperSubscription =
                    { selectedWrapperSubscription | showConfirmation = True }

                befores =
                    SelectList.before updatedWrapperSubscriptions

                afters =
                    SelectList.after updatedWrapperSubscriptions

                updatedSubscriptionWrappers =
                    SelectList.singleton updatedWrapperSubscription
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            ( { model | yourSubscriptions = updatedSubscriptionWrappers }, Cmd.none ) => NoOp

        DeleteYourSubscription subscriptionId ->
            let
                updatedSubscriptions =
                    SelectList.select (\subscriptionWrapper -> subscriptionWrapper.subscription.id == subscriptionId) model.yourSubscriptions

                updatedModel =
                    { model | yourSubscriptions = updatedSubscriptions }
            in
            ( updatedModel, deleteSubscription updatedModel ) => NoOp

        SelectYourCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\campaign -> campaign.id == campaignId) model.yourCampaigns

                selectedCampaign =
                    SelectList.selected updatedCampaigns

                updatedModel =
                    { model
                        | yourCampaigns = updatedCampaigns
                        , isEditingYourCampaigns = True
                        , longDescription = selectedCampaign.longDescription
                        , fundingGoal = selectedCampaign.fundingGoal
                    }
            in
            ( updatedModel
            , getRewards updatedModel
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

        HandlePutCampaign data ->
            case data of
                Success campaign ->
                    let
                        currentSelectedCampaign =
                            SelectList.selected model.yourCampaigns

                        updatedCampaign =
                            { currentSelectedCampaign
                                | longDescription = campaign.longDescription
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

        selectedCampaignId =
            selectedCampaign
                |> .id
                |> toString

        rewardUrl =
            model.apiUrl ++ "/campaigns/" ++ selectedCampaignId

        data =
            { longDescription = selectedCampaign.longDescription
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
        , yourRenderedSubscriptions model
        ]


renderCampaigns : Model -> List (Html Msg)
renderCampaigns model =
    let
        campaigns =
            model.yourCampaigns

        repos =
            model.yourRepos

        campaignsAsList =
            SelectList.toList campaigns

        persistedCampaigns =
            filterPersistedCampaigns campaignsAsList
    in
    List.map
        (\campaign ->
            showYourCampaign campaign repos
        )
        persistedCampaigns


renderSubscriptions : Model -> List (Html Msg)
renderSubscriptions model =
    let
        subscriptions =
            model.yourSubscriptions

        subscriptionsAsList =
            SelectList.toList subscriptions

        persistedSubscriptions =
            filterPersistedSubscriptions subscriptionsAsList
    in
    List.map
        (\subscription ->
            showYourSubscription subscription model
        )
        persistedSubscriptions


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


renderCampaignsWhenEditing : Model -> List (Html Msg)
renderCampaignsWhenEditing model =
    let
        campaigns =
            model.yourCampaigns

        repos =
            model.yourRepos

        campaignsAsList =
            SelectList.toList campaigns

        persistedCampaigns =
            filterPersistedCampaigns campaignsAsList
    in
    List.indexedMap
        (\index campaign ->
            case SelectList.selected model.yourCampaigns == campaign of
                True ->
                    updateCampaignForm model campaign repos

                False ->
                    showYourCampaign campaign repos
        )
        persistedCampaigns


campaignRows : Model -> List (Html Msg)
campaignRows model =
    let
        campaigns =
            model.yourCampaigns

        campaignsAsList =
            SelectList.toList campaigns

        persistedCampaigns =
            filterPersistedCampaigns campaignsAsList

        yourRenderedCampaigns =
            case model.isEditingYourCampaigns of
                True ->
                    renderCampaignsWhenEditing model

                False ->
                    renderCampaigns model

        yourRenderedCampaignsAsGroups =
            ListExtra.greedyGroupsOf 2 yourRenderedCampaigns
    in
    List.map (\campaign -> columnsWrapper campaign) yourRenderedCampaignsAsGroups


subscriptionRows : Model -> List (Html Msg)
subscriptionRows model =
    let
        subscriptions =
            model.yourSubscriptions

        subscriptionsAsList =
            SelectList.toList subscriptions

        yourRenderedSubscriptions =
            renderSubscriptions model

        yourRenderedSubscriptionsAsGroups =
            ListExtra.greedyGroupsOf 2 yourRenderedSubscriptions
    in
    List.map (\subscription -> columnsWrapper subscription) yourRenderedSubscriptionsAsGroups


yourRenderedSubscriptions : Model -> Html Msg
yourRenderedSubscriptions model =
    let
        subscriptions =
            model.yourSubscriptions

        subscriptionsAsList =
            SelectList.toList subscriptions

        persistedSubscriptions =
            filterPersistedSubscriptions subscriptionsAsList
    in
    section [ class "section" ]
        [ div [ class "container" ]
            ([ h1 [ class "title" ] [ text "Your Subscriptions" ]
             ]
                ++ subscriptionRows model
            )
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
    case List.length persistedCampaigns > 0 of
        True ->
            section [ class "section" ]
                [ div [ class "container" ]
                    ([ h1 [ class "title" ] [ text "Your Campaigns" ]
                     ]
                        ++ campaignRows model
                    )
                ]

        False ->
            section [ class "section" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ] [ text "Your Campaigns" ]
                    , a [ class "link", Router.href Router.CreateUserRoleRoute ]
                        [ text "Start a campaign!" ]
                    ]
                ]


updateCampaignForm : Model -> Campaign -> List IncludedStuff -> Html Msg
updateCampaignForm model campaign included =
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

        _ =
            Debug.log "model in updateCampaignForm" model

        displayRewards =
            if model.isEditingReward then
                displayUpdateRewards model |> SelectList.toList
            else
                displayAllRewards model
    in
    if model.isAddingReward then
        div [ class "card" ]
            [ div [ class "card-content" ]
                ([ displayCampaignUpdateFormHeader repoForCampaign
                 , viewErrors model.errors
                 , displayUpdateLongDescription model
                 , displayUpdateFundingGoal model
                 ]
                    ++ displayRewards
                    ++ [ displayUpdateButton ]
                    ++ [ a [ class "link", onClick ShowRewardForm ] [ text "Add Reward" ] ]
                    ++ [ createRewardForm model ]
                )
            ]
    else
        div [ class "card" ]
            [ div [ class "card-content" ]
                ([ displayCampaignUpdateFormHeader repoForCampaign
                 , viewErrors model.errors
                 , displayUpdateLongDescription model
                 , displayUpdateFundingGoal model
                 ]
                    ++ displayRewards
                    ++ [ displayUpdateButton ]
                    ++ [ a [ class "link", onClick ShowRewardForm ] [ text "Add Reward" ] ]
                )
            ]


filterPersistedRewards : List Reward -> List Reward
filterPersistedRewards rewardList =
    List.filter rewardHasId rewardList


planHasId : Plan -> Bool
planHasId plan =
    not (plan.id == "")


rewardHasId : Reward -> Bool
rewardHasId reward =
    not (reward.id == 0)


renderUpdateOrShow : Position -> Reward -> Html Msg
renderUpdateOrShow position reward =
    if position == SelectList.Selected then
        div [ class "card" ]
            [ div [ class "card-content" ]
                [ div [ class "field" ]
                    [ label [ class "label" ]
                        [ text "Description of reward" ]
                    , p [ class "control" ]
                        [ input
                            [ class "input"
                            , onInput UpdateDescriptionField
                            , value reward.description
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
    else if reward.id == 0 then
        div [] []
    else
        div [ class "card" ]
            [ div [ class "card-header" ]
                [ a [ class "card-header-icon", onClick (SelectReward reward.id) ]
                    [ span [] [ text "edit" ] ]
                , a [ class "card-header-icon", onClick (DeletePlan reward.id) ]
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


displayUpdateRewards : Model -> SelectList (Html Msg)
displayUpdateRewards model =
    model.rewardsAsSelectList
        |> SelectList.mapBy renderUpdateOrShow


showReward : Reward -> Html Msg
showReward reward =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ a [ class "card-header-icon", onClick (SelectReward reward.id) ]
                [ span [] [ text "edit" ] ]
            , a [ class "card-header-icon", onClick (DeletePlan reward.id) ]
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


hasRewardId : Reward -> Bool
hasRewardId reward =
    not (reward.id == 0)


displayAllRewards : Model -> List (Html Msg)
displayAllRewards model =
    let
        rewards =
            model.rewardsAsSelectList
                |> SelectList.toList
                |> List.filter hasRewardId
    in
    List.map (\reward -> showReward reward) rewards


updateRewardForm : Reward -> List (Html Msg)
updateRewardForm reward =
    [ updateRewardFormDonationLevel reward
    , updateRewardFormDescription reward
    ]


updateRewardFormDonationLevel : Reward -> Html Msg
updateRewardFormDonationLevel reward =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Donation Level" ]
        , p [ class "control" ]
            [ text (toString reward.donationLevel) ]
        ]


updateRewardFormDescription : Reward -> Html Msg
updateRewardFormDescription reward =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Description of reward" ]
        , p [ class "control" ]
            [ input
                [ class "input"
                , onInput UpdateDescriptionField
                , value reward.description
                ]
                []
            ]
        ]


filterPersistedCampaigns : List Campaign -> List Campaign
filterPersistedCampaigns campaignList =
    List.filter hasCampaignId campaignList


filterPersistedSubscriptions : List SubscriptionWrapper -> List SubscriptionWrapper
filterPersistedSubscriptions subscriptionList =
    List.filter hasSubscriptionId subscriptionList


hasCampaignId : Campaign -> Bool
hasCampaignId campaign =
    not (campaign.id == 0)


hasSubscriptionId : SubscriptionWrapper -> Bool
hasSubscriptionId subscriptionWrapper =
    not (subscriptionWrapper.subscription.id == "0")


campaignsYouContributedTo : List Campaign -> List (Html Msg)
campaignsYouContributedTo campaigns =
    List.map
        (\campaign ->
            showCampaignYouContributedTo campaign
        )
        campaigns


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
        [ displayCampaignFormHeader campaign repoForCampaign
        , displayCampaignFormContent campaign
        ]


showYourSubscription : SubscriptionWrapper -> Model -> Html Msg
showYourSubscription subscriptionWrapper model =
    let
        subscription =
            subscriptionWrapper.subscription

        plans =
            model.yourSubscribedPlans
                |> SelectList.toList

        correspondingPlan =
            List.filter (\r -> r.id == subscription.planId) plans
                |> List.head
                |> Maybe.withDefault Plan.default
    in
    div [ class "card" ]
        [ displaySubscriptionFormHeader correspondingPlan subscriptionWrapper
        , displaySubscriptionFormContent correspondingPlan
        ]


displayUpdateButton : Html Msg
displayUpdateButton =
    div [ class "field is-grouped" ]
        [ p [ class "control" ]
            [ button [ class "button is-primary", onClick SaveUpdateCampaignForm ]
                [ text "Update Campaign" ]
            ]
        ]


displayUpdateLongDescription : Model -> Html Msg
displayUpdateLongDescription model =
    div [ class "field" ]
        [ label [ class "label" ]
            [ text "Summary" ]
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


displayCampaignUpdateFormHeader : IncludedStuff -> Html Msg
displayCampaignUpdateFormHeader included =
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


displayCampaignFormHeader : Campaign -> IncludedStuff -> Html Msg
displayCampaignFormHeader campaign included =
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
        , a [ class "card-header-icon", onClick (SelectYourCampaign campaign.id) ]
            [ span [] [ text "edit" ] ]
        , a [ class "card-header-icon", onClick (DeleteCampaign campaign.id) ]
            [ span [] [ text "delete" ] ]
        ]


displaySubscriptionFormHeader : Plan -> SubscriptionWrapper -> Html Msg
displaySubscriptionFormHeader plan subscriptionWrapper =
    let
        subscription =
            subscriptionWrapper.subscription
    in
    case subscriptionWrapper.showConfirmation of
        True ->
            div [ class "card-header" ]
                [ p [ class "card-header-title" ]
                    [ text ("$ " ++ toString plan.amount) ]
                , label [ class "card-header-icon" ]
                    [ span [] [ text "Are you sure?" ] ]
                , a [ class "card-header-icon", onClick (DeleteYourSubscription subscription.id) ]
                    [ span [] [ text "Yes" ] ]
                , a [ class "card-header-icon", onClick HideConfirmation ]
                    [ span [] [ text "No" ] ]
                ]

        False ->
            div [ class "card-header" ]
                [ p [ class "card-header-title" ]
                    [ text ("$ " ++ toString plan.amount) ]
                , a [ class "card-header-icon", onClick (ShowConfirmation subscription.id) ]
                    [ span [] [ text "delete" ] ]
                ]


displaySubscriptionFormContent : Plan -> Html Msg
displaySubscriptionFormContent plan =
    div [ class "card-content" ]
        [ label [ class "label" ]
            [ text "Name" ]
        , p [ class "control" ]
            [ text (toString plan.name) ]
        ]


displayCampaignFormContent : Campaign -> Html Msg
displayCampaignFormContent campaign =
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
            , p [ class "control" ]
                [ text (toString campaign.fundingGoal) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding End Date" ]
            , p [ class "control" ]
                [ text (formatDateTime campaign) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Funding Progress" ]
            , progress [ class "progress", value (toString campaign.currentFunding), Html.Attributes.max (toString campaign.fundingGoal) ] [ text (toString campaign.currentFunding) ]
            ]
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


createRewardForm : Model -> Html Msg
createRewardForm model =
    div []
        [ viewErrors model.errors
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Donation Amount" ]
            , p [ class "control has-icons-left" ]
                [ input
                    [ class "input"
                    , Html.Attributes.type_ "number"
                    , onInput UpdateDonateLevelField
                    , value (toString model.donationLevel)
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


deleteCampaign : Model -> Cmd Msg
deleteCampaign model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .id
                |> toString

        rewardUrl =
            model.apiUrl ++ "/campaigns/" ++ selectedCampaignId

        data =
            { id = selectedCampaign.id
            , currentFunding = selectedCampaign.currentFunding
            , longDescription = selectedCampaign.longDescription
            , fundingGoal = selectedCampaign.fundingGoal
            , fundingEndDate = selectedCampaign.fundingEndDate
            , userId = selectedCampaign.userId
            , githubRepoId = selectedCampaign.githubRepoId
            }
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteCampaign (Campaign.encode data)


deleteSubscription : Model -> Cmd Msg
deleteSubscription model =
    let
        selectedSubscriptionWrapper =
            SelectList.selected model.yourSubscriptions

        subscriptionUrl =
            model.apiUrl ++ "/subscriptions/" ++ selectedSubscriptionWrapper.subscription.id

        data =
            { id = selectedSubscriptionWrapper.subscription.id
            , uuid = selectedSubscriptionWrapper.subscription.uuid
            , planId = selectedSubscriptionWrapper.subscription.planId
            }
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) subscriptionUrl HandleDeleteSubscription Subscription.deleteEncode


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


getRewards : Model -> Cmd Msg
getRewards model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .id
                |> toString

        updatedUrl =
            model.apiUrl ++ "/rewards/?campaign_id=" ++ (selectedCampaign.id |> toString)
    in
    RemoteData.Http.getWithConfig (Auth.config model.token) updatedUrl HandleFetchRewards Rewards.decoder


deletePlan : Model -> Cmd Msg
deletePlan model =
    let
        selectedPlan =
            SelectList.selected model.plansAsSelectList

        planUrl =
            model.apiUrl ++ "/plans/" ++ selectedPlan.id

        selectedReward =
            model.rewardsAsSelectList
                |> SelectList.selected
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) planUrl HandleDeletePlan (Encode.object [])


deleteReward : Model -> Cmd Msg
deleteReward model =
    let
        selectedReward =
            SelectList.selected model.rewardsAsSelectList

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ toString selectedReward.id
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteReward (Encode.object [])


putReward : Model -> Cmd Msg
putReward model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedReward =
            SelectList.selected model.rewardsAsSelectList

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ toString selectedReward.id

        data =
            { description = model.description
            , campaignId = selectedCampaign.id
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) rewardUrl HandlePutReward Reward.showDecoder (Reward.updateEncode data)


putPlan : Model -> Cmd Msg
putPlan model =
    let
        selectedReward =
            SelectList.selected model.rewardsAsSelectList

        selectedPlan =
            SelectList.selected model.plansAsSelectList

        planUrl =
            model.apiUrl ++ "/plans/" ++ selectedPlan.id

        data =
            { interval = "month"
            , name = selectedReward.description
            , currency = "usd"
            , rewardId = selectedReward.id
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) planUrl HandlePutPlan Plan.showDecoder (Plan.updateEncode data)


getPlans : Model -> Cmd Msg
getPlans model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .id
                |> toString

        apiUrl =
            model.apiUrl

        token =
            model.token

        reposUrl =
            apiUrl ++ "/plans/" ++ "?campaign_id=" ++ selectedCampaignId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleFetchPlans Plans.decoder


postReward : Model -> Cmd Msg
postReward model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        rewardUrl =
            model.apiUrl ++ "/rewards"

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = selectedCampaign.id
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) rewardUrl HandleReward Reward.showDecoder (Reward.encode data)


postPlan : Model -> Cmd Msg
postPlan model =
    let
        planUrl =
            model.apiUrl ++ "/plans"

        selectedReward =
            model.rewardsAsSelectList
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


deletePlans : Model -> Cmd Msg
deletePlans model =
    let
        plan =
            model.plansAsSelectList
                |> SelectList.selected

        planUrl =
            model.apiUrl ++ "/plans/" ++ plan.id

        _ =
            Debug.log "planUrl" planUrl
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) planUrl HandleDeletePlans (Encode.object [])


deleteRewards : Model -> Cmd Msg
deleteRewards model =
    let
        selectedReward =
            model.rewardsAsSelectList
                |> SelectList.selected

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ toString selectedReward.id
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteRewards (Encode.object [])


getPlansForDeletion : Model -> Cmd Msg
getPlansForDeletion model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .id
                |> toString

        apiUrl =
            model.apiUrl

        token =
            model.token

        reposUrl =
            apiUrl ++ "/plans/" ++ "?campaign_id=" ++ selectedCampaignId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleGetPlansForDeletion Plans.decoder
