module Pages.Dash exposing (CampaignWrapper, Error, ExternalMsg(..), Field(..), Model, Msg(..), RewardWrapper, SubscriptionWrapper, campaignRows, campaignWrapperDefault, columnsWrapper, createRewardForm, deleteCampaign, deletePlan, deletePlans, deleteReward, deleteRewards, deleteSubscription, displayAllRewards, displayCampaignFormContent, displayCampaignFormHeader, displayCampaignUpdateFormHeader, displaySubscriptionFormContent, displaySubscriptionFormHeader, displayUpdateButton, displayUpdateFundingGoal, displayUpdateLongDescription, displayUpdateRewards, filterPersistedCampaigns, filterPersistedRewards, filterPersistedSubscriptions, getPlans, getPlansForDeletion, getRewards, hasCampaignId, hasRewardId, hasSubscriptionId, ifZero, init, planHasId, postPlan, postReward, putCampaign, putPlan, putReward, renderButtonCode, renderCampaigns, renderCampaignsWhenEditing, renderSubscriptions, renderUpdateOrShow, rewardHasId, rewardWrapperDefault, showCampaignFooter, showReward, showYourCampaign, showYourSubscription, subscriptionRows, subscriptionWrapperDefault, update, updateCampaignForm, updateRewardForm, updateRewardFormDescription, updateRewardFormDonationLevel, validate, view, viewErrors, wrapCampaign, wrapReward, wrapSubscriptions, yourBounties, yourRenderedSubscriptions)

import Data.AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default, encode, showDecoder)
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
import Validate exposing (ifBlank, Validator)


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


type alias CampaignWrapper =
    { campaign : Campaign
    , campaignConfirmation : Bool
    , showButtonCode : Bool
    }


type alias RewardWrapper =
    { reward : Reward
    , confirmation : Bool
    }


campaignWrapperDefault : CampaignWrapper
campaignWrapperDefault =
    { campaign = Campaign.default
    , campaignConfirmation = False
    , showButtonCode = False
    }


rewardWrapperDefault : RewardWrapper
rewardWrapperDefault =
    { reward = Reward.default
    , confirmation = False
    }


type alias Model =
    { campaignId : Int
    , currentFunding : Float
    , fundingGoal : Float
    , longDescription : String
    , yourCampaigns : SelectList CampaignWrapper
    , yourRepos : List IncludedStuff
    , yourSubscriptions : SelectList SubscriptionWrapper
    , yourSubscribedPlans : SelectList Plan
    , apiUrl : String
    , token : AuthToken
    , errors : List Error
    , isEditingYourCampaigns : Bool
    , showConfirmation : Bool
    , rewards : WebData Rewards
    , rewardsAsSelectList : SelectList RewardWrapper
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


wrapCampaign : Campaign -> CampaignWrapper
wrapCampaign campaign =
    { campaign = campaign
    , campaignConfirmation = False
    , showButtonCode = False
    }


wrapReward : Reward -> RewardWrapper
wrapReward reward =
    { reward = reward, confirmation = False }


init : Maybe String -> AuthToken -> List Campaign -> List IncludedStuff -> List Subscription -> List Plan -> Model
init apiUrl token yourCampaigns yourRepos yourSubscriptions yourSubscribedPlans =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just matchedUrl ->
                    matchedUrl

        yourCampaignsWrapped =
            List.map (\campaign -> wrapCampaign campaign) yourCampaigns

        defaultYourCampaign =
            SelectList.singleton
                { campaign = Campaign.default
                , campaignConfirmation = False
                , showButtonCode = True
                }

        updatedYourCampaigns =
            defaultYourCampaign
                |> SelectList.append yourCampaignsWrapped

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
            SelectList.singleton { reward = Reward.default, confirmation = False }
    in
    { campaignId = 0
    , currentFunding = 0.0
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
    | ShowCampaignConfirmation Int
    | HideCampaignConfirmation
    | ShowRewardConfirmation Int
    | HideRewardConfirmation
    | Cancel
    | UpdateNewDescriptionField String
    | ToggleButtonCode Int


type ExternalMsg
    = NoOp
    | MakeMainFetchCampaigns


validate : Model -> Validator (Field, String) Model
validate =
    Validate.all
        [ .longDescription >> ifBlank (LongDescription, "Summary can't be blank.")
        , .fundingGoal >> ifZero (FundingGoal, "Funding Goal can't be zero.")
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        ToggleButtonCode campaignId ->
            let
                withCampaignWrapperSelected =
                    SelectList.select (\c -> c.campaign.id == campaignId) model.yourCampaigns

                selectedCampaignWrapper =
                    SelectList.selected withCampaignWrapperSelected

                updatedCampaignWrapper =
                    { selectedCampaignWrapper | showButtonCode = not selectedCampaignWrapper.showButtonCode }

                _ =
                    Debug.log "model" model

                befores =
                    SelectList.before withCampaignWrapperSelected

                afters =
                    SelectList.after withCampaignWrapperSelected

                updatedCampaignWrappers =
                    SelectList.singleton updatedCampaignWrapper
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            (( { model | yourCampaigns = updatedCampaignWrappers }, Cmd.none ), NoOp)

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
                        Just ( matchedHeadPlan, matchedTail ) ->
                            ( matchedHeadPlan, matchedTail )

                        Nothing ->
                            ( Plan.default, [] )

                plansAsSelectList =
                    SelectList.fromLists [] headPlan tail

                updatedModel =
                    { model | plansAsSelectList = plansAsSelectList }

                cmd =
                    case ListExtra.uncons plansList of
                        Just ( headPlanForCmd, tailForcmd ) ->
                            deletePlans updatedModel

                        Nothing ->
                            deleteCampaign model
            in
            (( updatedModel, cmd ), NoOp)

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
                    (({ model
                        | plansAsSelectList = updatedPlans
                        , isAddingReward = False
                    }
                    , Cmd.none)
                    , MakeMainFetchCampaigns)

                error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    (( model, Cmd.none ), NoOp)
                        

        HandleReward data ->
            case data of
                Success reward ->
                    let
                        currentSelectedReward =
                            SelectList.selected model.rewardsAsSelectList

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
                            SelectList.singleton { reward = reward, confirmation = False }
                                |> SelectList.prepend befores
                                |> SelectList.append aftersWithCurrentSelectedReward

                        updatedModel =
                            { model | rewardsAsSelectList = updatedRewards, description = "", donationLevel = 0.0 }
                    in
                    ((updatedModel
                        , postPlan updatedModel)
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        SaveRewardForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    (( model, postReward model ), NoOp)

                errors ->
                    (({ model | errors = errors }
                        , Cmd.none)
                        , NoOp)

        UpdateDonateLevelField str ->
            let
                updatedDonateLevel =
                    case String.toFloat str of
                        Ok donationLevel ->
                            donationLevel

                        Err error ->
                            0.0
            in
            (( { model | donationLevel = updatedDonateLevel }, Cmd.none ), NoOp)

        ShowRewardForm ->
            (( { model | isAddingReward = True }, Cmd.none ), NoOp)

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
                        Just ( matchedHeadPlan, matchedTail ) ->
                            ( matchedHeadPlan, matchedTail )

                        Nothing ->
                            ( Plan.default, [] )

                plansAsSelectList =
                    SelectList.fromLists [] headPlan tail
            in
            (( { model | plansAsSelectList = plansAsSelectList }, Cmd.none ), NoOp)

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
                                Just ( matchedHeadReward, matchedTail ) ->
                                    ( matchedHeadReward, matchedTail )

                                Nothing ->
                                    ( { reward = Reward.default, confirmation = False }, [] )

                        updatedRewards =
                            SelectList.fromLists [] headReward tail

                        updatedModel =
                            { model
                                | rewardsAsSelectList = updatedRewards
                                , description = ""
                                , isEditingReward = False
                            }
                    in
                    ((updatedModel
                        , Cmd.none)
                        , MakeMainFetchCampaigns)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        SaveUpdateForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    (( model, putPlan model ), NoOp)

                errors ->
                    (({ model | errors = errors }
                        , Cmd.none)
                        , NoOp)

        HandlePutReward data ->
            case data of
                Success reward ->
                    let
                        currentSelectedRewardWrapper =
                            SelectList.selected model.rewardsAsSelectList

                        currentSelectedReward =
                            currentSelectedRewardWrapper.reward

                        updatedReward =
                            { currentSelectedReward
                                | description = reward.description
                            }

                        befores =
                            SelectList.before model.rewardsAsSelectList

                        afters =
                            SelectList.after model.rewardsAsSelectList

                        updatedRewards =
                            SelectList.singleton { reward = updatedReward, confirmation = False }
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    (( { model | rewardsAsSelectList = updatedRewards, description = "", isEditingReward = False }, Cmd.none )
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

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
                    (( updatedModel, putReward updatedModel )
                        , NoOp)

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

        HandleDeleteRewards data ->
            case data of
                Success _ ->
                    let
                        selectedRewardWrapper =
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
                                    ((updatedModel
                                        , deleteRewards updatedModel)
                                        , NoOp)

                                Nothing ->
                                    let
                                        updatedRewards =
                                            SelectList.fromLists [] { reward = Reward.default, confirmation = False } []

                                        updatedModel =
                                            { model
                                                | rewardsAsSelectList = updatedRewards
                                            }
                                    in
                                    ((updatedModel
                                        , deleteCampaign updatedModel)
                                        , NoOp)
                    in
                    modelAndCmd

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

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
                                    ((updatedModel
                                        , deletePlans updatedModel)
                                        , NoOp)

                                Nothing ->
                                    let
                                        updatedPlans =
                                            SelectList.fromLists [] Plan.default []

                                        updatedModel =
                                            { model
                                                | plansAsSelectList = updatedPlans
                                            }
                                    in
                                    ((updatedModel
                                        , deleteCampaign updatedModel)
                                        , NoOp)
                    in
                    modelAndCmd

                _ ->
                    (( model, Cmd.none )
                        , NoOp)

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
                                Just ( matchedHeadPlan, matchedTail ) ->
                                    ( matchedHeadPlan, matchedTail )

                                Nothing ->
                                    ( Plan.default, [] )

                        updatedPlans =
                            SelectList.fromLists [] headPlan tail

                        updatedModel =
                            { model
                                | plansAsSelectList = updatedPlans
                            }
                    in
                    ((updatedModel
                        , deleteReward updatedModel)
                        , NoOp)

                _ ->
                    (( model, Cmd.none ), NoOp)

        DeletePlan rewardId ->
            let
                updatedRewards =
                    SelectList.select (\u -> u.reward.id == rewardId) model.rewardsAsSelectList

                updatedPlans =
                    SelectList.select (\u -> u.rewardId == rewardId) model.plansAsSelectList

                updatedModel =
                    { model
                        | plansAsSelectList = updatedPlans
                        , rewardsAsSelectList = updatedRewards
                    }
            in
            ((updatedModel, deletePlan updatedModel), NoOp)

        SelectReward rewardId ->
            let
                updatedRewards =
                    SelectList.select (\u -> u.reward.id == rewardId) model.rewardsAsSelectList

                selectedRewardWrapper =
                    SelectList.selected updatedRewards

                selectedReward =
                    selectedRewardWrapper.reward

                updatedPlans =
                    SelectList.select (\u -> u.rewardId == rewardId) model.plansAsSelectList

                selectedPlan =
                    SelectList.selected updatedPlans
            in
            (( { model
                | rewardsAsSelectList = updatedRewards
                , isEditingReward = True
                , description = selectedReward.description
                , plansAsSelectList = updatedPlans
              }
            , Cmd.none
            )
            , NoOp)

        UpdateNewDescriptionField str ->
            (( { model | description = str }, Cmd.none ), NoOp)

        UpdateDescriptionField str ->
            let
                selectedRewardWrapper =
                    SelectList.selected model.rewardsAsSelectList

                selectedReward =
                    selectedRewardWrapper.reward

                updatedSelectedReward =
                    { selectedReward | description = str }

                updatedSelectedRewardWrapper =
                    { reward = updatedSelectedReward, confirmation = False }

                befores =
                    SelectList.before model.rewardsAsSelectList

                afters =
                    SelectList.after model.rewardsAsSelectList

                updatedRewardWrappers =
                    SelectList.singleton updatedSelectedRewardWrapper
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            (( { model | rewardsAsSelectList = updatedRewardWrappers }, Cmd.none ), NoOp)

        HandleFetchRewards rewards ->
            let
                rewardsList =
                    case rewards of
                        Success matchedRewards ->
                            matchedRewards.rewards

                        _ ->
                            Rewards.default
                                |> .rewards

                ( headReward, tail ) =
                    case ListExtra.uncons rewardsList of
                        Just ( matchedHeadReward, matchedTail ) ->
                            ( matchedHeadReward, matchedTail )

                        Nothing ->
                            ( Reward.default, [] )

                tailAsRewardWrappers =
                    List.map (\reward -> wrapReward reward) tail

                rewardsAsSelectList =
                    SelectList.fromLists [] { reward = headReward, confirmation = False } tailAsRewardWrappers

                updatedModel =
                    { model | rewards = rewards, rewardsAsSelectList = rewardsAsSelectList }
            in
            (( updatedModel, getPlans updatedModel ), NoOp)

        Cancel ->
            (( { model | isEditingYourCampaigns = False, isEditingReward = False }, Cmd.none ), NoOp)

        HideRewardConfirmation ->
            let
                selectedReward =
                    SelectList.selected model.rewardsAsSelectList

                updatedSelectedReward =
                    { selectedReward | confirmation = False }

                befores =
                    SelectList.before model.rewardsAsSelectList

                afters =
                    SelectList.after model.rewardsAsSelectList

                beforesAndAfters =
                    befores ++ afters

                defaultSelectedReward =
                    List.filter (\reward -> not (hasRewardId reward)) beforesAndAfters
                        |> List.head
                        |> Maybe.withDefault rewardWrapperDefault

                rewardAsSelectList =
                    SelectList.singleton defaultSelectedReward

                updatedRewards =
                    rewardAsSelectList
                        |> SelectList.append [ updatedSelectedReward ]
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            (( { model | rewardsAsSelectList = updatedRewards }, Cmd.none ), NoOp)

        HideCampaignConfirmation ->
            let
                selectedCampaign =
                    SelectList.selected model.yourCampaigns

                updatedSelectedCampaign =
                    { selectedCampaign | campaignConfirmation = False }

                befores =
                    SelectList.before model.yourCampaigns

                afters =
                    SelectList.after model.yourCampaigns

                beforesAndAfters =
                    befores ++ afters

                defaultSelectedCampaign =
                    List.filter (\campaign -> not (hasCampaignId campaign)) beforesAndAfters
                        |> List.head
                        |> Maybe.withDefault campaignWrapperDefault

                campaignAsSelectList =
                    SelectList.singleton defaultSelectedCampaign

                updatedCampaigns =
                    campaignAsSelectList
                        |> SelectList.append [ updatedSelectedCampaign ]
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            (( { model | yourCampaigns = updatedCampaigns }, Cmd.none ), NoOp)

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
            (( { model | yourSubscriptions = updatedSubscriptions }, Cmd.none ), NoOp)

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
                    (({ model | yourSubscriptions = updatedSubscriptions }, Cmd.none), NoOp)

                _ ->
                    (( model, Cmd.none ), NoOp)

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
                                |> Maybe.withDefault campaignWrapperDefault

                        campaignAsSelectList =
                            SelectList.singleton defaultSelectedCampaign

                        updatedCampaigns =
                            campaignAsSelectList
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    (({ model | yourCampaigns = updatedCampaigns, longDescription = "", fundingGoal = 0.0 }, Cmd.none), MakeMainFetchCampaigns)
                        

                _ ->
                    (( model, Cmd.none ), NoOp)

        SaveUpdateCampaignForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    (( model, putCampaign model ), NoOp)

                errors ->
                    (({ model | errors = errors }, Cmd.none), NoOp)

        DeleteCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\u -> u.campaign.id == campaignId) model.yourCampaigns

                updatedModel =
                    { model
                        | yourCampaigns = updatedCampaigns
                    }
            in
            (( updatedModel, getPlansForDeletion updatedModel ), NoOp)

        ShowRewardConfirmation rewardId ->
            let
                updatedWrapperRewards =
                    SelectList.select (\u -> u.reward.id == rewardId) model.rewardsAsSelectList

                selectedWrapperReward =
                    SelectList.selected updatedWrapperRewards

                updatedWrapperReward =
                    { selectedWrapperReward | confirmation = True }

                befores =
                    SelectList.before updatedWrapperRewards

                afters =
                    SelectList.after updatedWrapperRewards

                updatedRewardWrappers =
                    SelectList.singleton updatedWrapperReward
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            (( { model | rewardsAsSelectList = updatedRewardWrappers }, Cmd.none ), NoOp)

        ShowCampaignConfirmation campaignId ->
            let
                updatedWrapperCampaigns =
                    SelectList.select (\u -> u.campaign.id == campaignId) model.yourCampaigns

                selectedWrapperCampaign =
                    SelectList.selected updatedWrapperCampaigns

                updatedWrapperCampaign =
                    { selectedWrapperCampaign | campaignConfirmation = True }

                befores =
                    SelectList.before updatedWrapperCampaigns

                afters =
                    SelectList.after updatedWrapperCampaigns

                updatedCampaignWrappers =
                    SelectList.singleton updatedWrapperCampaign
                        |> SelectList.prepend befores
                        |> SelectList.append afters
            in
            (( { model | yourCampaigns = updatedCampaignWrappers }, Cmd.none ), NoOp)

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
            (( { model | yourSubscriptions = updatedSubscriptionWrappers }, Cmd.none ), NoOp)

        DeleteYourSubscription subscriptionId ->
            let
                updatedSubscriptions =
                    SelectList.select (\subscriptionWrapper -> subscriptionWrapper.subscription.id == subscriptionId) model.yourSubscriptions

                updatedModel =
                    { model | yourSubscriptions = updatedSubscriptions }
            in
            (( updatedModel, deleteSubscription updatedModel ), NoOp)

        SelectYourCampaign campaignId ->
            let
                updatedCampaigns =
                    SelectList.select (\u -> u.campaign.id == campaignId) model.yourCampaigns

                selectedCampaign =
                    SelectList.selected updatedCampaigns

                updatedModel =
                    { model
                        | yourCampaigns = updatedCampaigns
                        , isEditingYourCampaigns = True
                        , longDescription = selectedCampaign.campaign.longDescription
                        , fundingGoal = selectedCampaign.campaign.fundingGoal
                    }
            in
            (( updatedModel
            , getRewards updatedModel
            )
            , NoOp)

        UpdateFundingGoalField updatedFundingGoal ->
            let
                updatedFundingGoalFloat =
                    case String.toFloat updatedFundingGoal of
                        Ok matchedUpdatedFundingGoalFloat ->
                            matchedUpdatedFundingGoalFloat

                        Err error ->
                            0.0
            in
            (( { model | fundingGoal = updatedFundingGoalFloat }, Cmd.none ), NoOp)

        UpdateLongDescriptionField updatedLongDescription ->
            (( { model | longDescription = updatedLongDescription }, Cmd.none ), NoOp)

        HandlePutCampaign data ->
            case data of
                Success campaign ->
                    let
                        currentSelectedCampaignWrapper =
                            SelectList.selected model.yourCampaigns

                        currentSelectedCampaign =
                            currentSelectedCampaignWrapper
                                |> .campaign

                        updatedCampaign =
                            { currentSelectedCampaign
                                | longDescription = model.longDescription
                                , fundingGoal = model.fundingGoal
                            }

                        updatedCampaignWrapper =
                            { currentSelectedCampaignWrapper | campaign = updatedCampaign }

                        befores =
                            SelectList.before model.yourCampaigns

                        afters =
                            SelectList.after model.yourCampaigns

                        updatedCampaignWrappers =
                            SelectList.singleton updatedCampaignWrapper
                                |> SelectList.prepend befores
                                |> SelectList.append afters
                    in
                    (({ model
                        | yourCampaigns = updatedCampaignWrappers
                        , currentFunding = 0.0
                        , fundingGoal = 0.0
                        , longDescription = ""
                        , isEditingYourCampaigns = False
                    }
                    , Cmd.none)
                    , MakeMainFetchCampaigns)

                _ ->
                    (( model, Cmd.none )
                    ,   NoOp)


putCampaign : Model -> Cmd Msg
putCampaign model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .campaign
                |> .id
                |> String.fromInt

        rewardUrl =
            model.apiUrl ++ "/campaigns/" ++ selectedCampaignId

        data =
            { longDescription = model.longDescription
            , fundingGoal = model.fundingGoal
            , userId = selectedCampaign.campaign.userId
            , githubRepoId = selectedCampaign.campaign.githubRepoId
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

        yourRenderedSubscriptionsHtml =
            renderSubscriptions model

        yourRenderedSubscriptionsAsGroups =
            ListExtra.greedyGroupsOf 2 yourRenderedSubscriptionsHtml
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
                    , a [ class "link", Router.href Router.CreateCampaignRoute ]
                        [ text "Start a campaign!" ]
                    ]
                ]


updateCampaignForm : Model -> CampaignWrapper -> List IncludedStuff -> Html Msg
updateCampaignForm model campaignWrapper included =
    let
        campaign =
            campaignWrapper.campaign

        repoForCampaign =
            List.filter
                (\includedStuff ->
                    case includedStuff of
                        IncludedGithub includedRepo ->
                            campaign.githubRepoId == includedRepo.id

                        _ ->
                            False
                )
                included
                |> List.head
                |> Maybe.withDefault Campaigns.includedRepoDefault

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
                 , hr [] []
                 , h1 [ class "title" ] [ text "Rewards" ]
                 ]
                    ++ displayRewards
                    ++ [ createRewardForm model ]
                    ++ [ displayUpdateButton (p [ class "control" ] [ button [ class "button is-primary", onClick SaveRewardForm ] [ text "Save Reward" ] ])
                       ]
                )
            ]

    else
        div [ class "card" ]
            [ div [ class "card-content" ]
                ([ displayCampaignUpdateFormHeader repoForCampaign
                 , viewErrors model.errors
                 , displayUpdateLongDescription model
                 , displayUpdateFundingGoal model
                 , hr [] []
                 , p [ class "card-header-title" ] [ text "Rewards" ]
                 ]
                    ++ displayRewards
                    ++ [ displayUpdateButton (p [ class "control" ] [ button [ class "button is-primary", onClick ShowRewardForm ] [ text "Add New Reward" ] ]) ]
                )
            ]


filterPersistedRewards : List RewardWrapper -> List RewardWrapper
filterPersistedRewards rewardWrapperList =
    List.filter rewardHasId rewardWrapperList


planHasId : Plan -> Bool
planHasId plan =
    not (plan.id == "")


rewardHasId : RewardWrapper -> Bool
rewardHasId rewardWrapper =
    let
        reward =
            rewardWrapper.reward
    in
    not (reward.id == 0)


renderUpdateOrShow : Position -> RewardWrapper -> Html Msg
renderUpdateOrShow position rewardWrapper =
    let
        reward =
            rewardWrapper.reward
    in
    if position == SelectList.Selected then
        div []
            [ div [ class "card" ]
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
                                [ text "Save Reward" ]
                            ]
                        ]
                    ]
                , footer [ class "card-footer" ]
                    []
                ]
            , hr [] []
            ]

    else if reward.id == 0 then
        div [] []

    else
        div []
            [ div [ class "card" ]
                [ div [ class "card-content" ]
                    [ label [ class "label" ]
                        [ text "Donation Level" ]
                    , p [ class "control" ]
                        [ text (String.fromFloat reward.donationLevel) ]
                    , label [ class "label" ]
                        [ text "Description" ]
                    , p []
                        [ text reward.description ]
                    ]
                , footer [ class "card-footer" ]
                    [ a [ class "card-footer-item", onClick (SelectReward reward.id) ]
                        [ span [] [ text "edit reward" ] ]
                    , a [ class "card-footer-item", onClick (DeletePlan reward.id) ]
                        [ span [] [ text "delete reward" ] ]
                    ]
                ]
            , hr [] []
            ]


displayUpdateRewards : Model -> SelectList (Html Msg)
displayUpdateRewards model =
    model.rewardsAsSelectList
        |> SelectList.mapBy renderUpdateOrShow


showReward : RewardWrapper -> Html Msg
showReward rewardWrapper =
    let
        reward =
            rewardWrapper.reward
    in
    case rewardWrapper.confirmation of
        True ->
            div [ class "card" ]
                [ div [ class "card-content" ]
                    [ label [ class "label" ]
                        [ text "Donation Level" ]
                    , p [ class "control" ]
                        [ text (String.fromFloat reward.donationLevel) ]
                    , label [ class "label" ]
                        [ text "Description" ]
                    , p []
                        [ text reward.description ]
                    ]
                , footer [ class "card-footer" ]
                    [ label [ class "card-header-icon" ]
                        [ span [] [ text "Are you sure?" ] ]
                    , a [ class "card-header-icon", onClick (DeletePlan reward.id) ]
                        [ span [] [ text "Yes" ] ]
                    , a [ class "card-header-icon", onClick HideRewardConfirmation ]
                        [ span [] [ text "No" ] ]
                    ]
                , hr [] []
                ]

        False ->
            div [ class "card" ]
                [ div [ class "card-content" ]
                    [ label [ class "label" ]
                        [ text "Donation Level" ]
                    , p [ class "control" ]
                        [ text (String.fromFloat reward.donationLevel) ]
                    , label [ class "label" ]
                        [ text "Description" ]
                    , p []
                        [ text reward.description ]
                    ]
                , footer [ class "card-footer" ]
                    [ a [ class "card-footer-item", onClick (SelectReward reward.id) ]
                        [ span [] [ text "edit reward" ] ]
                    , a [ class "card-footer-item", onClick (ShowRewardConfirmation reward.id) ]
                        [ span [] [ text "delete reward" ] ]
                    ]
                , hr [] []
                ]


hasRewardId : RewardWrapper -> Bool
hasRewardId rewardWrapper =
    let
        reward =
            rewardWrapper.reward
    in
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
            [ text (String.fromFloat reward.donationLevel) ]
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


filterPersistedCampaigns : List CampaignWrapper -> List CampaignWrapper
filterPersistedCampaigns campaignList =
    List.filter hasCampaignId campaignList


filterPersistedSubscriptions : List SubscriptionWrapper -> List SubscriptionWrapper
filterPersistedSubscriptions subscriptionList =
    List.filter hasSubscriptionId subscriptionList


hasCampaignId : CampaignWrapper -> Bool
hasCampaignId campaignWrapper =
    not (campaignWrapper.campaign.id == 0)


hasSubscriptionId : SubscriptionWrapper -> Bool
hasSubscriptionId subscriptionWrapper =
    not (subscriptionWrapper.subscription.id == "0")


showYourCampaign : CampaignWrapper -> List IncludedStuff -> Html Msg
showYourCampaign campaignWrapper included =
    let
        campaign =
            campaignWrapper.campaign

        repoForCampaign =
            List.filter
                (\includedStuff ->
                    case includedStuff of
                        IncludedGithub includedRepo ->
                            campaign.githubRepoId == includedRepo.id

                        _ ->
                            False
                )
                included
                |> List.head
                |> Maybe.withDefault Campaigns.includedRepoDefault
    in
    case campaignWrapper.showButtonCode of
        False ->
            div [ class "card" ]
                [ displayCampaignFormHeader campaignWrapper repoForCampaign
                , displayCampaignFormContent campaignWrapper
                , showCampaignFooter campaignWrapper
                ]

        True ->
            div [ class "card" ]
                [ displayCampaignFormHeader campaignWrapper repoForCampaign
                , displayCampaignFormContent campaignWrapper
                , renderButtonCode campaignWrapper repoForCampaign
                , showCampaignFooter campaignWrapper
                ]


renderButtonCode : CampaignWrapper -> IncludedStuff -> Html Msg
renderButtonCode campaignWrapper included =
    let
        campaign =
            campaignWrapper.campaign

        href =
            "https://tossbounty.com/#/contribute/" ++ String.fromInt campaign.id

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

        name =
            repo.name
    in
    div [ class "card-content" ]
        [ code [ class "code" ]
            [ text
                ("<a style=\"\nbackground-color: #00c4a7;\nborder-color: transparent;\ncolor: #fff;\ncursor: pointer;\n\njustify-content: center;\n\npadding-bottom: calc(0.375em - 1px);\n\npadding-left: 0.75em;\n\npadding-right: 0.75em;\n\npadding-top: calc(0.375em - 1px);\n\ntext-align: center;\n\nwhite-space: nowrap;\nborder-width: 1px;\nalign-items: center;\n\nborder: 1px solid transparent;\nborder-radius: 4px;\n\nbox-shadow: none;\n\ndisplay: inline-flex;\n\nfont-size: 1rem;\n\nheight: 2.25em;\nline-height: 1.5;\nposition: relative;\n\nvertical-align: top;\nfont-family: BlinkMacSystemFont, -apple-system, \"Segoe UI\", \"Roboto\", \"Oxygen\", \"Ubuntu\", \"Cantarell\", \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", \"Helvetica\", \"Arial\", sans-serif;\nmargin: 0;\ntext-decoration: none; font: menu;\ndisplay: inline-block; padding: 2px 8px;\nbackground: ButtonFace;color: ButtonText;\nborder-style: solid;border-width: 2px;\n    border-color: ButtonHighlight ButtonShadow ButtonShadow ButtonHighlight;\" href=\""
                    ++ href
                    ++ "\">"
                    ++ "TossBounty at "
                    ++ name
                    ++ "</a>"
                )
            ]
        ]


showCampaignFooter : CampaignWrapper -> Html Msg
showCampaignFooter campaignWrapper =
    let
        campaign =
            campaignWrapper.campaign

        showButtonCodeButton =
            case campaignWrapper.showButtonCode of
                True ->
                    "hide button code"

                False ->
                    "show button code"
    in
    case campaignWrapper.campaignConfirmation of
        True ->
            footer [ class "card-footer" ]
                [ div [ class "card-footer-item" ]
                    [ label []
                        [ span [] [ text "Are you sure?" ] ]
                    , a [ class "card-footer-item", onClick (DeleteCampaign campaign.id) ]
                        [ span [] [ text "Yes" ] ]
                    , a [ class "card-footer-item", onClick HideCampaignConfirmation ]
                        [ span [] [ text "No" ] ]
                    ]
                ]

        False ->
            footer [ class "card-footer" ]
                [ div [ class "card-footer-item" ]
                    [ a [ class "card-footer-item", onClick (ShowCampaignConfirmation campaign.id) ]
                        [ span [] [ text "delete campaign" ] ]
                    , a [ class "card-footer-item", onClick (SelectYourCampaign campaign.id) ]
                        [ span [] [ text "edit campaign" ] ]
                    , a [ class "card-footer-item", onClick (ToggleButtonCode campaign.id) ]
                        [ span [] [ text showButtonCodeButton ] ]
                    ]
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


displayUpdateButton : Html Msg -> Html Msg
displayUpdateButton updateButton =
    div [ class "field is-grouped" ]
        [ p [ class "control" ]
            [ button [ class "button is-primary", onClick SaveUpdateCampaignForm ]
                [ text "Save Campaign" ]
            ]
        , updateButton
        , p [ class "control" ]
            [ button [ class "button is-danger", onClick Cancel ] [ text "Cancel" ]
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
                , Html.Attributes.value (String.fromFloat model.fundingGoal)
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


displayCampaignFormHeader : CampaignWrapper -> IncludedStuff -> Html Msg
displayCampaignFormHeader campaignWrapper included =
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

        campaign =
            campaignWrapper.campaign
    in
    div [ class "card-header" ]
        [ p [ class "card-header-title" ] [ text repo.name ]
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
                    [ text ("$ " ++ String.fromFloat plan.amount) ]
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
                    [ text ("$ " ++ String.fromFloat plan.amount) ]
                , a [ class "card-header-icon", onClick (ShowConfirmation subscription.id) ]
                    [ span [] [ text "delete" ] ]
                ]


displaySubscriptionFormContent : Plan -> Html Msg
displaySubscriptionFormContent plan =
    div [ class "card-content" ]
        [ label [ class "label" ]
            [ text "Name" ]
        , p [ class "control" ]
            [ text (String.fromInt plan.name) ]
        ]


displayCampaignFormContent : CampaignWrapper -> Html Msg
displayCampaignFormContent campaignWrapper =
    let
        campaign =
            campaignWrapper.campaign
    in
    div [ class "card-content" ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Summary" ]
            , p [ class "control" ]
                [ text campaign.longDescription ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding Goal" ]
            , p [ class "control" ]
                [ text (String.fromFloat campaign.fundingGoal) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Funding Progress" ]
            , progress [ class "progress", value (String.fromFloat campaign.currentFunding), Html.Attributes.max (String.fromFloat campaign.fundingGoal) ] [ text (String.fromFloat campaign.currentFunding) ]
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
                    , value (String.fromFloat model.donationLevel)
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
                    , onInput UpdateNewDescriptionField
                    , value model.description
                    ]
                    []
                ]
            ]
        , hr [] []
        ]


deleteCampaign : Model -> Cmd Msg
deleteCampaign model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .campaign
                |> .id
                |> String.fromInt

        rewardUrl =
            model.apiUrl ++ "/campaigns/" ++ selectedCampaignId

        data =
            { id = selectedCampaignId
            , currentFunding = selectedCampaign.campaign.currentFunding
            , longDescription = selectedCampaign.campaign.longDescription
            , fundingGoal = selectedCampaign.campaign.fundingGoal
            , userId = selectedCampaign.campaign.userId
            , githubRepoId = selectedCampaign.campaign.githubRepoId
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
                |> .campaign
                |> .id
                |> String.fromInt

        updatedUrl =
            model.apiUrl ++ "/rewards/?campaign_id=" ++ selectedCampaignId
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
        selectedRewardWrapper =
            SelectList.selected model.rewardsAsSelectList

        selectedReward =
            selectedRewardWrapper.reward

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ String.fromInt selectedReward.id
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteReward (Encode.object [])


putReward : Model -> Cmd Msg
putReward model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedRewardWrapper =
            SelectList.selected model.rewardsAsSelectList

        selectedReward =
            selectedRewardWrapper.reward

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ String.fromInt selectedReward.id

        data =
            { description = selectedReward.description
            , campaignId = selectedCampaign.campaign.id
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) rewardUrl HandlePutReward Reward.showDecoder (Reward.updateEncode data)


putPlan : Model -> Cmd Msg
putPlan model =
    let
        selectedRewardWrapper =
            SelectList.selected model.rewardsAsSelectList

        selectedReward =
            selectedRewardWrapper.reward

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
                |> .campaign
                |> .id
                |> String.fromInt

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
            , campaignId = selectedCampaign.campaign.id
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) rewardUrl HandleReward Reward.showDecoder (Reward.encode data)


postPlan : Model -> Cmd Msg
postPlan model =
    let
        planUrl =
            model.apiUrl ++ "/plans"

        selectedRewardWrapper =
            model.rewardsAsSelectList
                |> SelectList.selected

        selectedReward =
            selectedRewardWrapper.reward

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
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) planUrl HandleDeletePlans (Encode.object [])


deleteRewards : Model -> Cmd Msg
deleteRewards model =
    let
        selectedRewardWrapper =
            model.rewardsAsSelectList
                |> SelectList.selected

        selectedReward =
            selectedRewardWrapper.reward

        rewardUrl =
            model.apiUrl ++ "/rewards/" ++ String.fromInt selectedReward.id
    in
    RemoteData.Http.deleteWithConfig (Auth.config model.token) rewardUrl HandleDeleteRewards (Encode.object [])


getPlansForDeletion : Model -> Cmd Msg
getPlansForDeletion model =
    let
        selectedCampaign =
            SelectList.selected model.yourCampaigns

        selectedCampaignId =
            selectedCampaign
                |> .campaign
                |> .id
                |> String.fromInt

        apiUrl =
            model.apiUrl

        token =
            model.token

        reposUrl =
            apiUrl ++ "/plans/" ++ "?campaign_id=" ++ selectedCampaignId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleGetPlansForDeletion Plans.decoder
