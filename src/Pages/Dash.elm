module Pages.Dash exposing (..)

import Data.AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default, defaultDate, encode, showDecoder)
import Data.Campaigns as Campaigns exposing (IncludedStuff(..))
import Data.Plan as Plan exposing (Plan)
import Data.Subscription as Subscription exposing (Subscription)
import Html exposing (..)
import Html.Attributes exposing (class, src, style, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as ListExtra exposing (greedyGroupsOf)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router exposing (href)
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


type alias SubscriptionWrapper =
    { subscription : Subscription
    , showConfirmation : Bool
    }


subscriptionWrapperDefault =
    { subscription = Subscription.default
    , showConfirmation = False
    }


type alias Model =
    { campaignId : String
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
    , yourCampaigns = updatedYourCampaigns
    , yourRepos = yourRepos
    , yourSubscriptions = updatedYourSubscriptions
    , yourSubscribedPlans = updatedYourPlans
    , apiUrl = url
    , token = token
    , errors = []
    , isEditingYourCampaigns = False
    , showConfirmation = False
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

        DeleteCampaign rewardId ->
            let
                updatedCampaigns =
                    SelectList.select (\u -> u.id == rewardId) model.yourCampaigns

                updatedModel =
                    { model | yourCampaigns = updatedCampaigns }
            in
            ( updatedModel, deleteCampaign updatedModel ) => NoOp

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
            in
            ( { model
                | yourCampaigns = updatedCampaigns
                , isEditingYourCampaigns = True
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
    in
    div [ class "card" ]
        [ div [ class "card-content" ]
            [ displayCampaignUpdateFormHeader repoForCampaign
            , viewErrors model.errors
            , displayUpdateLongDescription model
            , displayUpdateFundingGoal model
            , displayUpdateButton
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
