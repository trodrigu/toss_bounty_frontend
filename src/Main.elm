module Main exposing (HandleMsg(..), Model, Msg(..), Page(..), burgerMenu, burgerMenuNavItems, consumeToken, decodeUrlFromJson, decodeUserFromJson, developerHeroArea, fetchYourCampaigns, fetchYourCampaignsForRewards, fetchYourSubscribedPlans, fetchYourSubscriptions, getCampaign, getCampaigns, getGitHubSignInUrl, getIssues, getRepo, getRepos, getRewards, getStripeConnectUrl, getUser, githubUrlDecoder, init, main, pageView, sessionChange, setRoute, stripeConnectUrlUrlDecoder, subscriptions, update, updatePage, updateUserWithStripeInfo, view)

import Data.AuthToken exposing (AuthToken, fallback, init)
import Data.Campaign as Campaign exposing (..)
import Data.Campaigns as Campaigns exposing (Campaigns, IncludedRepo, IncludedStuff(..), decoder)
import Data.Issues as Issues exposing (Issues)
import Data.Plans as Plans exposing (Plans, decoder)
import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, SelectListRepos, defaultSelectListRepos, mostBountifulRepo, selectListDecoder)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Session as Session exposing (Session)
import Data.Stripe as Stripe exposing (Stripe, decoder)
import Data.StripeConnectUrl as StripeConnectUrl exposing (StripeConnectUrl)
import Data.Subscriptions as Subscriptions exposing (Subscriptions, decoder)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (succeed, Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, optionalAt, requiredAt)
import Pages.About as About
import Pages.Contribute as Contribute
import Pages.CreateCampaign as CreateCampaign
import Pages.CreateRewards as CreateRewards
import Pages.CreateUserRole as CreateUserRole
import Pages.Dash as Dash
import Pages.Discover as Discover
import Pages.GithubOops as GithubOops
import Pages.Home as Home exposing (GitHubUrl)
import Pages.Login as Login
import Pages.NotFound as NotFound
import Pages.StripeConnectSignUp as StripeConnectSignUp
import Pages.TosserSignUp as TosserSignUp
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Request.User exposing (storeSession)
import Routing.Router as Router exposing (Route(..), fromLocation)
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Views.Page as Page exposing (frame)
import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Navigation exposing (Key)
import Url exposing (Url)


type Page
    = Home Home.Model
    | TosserSignUp TosserSignUp.Model
    | Contribute Contribute.Model
    | Dash Dash.Model
    | Login Login.Model
    | About About.Model
    | StripeConnectSignUp StripeConnectSignUp.Model
    | CreateCampaign CreateCampaign.Model
    | CreateRewards CreateRewards.Model
    | CreateUserRole CreateUserRole.Model
    | Discover Discover.Model
    | GithubOops GithubOops.Model
    | NotFound NotFound.Model


type alias Model =
    { session : Session
    , location : Url
    , page : Page
    , githubUrl : WebData GitHubUrl
    , apiUrl : Maybe String
    , yourCampaigns : WebData Campaigns
    , yourSubscriptions : WebData Subscriptions
    , yourSubscribedPlans : WebData Plans
    , repos : WebData SelectListRepos
    , mostBountifulIssues : WebData Issues
    , stripeConnectUrl : WebData StripeConnectUrl
    , stripe : Maybe Stripe
    , campaignOfInterest : WebData Campaign
    , repoOfInterest : WebData Repo
    , rewardsOfInterest : WebData Rewards
    , allCampaigns : WebData Campaigns
    , showMenu : Bool
    , key : Key
    }


type Msg
    = HomeMsg Home.Msg
    | TosserSignUpMsg TosserSignUp.Msg
    | ContributeMsg Contribute.Msg
    | DiscoverMsg Discover.Msg
    | DashMsg Dash.Msg
    | LoginMsg Login.Msg
    | GithubOopsMsg GithubOops.Msg
    | AboutMsg About.Msg
    | CreateCampaignMsg CreateCampaign.Msg
    | CreateRewardsMsg CreateRewards.Msg
    | StripeConnectSignUpMsg StripeConnectSignUp.Msg
    | CreateUserRoleMsg CreateUserRole.Msg
    | NotFoundMsg NotFound.Msg
    | SetUser (Maybe User)
    | ConsumeToken (Maybe Stripe)
    | SetRoute (Maybe Route)
    | HandleMsg HandleMsg
    | ToggleMenu
    | ClickedLink UrlRequest
    | ChangedUrl Url


type HandleMsg
    = HandleGithubUrl (WebData GitHubUrl)
    | HandleFetchYourCampaigns (WebData Campaigns)
    | HandleFetchCampaignsForRewards (WebData Campaigns)
    | HandleFetchRepos (WebData SelectListRepos)
    | HandleFetchIssues (WebData Issues)
    | HandleStripeConnectUrl (WebData StripeConnectUrl)
    | HandleStripeIdUpdate (WebData User)
    | HandleFetchRewards (WebData Rewards)
    | HandleFetchCampaign (WebData Campaign)
    | HandleFetchRepo (WebData Repo)
    | HandleFetchAllCampaigns (WebData Campaigns)
    | HandleFetchUser (WebData User)
    | HandleFetchYourSubscriptions (WebData Subscriptions)
    | HandleFetchYourSubscribedPlans (WebData Plans)


decodeUserFromJson : Decode.Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue (Decode.field "session" Decode.string)
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.returnToSessionDecoder >> Result.toMaybe)


init : Decode.Value -> Url -> Key -> ( Model, Cmd Msg )
init val location key =
    setRoute (Router.fromLocation location)
        { session = { user = (decodeUserFromJson val) }
        , location = location
        , page = Home (decodeUrlFromJson val |> Home.init)
        , githubUrl = Loading
        , apiUrl = decodeUrlFromJson val
        , yourCampaigns = NotAsked
        , yourSubscriptions = NotAsked
        , yourSubscribedPlans = NotAsked
        , repos = NotAsked
        , mostBountifulIssues = Loading
        , stripeConnectUrl = NotAsked
        , stripe = Nothing
        , campaignOfInterest = NotAsked
        , repoOfInterest = NotAsked
        , rewardsOfInterest = NotAsked
        , allCampaigns = NotAsked
        , showMenu = False
        , key = key
        }


decodeUrlFromJson : Decode.Value -> Maybe String
decodeUrlFromJson json =
    json
        |> Decode.decodeValue (Decode.field "apiUrl" Decode.string)
        |> Result.toMaybe


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Just Router.AboutRoute ->
            let
                updatedPage =
                    About About.init
            in
            ({ model | page = updatedPage } , Cmd.none)

        Just Router.HomeRoute ->
            (model , Cmd.map HandleMsg (getGitHubSignInUrl model.apiUrl))

        Just (Router.SaveTokenRoute (Just token) (Just email) (Just userId)) ->
            let
                updatedToken =
                    Data.AuthToken.init token

                user =
                    { email = email
                    , token = updatedToken
                    , userId = userId
                    , stripeExternalId = ""
                    , stripeAccessToken = ""
                    , role = 0
                    }

                session =
                    model.session

                updatedModel =
                    { model | session = { session | user = Just user } }
            in
            (updatedModel
                , Cmd.map HandleMsg (getUser model.apiUrl updatedToken userId))

        Just (Router.SaveTokenRoute _ _ _) ->
            (model
                , Cmd.batch
                    [ Router.modifyUrl model.key Router.DashRoute
                    ])

        Just Router.LoginRoute ->
            let
                updatedPage =
                    Login Login.init
            in
            ({ model | page = updatedPage } , Cmd.none)

        Just Router.LogoutRoute ->
            let
                session =
                    model.session
            in
            ({ model | session = { session | user = Nothing } }
                , Cmd.batch
                    [ Ports.storeSession Nothing
                    , Router.modifyUrl model.key Router.HomeRoute
                    ])

        Just (Router.ContributeRoute campaignId) ->
            case model.session.user of
                Just user ->
                    case model.campaignOfInterest of
                        Success matchedCampaign ->
                            if campaignId == matchedCampaign.id then
                                let
                                    token =
                                        user.token

                                    apiUrl =
                                        model.apiUrl

                                    campaign =
                                        model.campaignOfInterest

                                    repo =
                                        model.repoOfInterest

                                    rewards =
                                        model.rewardsOfInterest

                                    updatedPage =
                                        Contribute (Contribute.init model.key token apiUrl campaign repo rewards user)
                                in
                                ({ model | page = updatedPage } , Cmd.none)

                            else
                                let
                                    token =
                                        user.token

                                    apiUrl =
                                        case model.apiUrl of
                                            Nothing ->
                                                ""

                                            Just url ->
                                                url
                                in
                                (model , Cmd.map HandleMsg (getCampaign apiUrl token (String.fromInt campaignId)))

                        Loading ->
                            (model , Cmd.none)

                        Failure _ ->
                            (model , Cmd.none)

                        NotAsked ->
                            let
                                token =
                                    user.token

                                apiUrl =
                                    case model.apiUrl of
                                        Nothing ->
                                            ""

                                        Just url ->
                                            url
                            in
                            (model , Cmd.map HandleMsg (getCampaign apiUrl token (String.fromInt campaignId)))

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just Router.TosserSignUpRoute ->
            let
                updatedPage =
                    TosserSignUp TosserSignUp.init
            in
            ({ model | page = updatedPage } , Cmd.none)

        Just Router.StripeConnectSignUpRoute ->
            case model.session.user of
                Just user ->
                    let
                        cmd =
                            case user.stripeExternalId of
                                "" ->
                                    case model.stripeConnectUrl of
                                        Success stripeConnectUrl ->
                                            Cmd.none

                                        Failure error ->
                                            Cmd.none

                                        Loading ->
                                            Cmd.none

                                        NotAsked ->
                                            Cmd.map HandleMsg (getStripeConnectUrl model.apiUrl)

                                externalId ->
                                    Cmd.map HandleMsg (fetchYourCampaignsForRewards model.apiUrl user.token user.userId)

                        updatedPage =
                            StripeConnectSignUp (StripeConnectSignUp.init model.stripeConnectUrl)
                    in
                    ({ model | page = updatedPage } , cmd)

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just (Router.SaveStripeRoute (Just stripeId)) ->
            case model.session.user of
                Just user ->
                    let
                        session =
                            model.session

                        token =
                            user.token

                        updatedUser =
                            { user | stripeExternalId = stripeId }

                        updatedModel =
                            { model
                                | page = CreateRewards (CreateRewards.init model.key Nothing Data.AuthToken.fallback 0)
                                , session = { session | user = Just updatedUser }
                            }
                    in
                    (updatedModel , Cmd.map HandleMsg (updateUserWithStripeInfo model.apiUrl token stripeId user.userId))

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just (Router.SaveStripeRoute Nothing) ->
            (model
                , Cmd.batch
                    [ Router.modifyUrl model.key Router.HomeRoute
                    ])

        Just Router.DashRoute ->
            case model.session.user of
                Just user ->
                    let
                        token =
                            user.token

                        updatedModelAndCmd =
                            case model.yourCampaigns of
                                NotAsked ->
                                    (model , Cmd.map HandleMsg (fetchYourCampaigns model.apiUrl user.token user.userId))

                                Loading ->
                                    (model , Cmd.none)

                                Failure error ->
                                    (model , Cmd.none)

                                Success campaignsData ->
                                    case model.yourSubscriptions of
                                        NotAsked ->
                                            (model , Cmd.map HandleMsg (fetchYourSubscriptions model.apiUrl user.token user.userId))

                                        Loading ->
                                            (model , Cmd.none)

                                        Failure error ->
                                            (model , Cmd.none)

                                        Success subscriptionsData ->
                                            let
                                                repos =
                                                    List.filter
                                                        (\included ->
                                                            case included of
                                                                IncludedGithub includedRepo ->
                                                                    True

                                                                IncludedStripe _ ->
                                                                    False
                                                        )
                                                        campaignsData.included
                                            in
                                            case model.yourSubscribedPlans of
                                                NotAsked ->
                                                    (model , Cmd.map HandleMsg (fetchYourSubscribedPlans model.apiUrl user.token user.userId))

                                                Loading ->
                                                    (model , Cmd.none)

                                                Failure error ->
                                                    (model , Cmd.none)

                                                Success plansData ->
                                                    ({ model | page = Dash (Dash.init model.apiUrl token campaignsData.campaigns repos subscriptionsData.subscriptions plansData.plans) } , Cmd.none)
                    in
                    updatedModelAndCmd

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just Router.DiscoverRoute ->
            case model.session.user of
                Just user ->
                    let
                        token =
                            user.token

                        apiUrl =
                            model.apiUrl

                        cmd =
                            case model.allCampaigns of
                                Success campaigns ->
                                    Cmd.none

                                Failure error ->
                                    Cmd.none

                                Loading ->
                                    Cmd.none

                                NotAsked ->
                                    Cmd.map HandleMsg (getCampaigns apiUrl token)

                        updatedPage =
                            Discover (Discover.init model.key token apiUrl model.allCampaigns)
                    in
                    ({ model | page = updatedPage } , cmd)

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just Router.CreateCampaignRoute ->
            case model.session.user of
                Just user ->
                    let
                        session =
                            model.session

                        token =
                            user.token

                        userId =
                            user.userId

                        cmd =
                            case model.repos of
                                Success repos ->
                                    Cmd.none

                                Failure error ->
                                    Cmd.none

                                Loading ->
                                    Cmd.none

                                NotAsked ->
                                    Cmd.map HandleMsg (getRepos model.apiUrl token userId)

                        updatedPage =
                            CreateCampaign (CreateCampaign.init token userId model.repos model.apiUrl)
                    in
                    ({ model | page = updatedPage } , cmd)

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just Router.CreateRewardsRoute ->
            case model.session.user of
                Just user ->
                    let
                        session =
                            model.session

                        token =
                            user.token

                        apiUrl =
                            model.apiUrl

                        campaignsSorted =
                            case model.yourCampaigns of
                                Success yourCampaigns ->
                                    yourCampaigns
                                        |> .campaigns
                                        |> List.sortBy .id

                                _ ->
                                    []

                        campaign =
                            campaignsSorted
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault Campaign.default

                        userId =
                            user.userId

                        cmd =
                            case model.yourCampaigns of
                                Success campaigns ->
                                    Cmd.none

                                Failure error ->
                                    Cmd.none

                                Loading ->
                                    Cmd.none

                                NotAsked ->
                                    Cmd.map HandleMsg (fetchYourCampaignsForRewards model.apiUrl token userId)

                        updatedPage =
                            CreateRewards (CreateRewards.init model.key apiUrl token campaign.id)
                    in
                    ({ model | page = updatedPage } , cmd)

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just Router.CreateUserRoleRoute ->
            case model.session.user of
                Just user ->
                    let
                        token =
                            user.token

                        apiUrl =
                            model.apiUrl

                        updatedPage =
                            CreateUserRole (CreateUserRole.init token apiUrl user)
                    in
                    ({ model | page = updatedPage } , Cmd.none)

                Nothing ->
                    (model , Router.modifyUrl model.key Router.HomeRoute)

        Just Router.NotFoundRoute ->
            (model , Cmd.none)

        Just Router.GithubOopsRoute ->
            let
                updatedPage =
                    GithubOops GithubOops.init
            in
            ({ model | page = updatedPage } , Cmd.none)

        Nothing ->
            (model , Cmd.none)


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        pageSession =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        ( ClickedLink urlRequest, _) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (url.path))

                Browser.External href ->
                    if href == "" then
                        (model, Cmd.none)
                    else
                        (model, Navigation.load href)

        ( ChangedUrl url, _) ->
            setRoute (Router.fromLocation url) model


        ( HandleMsg (HandleFetchUser data), _ ) ->
            case data of
                Success user ->
                    let
                        currentUser =
                            pageSession.user
                                |> Maybe.withDefault User.default

                        updatedUser =
                            { currentUser | role = user.role }

                        updatedModel =
                            { model | session = { pageSession | user = Just updatedUser } }
                    in
                    case user.role of
                        0 ->
                            (updatedModel , Cmd.batch [ Request.User.storeSession updatedUser, Router.modifyUrl model.key CreateUserRoleRoute ])

                        _ ->
                            (updatedModel , Cmd.batch [ Request.User.storeSession updatedUser, Router.modifyUrl model.key DiscoverRoute ])

                _ ->
                    (model , Cmd.batch [ Router.modifyUrl model.key HomeRoute ])

        ( HandleMsg (HandleFetchRepo data), _ ) ->
            let

                user =
                    pageSession.user

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                campaignId =
                    case model.campaignOfInterest of
                        Success campaign ->
                            campaign.id

                        _ ->
                            0

                apiUrl =
                    case model.apiUrl of
                        Nothing ->
                            ""

                        Just url ->
                            url
            in
            ( { model | repoOfInterest = data }
            , Cmd.map HandleMsg (getRewards apiUrl token campaignId)
            )

        ( HandleMsg (HandleFetchYourSubscribedPlans data), _ ) ->
            let
                user =
                    pageSession.user

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                apiUrl =
                    case model.apiUrl of
                        Nothing ->
                            ""

                        Just url ->
                            url
            in
            ( { model | yourSubscribedPlans = data }
            , Router.modifyUrl model.key DashRoute
            )

        ( HandleMsg (HandleFetchYourSubscriptions data), _ ) ->
            let
                user =
                    pageSession.user

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                apiUrl =
                    case model.apiUrl of
                        Nothing ->
                            ""

                        Just url ->
                            url
            in
            ( { model | yourSubscriptions = data }
            , Router.modifyUrl model.key DashRoute
            )

        ( HandleMsg (HandleFetchCampaign data), _ ) ->
            let
                user =
                    pageSession.user

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                githubRepoId =
                    case data of
                        Success campaign ->
                            campaign.githubRepoId

                        _ ->
                            "0"

                apiUrl =
                    case model.apiUrl of
                        Nothing ->
                            ""

                        Just url ->
                            url
            in
            ( { model | campaignOfInterest = data }
            , Cmd.map HandleMsg (getRepo apiUrl token githubRepoId)
            )

        ( HandleMsg (HandleFetchRewards updatedRewards), _ ) ->
            let
                campaignId =
                    case model.campaignOfInterest of
                        Success campaign ->
                            campaign.id

                        _ ->
                            0
            in
            ( { model | rewardsOfInterest = updatedRewards }, Router.modifyUrl model.key (ContributeRoute campaignId) )

        ( ConsumeToken stripe, Contribute subModel ) ->
            let
                newSubModel =
                    { subModel
                        | stripe = stripe
                        , isPaying = False
                        , paid = True
                        , subscription = Loading
                    }

                ( ( pageModel, cmdFromPage ), msgFromPage ) =
                    Contribute.update Contribute.MakeSubscription newSubModel
            in
            ({ model | stripe = stripe, page = Contribute newSubModel } , Cmd.map ContributeMsg cmdFromPage)

        ( SetRoute route, _ ) ->
            setRoute route model

        ( SetUser user, _ ) ->
            let
                cmd =
                    -- If we just signed out, then redirect to Home.
                    if pageSession.user /= Nothing && user == Nothing then
                        Router.modifyUrl model.key Router.HomeRoute

                    else
                        Cmd.none
            in
            ({ model | session = { pageSession | user = user } }
                , cmd)

        ( HomeMsg subMsg, Home subModel ) ->
            let
                newSubModel =
                    { subModel | webDataUrl = model.githubUrl }

                ( ( pageModel, cmd ), msgFromPage ) =
                    Home.update subMsg newSubModel
                    
            in
                ({ model | page = Home pageModel } , Cmd.map HomeMsg cmd)

        ( ContributeMsg subMsg, Contribute subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Contribute.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Contribute.NoOp ->
                            { model | page = Contribute pageModel }

                        Contribute.Sync ->
                            { model | page = Contribute pageModel, yourSubscriptions = NotAsked, yourSubscribedPlans = NotAsked, allCampaigns = NotAsked }
            in
            (newModel , Cmd.map ContributeMsg cmd)

        ( DashMsg subMsg, Dash subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Dash.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Dash.NoOp ->
                            { model | page = Dash pageModel }

                        Dash.MakeMainFetchCampaigns ->
                            { model
                                | page = Dash pageModel
                                , yourCampaigns = NotAsked
                                , allCampaigns = NotAsked
                                , campaignOfInterest = NotAsked
                                , repoOfInterest = NotAsked
                                , rewardsOfInterest = NotAsked
                            }
            in
            (newModel , Cmd.map DashMsg cmd)

        ( CreateRewardsMsg subMsg, CreateRewards subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    CreateRewards.update subMsg subModel

                newModel =
                    case msgFromPage of
                        CreateRewards.NoOp ->
                            { model | page = CreateRewards pageModel }

                        CreateRewards.MakeMainFetchCampaigns ->
                            { model
                                | page = CreateRewards pageModel
                                , yourCampaigns = NotAsked
                                , allCampaigns = NotAsked
                                , campaignOfInterest = NotAsked
                                , repoOfInterest = NotAsked
                                , rewardsOfInterest = NotAsked
                            }
            in
            (newModel , Cmd.map CreateRewardsMsg cmd)

        ( CreateUserRoleMsg subMsg, CreateUserRole subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    CreateUserRole.update subMsg subModel

                newModel =
                    case msgFromPage of
                        CreateUserRole.NoOp ->
                            model

                        CreateUserRole.UpdateUserWithRole user ->
                            let
                                currentUser =
                                    pageSession.user
                                        |> Maybe.withDefault User.default

                                updatedUser =
                                    { currentUser | role = user.role }

                                updatedModel =
                                    { model | session = { pageSession | user = Just updatedUser } }
                            in
                            updatedModel

                updatedCmdAndMsg =
                    case msgFromPage of
                        CreateUserRole.NoOp ->
                            Cmd.map CreateUserRoleMsg cmd

                        CreateUserRole.UpdateUserWithRole user ->
                            case user.role of
                                1 ->
                                    Router.modifyUrl model.key DiscoverRoute

                                2 ->
                                    Router.modifyUrl model.key CreateCampaignRoute

                                3 ->
                                    Router.modifyUrl model.key CreateCampaignRoute

                                _ ->
                                    Router.modifyUrl model.key DiscoverRoute
            in
            ({ newModel | page = CreateUserRole pageModel } , updatedCmdAndMsg)

        ( DiscoverMsg subMsg, Discover subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Discover.update subMsg subModel
            in
            ({ model | page = Discover pageModel } , Cmd.map DiscoverMsg cmd)

        ( CreateCampaignMsg subMsg, CreateCampaign subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    CreateCampaign.update subMsg subModel

                updatedCmd =
                    case pageSession.user of
                        Just user ->
                            case msgFromPage of
                                CreateCampaign.NoOp ->
                                    Cmd.map CreateCampaignMsg cmd

                                CreateCampaign.GoToStripeSignUp ->
                                    Router.modifyUrl model.key Router.StripeConnectSignUpRoute

                        Nothing ->
                            Router.modifyUrl model.key HomeRoute
            in
            ({ model | page = CreateCampaign pageModel } , updatedCmd)

        ( TosserSignUpMsg subMsg, TosserSignUp subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    TosserSignUp.update subMsg subModel

                newModel =
                    case msgFromPage of
                        TosserSignUp.NoOp ->
                            model

                        TosserSignUp.SetUser user ->
                            { model | session = { user = Just user } }
            in
            ({ newModel | page = TosserSignUp pageModel } , Cmd.map TosserSignUpMsg cmd)

        ( AboutMsg subMsg, About subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    About.update subMsg subModel

                newModel =
                    case msgFromPage of
                        About.NoOp ->
                            model
            in
            ({ model | page = About pageModel } , Cmd.map AboutMsg cmd)

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Login.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Login.NoOp ->
                            model

                        Login.SetUser user ->
                            let
                                session =
                                    model.session
                            in
                            { model | session = { user = Just user } }
            in
            ({ model | page = Login pageModel } , Cmd.map LoginMsg cmd)

        ( HandleMsg (HandleStripeIdUpdate data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                token =
                    user
                        |> Maybe.withDefault User.default
                        |> .token

                userId =
                    user
                        |> Maybe.withDefault User.default
                        |> .userId
            in
            ( model
            , Cmd.map HandleMsg (fetchYourCampaignsForRewards model.apiUrl token userId)
            )

        ( HandleMsg (HandleGithubUrl data), _ ) ->
            let
                updatedModel =
                    { model | githubUrl = data }
            in
            ( { updatedModel | page = Home { webDataUrl = data, apiUrl = model.apiUrl } }, Cmd.none )

        ( HandleMsg (HandleStripeConnectUrl data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token
            in
            ( { model | stripeConnectUrl = data }
            , Router.modifyUrl model.key StripeConnectSignUpRoute
            )

        ( HandleMsg (HandleFetchRepos data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                userId =
                    updatedUser.userId

                updatedModel =
                    { model | repos = data }

                repos =
                    case data of
                        Success matchedRepos ->
                            matchedRepos

                        _ ->
                            Repos.defaultSelectListRepos

                reposAsList =
                    repos
                        |> .selectListRepos
                        |> SelectList.toList

                bountifulRepo =
                    Repos.mostBountifulRepo reposAsList
            in
            ( updatedModel
            , Cmd.map HandleMsg (getIssues model.apiUrl token bountifulRepo.id)
            )

        ( HandleMsg (HandleFetchIssues data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                cmd =
                    -- If we just signed out, then redirect to Home.
                    if session.user /= Nothing && user == Nothing then
                        Router.modifyUrl model.key Router.HomeRoute

                    else
                        Router.modifyUrl model.key Router.CreateCampaignRoute

                updatedUser =
                    case user of
                        Just matchedUser ->
                            matchedUser

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                userId =
                    updatedUser.userId

                updatedPage =
                    CreateCampaign (CreateCampaign.init token userId model.repos model.apiUrl)
            in
            ( { model | page = updatedPage, mostBountifulIssues = data }
            , Cmd.batch [ cmd, Router.modifyUrl model.key Router.CreateCampaignRoute ]
            )

        ( HandleMsg (HandleFetchAllCampaigns data), _ ) ->
            ( { model | allCampaigns = data }, Router.modifyUrl model.key Router.DiscoverRoute )

        ( HandleMsg (HandleFetchCampaignsForRewards data), _ ) ->
            ( { model | yourCampaigns = data }, Router.modifyUrl model.key Router.CreateRewardsRoute )

        ( HandleMsg (HandleFetchYourCampaigns data), _ ) ->
            ( { model | yourCampaigns = data }, Router.modifyUrl model.key Router.DashRoute )

        ( GithubOopsMsg subMsg, GithubOops subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    GithubOops.update subMsg subModel
            in
            ({ model | page = GithubOops pageModel } , Cmd.map GithubOopsMsg cmd)

        ( ToggleMenu, _ ) ->
            let
                updatedMenuStatus =
                    not model.showMenu
            in
            ( { model | showMenu = updatedMenuStatus }, Cmd.none )

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            (model , Cmd.none)


burgerMenu : Model -> Html Msg
burgerMenu model =
    let
        burgerMenuClass =
            if model.showMenu then
                "button navbar-burger is-active"

            else
                "button navbar-burger"
    in
    button [ class burgerMenuClass, onClick ToggleMenu ]
        [ span [] []
        , span [] []
        , span [] []
        ]


burgerMenuNavItems : Model -> Html Msg
burgerMenuNavItems model =
    let
        pageSession =
            model.session

        user =
            pageSession.user
                |> Maybe.withDefault User.default
    in
    if pageSession.user == Nothing then
        div [ classList [ ( "navbar-menu", True ), ( "is-active", model.showMenu ) ] ]
            [ div [ class "navbar-end" ]
                [ a [ class "navbar-item", Router.href AboutRoute ]
                    [ text
                        "About"
                    ]
                ]
            ]

    else
        div [ classList [ ( "navbar-menu", True ), ( "is-active", model.showMenu ) ] ]
            [ div [ class "navbar-end" ]
                [ a [ class "navbar-item", Router.href DiscoverRoute ]
                    [ text
                        "Discover"
                    ]
                , a [ class "navbar-item", Router.href DashRoute ]
                    [ text
                        "Dash"
                    ]
                , a [ class "navbar-item", Router.href LogoutRoute ]
                    [ text
                        "Logout"
                    ]
                , p [ class "navbar-item" ]
                    [ text
                        ("Hello, "
                            ++ user.email
                        )
                    ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SetUser sessionChange
        , Sub.map ConsumeToken consumeToken
        ]


developerHeroArea : Html Msg
developerHeroArea =
    section [ class "hero" ]
        [ div [ class "hero-body", style "padding" "7rem 1.5rem" ]
            [ div [ class "container" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column has-text-centered" ]
                        [ p [ class "title" ]
                            [ text "For developers" ]
                        , p [ class "subtitle" ]
                            [ text "get paid with"
                            , strong [] [ text " no strings" ]
                            ]
                        , button [ class "button" ]
                            [ text "Start winning" ]
                        ]
                    ]
                ]
            ]
        ]


pageView : Model -> Document Msg
pageView model =
    let
        pageSession =
            model.session

        page =
            model.page

        updatedBurgerMenuNavItems =
            burgerMenuNavItems model
    in
        case page of
            Home subModel ->
                let
                    updatedView =
                        Home.view subModel
                            |> Html.map HomeMsg

                    updatedBurgerMenu =
                        burgerMenu model

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.Home updatedView updatedBurgerMenu
                in
                { title = "Home", body = [updatedFrame] }

            StripeConnectSignUp subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        StripeConnectSignUp.view subModel
                            |> Html.map StripeConnectSignUpMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.StripeConnectSignUp updatedView updatedBurgerMenu
                in
                { title = "Stripe Connect Sign Up", body = [updatedFrame] }

            TosserSignUp subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        TosserSignUp.view subModel
                            |> Html.map TosserSignUpMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.TosserSignUp updatedView updatedBurgerMenu
                in
                { title = "Tosser Sign Up", body = [updatedFrame] }

            Discover subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        Discover.view subModel
                            |> Html.map DiscoverMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.Discover updatedView updatedBurgerMenu
                in
                { title = "Discover", body = [updatedFrame] }

            Contribute subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        Contribute.view subModel
                            |> Html.map ContributeMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.Contribute updatedView updatedBurgerMenu
                in
                { title = "Contribute", body = [updatedFrame] }

            Dash subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        Dash.view subModel
                            |> Html.map DashMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.Dash updatedView updatedBurgerMenu
                in
                { title = "Dashboard", body = [updatedFrame] }

            About subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        About.view subModel
                            |> Html.map AboutMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.About updatedView updatedBurgerMenu
                in
                { title = "About", body = [updatedFrame] }

            Login subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        Login.view subModel
                            |> Html.map LoginMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.Login updatedView updatedBurgerMenu
                in
                { title = "Login", body = [updatedFrame] }

            CreateCampaign subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        CreateCampaign.view subModel
                            |> Html.map CreateCampaignMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.CreateCampaign updatedView updatedBurgerMenu
                in
                { title = "Create Campaign", body = [updatedFrame] }

            CreateRewards subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        CreateRewards.view subModel
                            |> Html.map CreateRewardsMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.CreateRewards updatedView updatedBurgerMenu
                in
                { title = "Create Rewards", body = [updatedFrame] }

            CreateUserRole subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        CreateUserRole.view subModel
                            |> Html.map CreateUserRoleMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.CreateUserRole updatedView updatedBurgerMenu
                in
                { title = "Create user role", body = [updatedFrame] }

            GithubOops subModel ->
                let
                    updatedBurgerMenu =
                        burgerMenu model

                    updatedView =
                        GithubOops.view subModel
                            |> Html.map GithubOopsMsg

                    updatedFrame =
                        Page.frame updatedBurgerMenuNavItems pageSession.user Page.GithubOops updatedView updatedBurgerMenu
                in
                { title = "Github Oops", body = [updatedFrame] }

            NotFound subModel ->
                let
                    updatedView =
                        NotFound.view
                            |> Html.map NotFoundMsg
                in
                { title = "Not Found", body = [updatedView]}


view : Model -> Document Msg
view model =
    pageView model


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


fetchYourCampaigns : Maybe String -> AuthToken -> String -> Cmd HandleMsg
fetchYourCampaigns apiUrl token userId =
    let
        campaignUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/campaigns?user_id=" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignUrl HandleFetchYourCampaigns Campaigns.decoder


fetchYourSubscriptions : Maybe String -> AuthToken -> String -> Cmd HandleMsg
fetchYourSubscriptions apiUrl token userId =
    let
        subscriptionUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/subscriptions?user_id=" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) subscriptionUrl HandleFetchYourSubscriptions Subscriptions.decoder


fetchYourSubscribedPlans : Maybe String -> AuthToken -> String -> Cmd HandleMsg
fetchYourSubscribedPlans apiUrl token userId =
    let
        planUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/plans?subscriber_id=" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) planUrl HandleFetchYourSubscribedPlans Plans.decoder


fetchYourCampaignsForRewards : Maybe String -> AuthToken -> String -> Cmd HandleMsg
fetchYourCampaignsForRewards apiUrl token userId =
    let
        campaignUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/campaigns?user_id=" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignUrl HandleFetchCampaignsForRewards Campaigns.decoder


getRepos : Maybe String -> AuthToken -> String -> Cmd HandleMsg
getRepos apiUrl token userId =
    let
        reposUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/github_repos?user_id=" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleFetchRepos Repos.selectListDecoder


getIssues : Maybe String -> AuthToken -> String -> Cmd HandleMsg
getIssues apiUrl token githubRepoId =
    let
        issuesUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/github_issues"

        issuesFilteredByRepoId =
            issuesUrl ++ "?github_repo_id=" ++ githubRepoId
    in
    RemoteData.Http.getWithConfig (Auth.config token) issuesFilteredByRepoId HandleFetchIssues Issues.decoder


getGitHubSignInUrl : Maybe String -> Cmd HandleMsg
getGitHubSignInUrl apiUrl =
    let
        github_oauth_url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/github_oauth_url"
    in
    RemoteData.Http.get github_oauth_url HandleGithubUrl githubUrlDecoder


githubUrlDecoder : Decoder GitHubUrl
githubUrlDecoder =
    succeed GitHubUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""


updateUserWithStripeInfo : Maybe String -> AuthToken -> String -> String -> Cmd HandleMsg
updateUserWithStripeInfo apiUrl token stripeExternalId userId =
    let
        usersUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/users/" ++ userId

        data =
            { stripeExternalId = stripeExternalId
            , stripeAccessToken = ""
            }
    in
    RemoteData.Http.patchWithConfig (Auth.config token) usersUrl HandleStripeIdUpdate User.decoder (User.encodeStripeUserUpdate data)


getStripeConnectUrl : Maybe String -> Cmd HandleMsg
getStripeConnectUrl apiUrl =
    let
        stripeConnectUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/stripe_oauth_url"
    in
    RemoteData.Http.get stripeConnectUrl HandleStripeConnectUrl stripeConnectUrlUrlDecoder


stripeConnectUrlUrlDecoder : Decoder StripeConnectUrl
stripeConnectUrlUrlDecoder =
    succeed StripeConnectUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""


consumeToken : Sub (Maybe Stripe)
consumeToken =
    Ports.consumeToken (Decode.decodeValue Stripe.decoder >> Result.toMaybe)


getRewards : String -> AuthToken -> Int -> Cmd HandleMsg
getRewards apiUrl token campaignId =
    let
        updatedUrl =
            apiUrl ++ "/rewards/?campaign_id=" ++ (campaignId |> String.fromInt)
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchRewards Rewards.decoder


getCampaign : String -> AuthToken -> String -> Cmd HandleMsg
getCampaign apiUrl token campaignId =
    let
        campaignUrl =
            apiUrl ++ "/campaigns/" ++ campaignId
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignUrl HandleFetchCampaign Campaign.showDecoder


getCampaigns : Maybe String -> AuthToken -> Cmd HandleMsg
getCampaigns apiUrl token =
    let
        updatedApiUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url

        page_size =
            4

        page =
            1

        campaignsUrl =
            updatedApiUrl ++ "/campaigns" ++ "?page_size=" ++ String.fromInt page_size ++ "&page=" ++ String.fromInt page
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignsUrl HandleFetchAllCampaigns Campaigns.decoder


getRepo : String -> AuthToken -> String -> Cmd HandleMsg
getRepo apiUrl token githubRepoId =
    let
        reposUrl =
            apiUrl ++ "/github_repos/" ++ githubRepoId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleFetchRepo Repo.decoder


getUser : Maybe String -> AuthToken -> String -> Cmd HandleMsg
getUser apiUrl token userId =
    let
        updatedApiUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url

        userUrl =
            updatedApiUrl ++ "/users/" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) userUrl HandleFetchUser User.decoder
