module Main exposing (..)

import Data.AuthToken exposing (AuthToken, fallback, init)
import Data.Campaign as Campaign exposing (..)
import Data.Campaigns as Campaigns exposing (Campaigns, IncludedRepo, IncludedStuff(..), decoder)
import Data.Issues as Issues exposing (Issues)
import Data.Plans as Plans exposing (Plans, decoder)
import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, mostBountifulRepo)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Session as Session exposing (Session)
import Data.Stripe as Stripe exposing (Stripe, decoder)
import Data.StripeConnectUrl as StripeConnectUrl exposing (StripeConnectUrl)
import Data.Subscriptions as Subscriptions exposing (Subscriptions, decoder)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, optionalAt, requiredAt)
import Navigation exposing (Location)
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
import Time.DateTime as DateTime exposing (DateTime)
import Util exposing ((=>))
import Views.Page as Page exposing (frame)


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
    , location : Location
    , page : Page
    , githubUrl : WebData GitHubUrl
    , apiUrl : Maybe String
    , yourCampaigns : WebData Campaigns
    , yourSubscriptions : WebData Subscriptions
    , yourSubscribedPlans : WebData Plans
    , repos : WebData Repos
    , mostBountifulIssues : WebData Issues
    , stripeConnectUrl : WebData StripeConnectUrl
    , stripe : Maybe Stripe
    , campaignOfInterest : WebData Campaign
    , repoOfInterest : WebData Repo
    , rewardsOfInterest : WebData Rewards
    , allCampaigns : WebData Campaigns
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


type HandleMsg
    = HandleGithubUrl (WebData GitHubUrl)
    | HandleFetchYourCampaigns (WebData Campaigns)
    | HandleFetchCampaignsForRewards (WebData Campaigns)
    | HandleFetchRepos (WebData Repos)
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


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Router.fromLocation location)
        { session = { user = decodeUserFromJson val }
        , location = location
        , page = Home Home.init
        , githubUrl = Loading
        , apiUrl = decodeUrlFromJson val
        , yourCampaigns = NotAsked
        , yourSubscriptions = NotAsked
        , yourSubscribedPlans = NotAsked
        , repos = Loading
        , mostBountifulIssues = Loading
        , stripeConnectUrl = Loading
        , stripe = Nothing
        , campaignOfInterest = NotAsked
        , repoOfInterest = NotAsked
        , rewardsOfInterest = NotAsked
        , allCampaigns = NotAsked
        }


decodeUrlFromJson : Decode.Value -> Maybe String
decodeUrlFromJson json =
    json
        |> Decode.decodeValue (Decode.field "apiUrl" Decode.string)
        |> Result.toMaybe


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                HomeRoute ->
                    [ "home" ]

                StripeConnectSignUpRoute ->
                    [ "stripe-connect-sign-up" ]

                SaveStripeRoute (Just stripeId) ->
                    [ "save-stripe" ++ "?stripe_id" ++ stripeId ]

                SaveStripeRoute _ ->
                    [ "save-stripe" ]

                AboutRoute ->
                    [ "about" ]

                TosserSignUpRoute ->
                    [ "tosser-sign-up" ]

                DashRoute ->
                    [ "dash" ]

                CreateCampaignRoute ->
                    [ "create-campaign" ]

                CreateRewardsRoute ->
                    [ "create-rewards" ]

                LoginRoute ->
                    [ "login" ]

                LogoutRoute ->
                    [ "logout" ]

                SaveTokenRoute (Just token) _ _ ->
                    [ "save-session" ++ "?token" ++ token ]

                SaveTokenRoute _ _ _ ->
                    [ "save-session" ]

                DiscoverRoute ->
                    [ "discover" ]

                ContributeRoute campaignId ->
                    [ "contribute/" ++ toString campaignId ]

                CreateUserRoleRoute ->
                    [ "get-user-type" ]

                GithubOopsRoute ->
                    [ "github-oops" ]

                NotFoundRoute ->
                    []
    in
    "#/" ++ String.join "/" pieces


modifyUrl : Route -> Cmd Msg
modifyUrl page =
    routeToString page
        |> Navigation.modifyUrl
        |> Cmd.map HandleMsg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Just Router.AboutRoute ->
            let
                updatedPage =
                    About About.init
            in
            { model | page = updatedPage } => Cmd.none

        Just Router.HomeRoute ->
            case model.session.user of
                Nothing ->
                    model => Cmd.map HandleMsg (getGitHubSignInUrl model.apiUrl)

                _ ->
                    model => Router.modifyUrl Router.DiscoverRoute

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
            updatedModel
                => Cmd.map HandleMsg (getUser model.apiUrl updatedToken userId)

        Just (Router.SaveTokenRoute _ _ _) ->
            model
                => Cmd.batch
                    [ Router.modifyUrl Router.DashRoute
                    ]

        Just Router.LoginRoute ->
            let
                updatedPage =
                    Login Login.init
            in
            { model | page = updatedPage } => Cmd.none

        Just Router.LogoutRoute ->
            let
                session =
                    model.session
            in
            { model | session = { session | user = Nothing } }
                => Cmd.batch
                    [ Ports.storeSession Nothing
                    , Router.modifyUrl Router.HomeRoute
                    ]

        Just (Router.ContributeRoute campaignId) ->
            case model.session.user of
                Just user ->
                    case model.campaignOfInterest of
                        Success campaign ->
                            if campaignId == campaign.id then
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
                                        Contribute (Contribute.init token apiUrl campaign repo rewards user)
                                in
                                { model | page = updatedPage } => Cmd.none
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
                                model => Cmd.map HandleMsg (getCampaign apiUrl token (toString campaignId))

                        Loading ->
                            model => Cmd.none

                        Failure _ ->
                            model => Cmd.none

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
                            model => Cmd.map HandleMsg (getCampaign apiUrl token (toString campaignId))

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

        Just Router.TosserSignUpRoute ->
            let
                updatedPage =
                    TosserSignUp TosserSignUp.init
            in
            { model | page = updatedPage } => Cmd.none

        Just Router.StripeConnectSignUpRoute ->
            let
                updatedPage =
                    StripeConnectSignUp (StripeConnectSignUp.init model.stripeConnectUrl)
            in
            { model | page = updatedPage } => Cmd.none

        Just (Router.SaveStripeRoute (Just stripeId)) ->
            case model.session.user of
                Just user ->
                    let
                        session =
                            model.session

                        token =
                            user.token
                    in
                    model => Cmd.map HandleMsg (updateUserWithStripeInfo model.apiUrl token stripeId user.userId)

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

        Just (Router.SaveStripeRoute Nothing) ->
            model
                => Cmd.batch
                    [ Router.modifyUrl Router.HomeRoute
                    ]

        Just Router.DashRoute ->
            case model.session.user of
                Just user ->
                    let
                        token =
                            user.token

                        updatedModelAndCmd =
                            case model.yourCampaigns of
                                NotAsked ->
                                    model => Cmd.map HandleMsg (fetchYourCampaigns model.apiUrl user.token user.userId)

                                Loading ->
                                    model => Cmd.none

                                Failure error ->
                                    let
                                        _ =
                                            Debug.log "campaign error" error
                                    in
                                    model => Cmd.none

                                Success campaignsData ->
                                    case model.yourSubscriptions of
                                        NotAsked ->
                                            model => Cmd.map HandleMsg (fetchYourSubscriptions model.apiUrl user.token user.userId)

                                        Loading ->
                                            model => Cmd.none

                                        Failure error ->
                                            let
                                                _ =
                                                    Debug.log "subscription error" error
                                            in
                                            model => Cmd.none

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
                                                    model => Cmd.map HandleMsg (fetchYourSubscribedPlans model.apiUrl user.token user.userId)

                                                Loading ->
                                                    model => Cmd.none

                                                Failure error ->
                                                    let
                                                        _ =
                                                            Debug.log "plan error" error
                                                    in
                                                    model => Cmd.none

                                                Success plansData ->
                                                    { model | page = Dash (Dash.init model.apiUrl token campaignsData.campaigns repos subscriptionsData.subscriptions plansData.plans) } => Cmd.none
                    in
                    updatedModelAndCmd

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

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
                                    let
                                        _ =
                                            Debug.log "error" error
                                    in
                                    Cmd.none

                                Loading ->
                                    Cmd.none

                                NotAsked ->
                                    Cmd.map HandleMsg (getCampaigns apiUrl token)

                        updatedPage =
                            Discover (Discover.init token apiUrl model.allCampaigns)
                    in
                    { model | page = updatedPage } => cmd

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

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

                        repos =
                            case model.repos of
                                Success repos ->
                                    repos

                                _ ->
                                    Repos []

                        updatedPage =
                            CreateCampaign (CreateCampaign.init token userId repos model.apiUrl)
                    in
                    { model | page = updatedPage } => Cmd.none

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

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

                        yourCampaigns =
                            case model.yourCampaigns of
                                Success yourCampaigns ->
                                    yourCampaigns

                                _ ->
                                    Campaigns.default

                        campaignsSorted =
                            yourCampaigns.campaigns
                                |> List.sortBy .id

                        campaign =
                            campaignsSorted
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault Campaign.default

                        _ =
                            Debug.log "campaign" campaign

                        updatedPage =
                            CreateRewards (CreateRewards.init apiUrl token campaign.id)
                    in
                    { model | page = updatedPage } => Cmd.none

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

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
                    { model | page = updatedPage } => Cmd.none

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

        Just Router.NotFoundRoute ->
            model => Cmd.none

        Just Router.GithubOopsRoute ->
            let
                updatedPage =
                    GithubOops GithubOops.init
            in
            { model | page = updatedPage } => Cmd.none

        Nothing ->
            model => Cmd.none


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        ( HandleMsg (HandleFetchUser data), _ ) ->
            case data of
                Success user ->
                    let
                        currentUser =
                            model.session.user
                                |> Maybe.withDefault User.default

                        updatedUser =
                            { currentUser | role = user.role }

                        updatedModel =
                            { model | session = { session | user = Just updatedUser } }
                    in
                    case user.role of
                        0 ->
                            updatedModel => Cmd.batch [ Request.User.storeSession updatedUser, Router.modifyUrl CreateUserRoleRoute ]

                        _ ->
                            updatedModel => Cmd.batch [ Request.User.storeSession updatedUser, Router.modifyUrl DiscoverRoute ]

                _ ->
                    model => Cmd.batch [ Router.modifyUrl HomeRoute ]

        ( HandleMsg (HandleFetchRepo data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

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
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

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
            , modifyUrl DashRoute
            )

        ( HandleMsg (HandleFetchYourSubscriptions data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

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
            , modifyUrl DashRoute
            )

        ( HandleMsg (HandleFetchCampaign data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

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
            ( { model | rewardsOfInterest = updatedRewards }, modifyUrl (ContributeRoute campaignId) )

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
            { model | stripe = stripe, page = Contribute newSubModel } => Cmd.map ContributeMsg cmdFromPage

        ( SetRoute route, _ ) ->
            setRoute route model

        ( SetUser user, _ ) ->
            let
                session =
                    model.session

                cmd =
                    -- If we just signed out, then redirect to Home.
                    if session.user /= Nothing && user == Nothing then
                        Router.modifyUrl Router.HomeRoute
                    else
                        Cmd.none
            in
            { model | session = { session | user = user } }
                => cmd

        ( HomeMsg subMsg, Home subModel ) ->
            let
                newSubModel =
                    { subModel | url = model.githubUrl }

                ( ( pageModel, cmd ), msgFromPage ) =
                    Home.update subMsg newSubModel
            in
            { model | page = Home pageModel } => Cmd.map HomeMsg cmd

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
            newModel => Cmd.map ContributeMsg cmd

        ( DashMsg subMsg, Dash subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Dash.update subMsg subModel
            in
            { model | page = Dash pageModel } => Cmd.map DashMsg cmd

        ( CreateRewardsMsg subMsg, CreateRewards subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    CreateRewards.update subMsg subModel
            in
            { model | page = CreateRewards pageModel } => Cmd.map CreateRewardsMsg cmd

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
                                    model.session.user
                                        |> Maybe.withDefault User.default

                                updatedUser =
                                    { currentUser | role = user.role }

                                updatedModel =
                                    { model | session = { session | user = Just updatedUser } }
                            in
                            updatedModel

                updatedCmdAndMsg =
                    case msgFromPage of
                        CreateUserRole.NoOp ->
                            Cmd.map CreateUserRoleMsg cmd

                        CreateUserRole.UpdateUserWithRole user ->
                            case user.role of
                                1 ->
                                    modifyUrl DiscoverRoute

                                2 ->
                                    Cmd.map HandleMsg (getStripeConnectUrl model.apiUrl)

                                3 ->
                                    Cmd.map HandleMsg (getStripeConnectUrl model.apiUrl)

                                _ ->
                                    modifyUrl DiscoverRoute
            in
            { newModel | page = CreateUserRole pageModel } => updatedCmdAndMsg

        ( DiscoverMsg subMsg, Discover subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Discover.update subMsg subModel
            in
            { model | page = Discover pageModel } => Cmd.map DiscoverMsg cmd

        ( CreateCampaignMsg subMsg, CreateCampaign subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    CreateCampaign.update subMsg subModel

                updatedCmd =
                    case model.session.user of
                        Just user ->
                            case msgFromPage of
                                CreateCampaign.NoOp ->
                                    Cmd.map CreateCampaignMsg cmd

                                CreateCampaign.GoToStripeSignUp ->
                                    Router.modifyUrl Router.StripeConnectSignUpRoute

                        Nothing ->
                            Router.modifyUrl HomeRoute
            in
            { model | page = CreateCampaign pageModel } => updatedCmd

        ( TosserSignUpMsg subMsg, TosserSignUp subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    TosserSignUp.update subMsg subModel

                newModel =
                    case msgFromPage of
                        TosserSignUp.NoOp ->
                            model

                        TosserSignUp.SetUser user ->
                            let
                                session =
                                    model.session
                            in
                            { model | session = { user = Just user } }
            in
            { newModel | page = TosserSignUp pageModel } => Cmd.map TosserSignUpMsg cmd

        ( AboutMsg subMsg, About subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    About.update subMsg subModel

                newModel =
                    case msgFromPage of
                        About.NoOp ->
                            model
            in
            { model | page = About pageModel } => Cmd.map AboutMsg cmd

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
            { model | page = Login pageModel } => Cmd.map LoginMsg cmd

        ( HandleMsg (HandleStripeIdUpdate data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                userId =
                    updatedUser.userId
            in
            ( model
            , Cmd.map HandleMsg (fetchYourCampaignsForRewards model.apiUrl token userId)
            )

        ( HandleMsg (HandleGithubUrl data), _ ) ->
            let
                updatedModel =
                    { model | githubUrl = data }
            in
            ( { updatedModel | page = Home { url = data } }, Cmd.none )

        ( HandleMsg (HandleStripeConnectUrl data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                updatedModel =
                    { model | stripeConnectUrl = data }
            in
            ( { updatedModel | page = StripeConnectSignUp { url = data } }
            , Cmd.map HandleMsg (getRepos model.apiUrl token updatedUser.userId)
            )

        ( HandleMsg (HandleFetchRepos data), _ ) ->
            let
                session =
                    model.session

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

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
                        Success repos ->
                            repos

                        _ ->
                            Repos []

                bountifulRepo =
                    Repos.mostBountifulRepo repos
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
                        Router.modifyUrl Router.HomeRoute
                    else
                        Router.modifyUrl Router.CreateCampaignRoute

                updatedUser =
                    case user of
                        Just user ->
                            user

                        Nothing ->
                            User.default

                token =
                    updatedUser.token

                userId =
                    updatedUser.userId

                repos =
                    case model.repos of
                        Success repos ->
                            repos

                        _ ->
                            Repos []

                updatedPage =
                    CreateCampaign (CreateCampaign.init token userId repos model.apiUrl)
            in
            ( { model | page = updatedPage, mostBountifulIssues = data }
            , Cmd.batch [ cmd, Router.modifyUrl Router.CreateCampaignRoute ]
            )

        ( HandleMsg (HandleFetchAllCampaigns data), _ ) ->
            ( { model | allCampaigns = data }, Router.modifyUrl Router.DiscoverRoute )

        ( HandleMsg (HandleFetchCampaignsForRewards data), _ ) ->
            ( { model | yourCampaigns = data }, Router.modifyUrl Router.CreateRewardsRoute )

        ( HandleMsg (HandleFetchYourCampaigns data), _ ) ->
            ( { model | yourCampaigns = data }, Router.modifyUrl Router.DashRoute )

        ( GithubOopsMsg subMsg, GithubOops subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    GithubOops.update subMsg subModel
            in
            { model | page = GithubOops pageModel } => Cmd.map GithubOopsMsg cmd

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model => Cmd.none


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
        [ div [ class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
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


pageView : Session -> Page -> Html Msg
pageView session page =
    let
        frame =
            Page.frame session.user
    in
    div []
        [ case page of
            Home subModel ->
                Home.view subModel
                    |> frame Page.Home
                    |> Html.map HomeMsg

            StripeConnectSignUp subModel ->
                StripeConnectSignUp.view subModel
                    |> frame Page.StripeConnectSignUp
                    |> Html.map StripeConnectSignUpMsg

            TosserSignUp subModel ->
                TosserSignUp.view subModel
                    |> frame Page.TosserSignUp
                    |> Html.map TosserSignUpMsg

            Discover subModel ->
                Discover.view subModel
                    |> frame Page.Discover
                    |> Html.map DiscoverMsg

            Contribute subModel ->
                Contribute.view subModel
                    |> frame Page.Contribute
                    |> Html.map ContributeMsg

            Dash subModel ->
                Dash.view subModel
                    |> frame Page.Dash
                    |> Html.map DashMsg

            About subModel ->
                About.view subModel
                    |> frame Page.About
                    |> Html.map AboutMsg

            Login subModel ->
                Login.view subModel
                    |> frame Page.Login
                    |> Html.map LoginMsg

            CreateCampaign subModel ->
                CreateCampaign.view subModel
                    |> frame Page.CreateCampaign
                    |> Html.map CreateCampaignMsg

            CreateRewards subModel ->
                CreateRewards.view subModel
                    |> frame Page.CreateRewards
                    |> Html.map CreateRewardsMsg

            CreateUserRole subModel ->
                CreateUserRole.view subModel
                    |> frame Page.CreateUserRole
                    |> Html.map CreateUserRoleMsg

            GithubOops subModel ->
                GithubOops.view subModel
                    |> frame Page.GithubOops
                    |> Html.map GithubOopsMsg

            NotFound subModel ->
                NotFound.view
                    |> Html.map NotFoundMsg
        ]


view : Model -> Html Msg
view model =
    div []
        [ pageView model.session model.page ]


main : Program Decode.Value Model Msg
main =
    Navigation.programWithFlags (Router.fromLocation >> SetRoute)
        { init = init
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
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleFetchRepos Repos.decoder


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
    decode GitHubUrl
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
    decode StripeConnectUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""


consumeToken : Sub (Maybe Stripe)
consumeToken =
    Ports.consumeToken (Decode.decodeValue Stripe.decoder >> Result.toMaybe)


getRewards : String -> AuthToken -> Int -> Cmd HandleMsg
getRewards apiUrl token campaignId =
    let
        updatedUrl =
            apiUrl ++ "/rewards/?campaign_id=" ++ (campaignId |> toString)
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

        page_size = 4
        page = 1

        campaignsUrl =
            updatedApiUrl ++ "/campaigns" ++ "?page_size=" ++ ( toString page_size ) ++ "&page=" ++ ( toString page )
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
