module Main exposing (..)

import Data.AuthToken exposing (AuthToken, fallback, init)
import Data.Campaign as Campaign exposing (..)
import Data.Campaigns as Campaigns exposing (Campaigns, decoder)
import Data.Issues as Issues exposing (Issues)
import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, mostBountifulRepo)
import Data.Rewards as Rewards exposing (Rewards)
import Data.Session as Session exposing (Session)
import Data.Stripe as Stripe exposing (Stripe, decoder)
import Data.StripeConnectUrl as StripeConnectUrl exposing (StripeConnectUrl)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)
import Navigation exposing (Location)
import Pages.BetaSignUp as BetaSignUp
import Pages.Contribute as Contribute
import Pages.CreateCampaign as CreateCampaign
import Pages.CreateRewards as CreateRewards
import Pages.Dash as Dash
import Pages.Discover as Discover
import Pages.Home as Home exposing (GitHubUrl)
import Pages.Login as Login
import Pages.Logout as Logout
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
    | BetaSignUp BetaSignUp.Model
    | StripeConnectSignUp StripeConnectSignUp.Model
    | CreateCampaign CreateCampaign.Model
    | CreateRewards CreateRewards.Model
    | Logout Logout.Model
    | Discover Discover.Model
    | NotFound NotFound.Model


type alias Model =
    { session : Session
    , location : Location
    , page : Page
    , githubUrl : WebData GitHubUrl
    , apiUrl : Maybe String
    , yourCampaigns : WebData Campaigns
    , campaignsContributedTo : List Campaign
    , allCampaigns : WebData Campaigns
    , repos : WebData Repos
    , mostBountifulIssues : WebData Issues
    , stripeConnectUrl : WebData StripeConnectUrl
    , stripe : Maybe Stripe
    , campaignOfInterest : WebData Campaign
    , repoOfInterest : WebData Repo
    , rewardsOfInterest : WebData Rewards
    }


type Msg
    = HomeMsg Home.Msg
    | TosserSignUpMsg TosserSignUp.Msg
    | ContributeMsg Contribute.Msg
    | DiscoverMsg Discover.Msg
    | DashMsg Dash.Msg
    | LoginMsg Login.Msg
    | BetaSignUpMsg BetaSignUp.Msg
    | CreateCampaignMsg CreateCampaign.Msg
    | CreateRewardsMsg CreateRewards.Msg
    | StripeConnectSignUpMsg StripeConnectSignUp.Msg
    | LogoutMsg Logout.Msg
    | NotFoundMsg NotFound.Msg
    | SetUser (Maybe User)
    | ConsumeToken (Maybe Stripe)
    | LoginUser String String
    | SetRoute (Maybe Route)
    | JoinChannel
    | ShowJoinedMessage String
    | ShowLeftMessage String
    | ReceiveMessage Encode.Value
    | FetchGitHubUrl (WebData GitHubUrl)
    | HandleFetchCampaigns (WebData Campaigns)
    | HandleFetchCampaignsForRewards (WebData Campaigns)
    | FetchRepos (WebData Repos)
    | HandleFetchIssues (WebData Issues)
    | FetchStripeConnectUrl (WebData StripeConnectUrl)
    | HandleStripeIdUpdate (WebData User)
    | HandleFetchAllCampaigns (WebData Campaigns)
    | HandleFetchRewards (WebData Rewards)
    | HandleFetchCampaign (WebData Campaign)
    | HandleFetchRepo (WebData Repo)


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
        , yourCampaigns = Loading
        , campaignsContributedTo = []
        , repos = Loading
        , mostBountifulIssues = Loading
        , stripeConnectUrl = Loading
        , allCampaigns = NotAsked
        , stripe = Nothing
        , campaignOfInterest = NotAsked
        , repoOfInterest = NotAsked
        , rewardsOfInterest = NotAsked
        }


decodeUrlFromJson : Decode.Value -> Maybe String
decodeUrlFromJson json =
    json
        |> Decode.decodeValue (Decode.field "apiUrl" Decode.string)
        |> Result.toMaybe


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Just Router.BetaSignUpRoute ->
            let
                updatedPage =
                    Home Home.init
            in
            { model | page = updatedPage } => Cmd.none

        Just Router.HomeRoute ->
            model => Cmd.batch [ getGitHubSignInUrl model.apiUrl ]

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
                    }

                session =
                    model.session

                repos =
                    case model.repos of
                        Success repos ->
                            repos

                        _ ->
                            Repos []

                bountifulRepo =
                    Repos.mostBountifulRepo repos

                updatedModel =
                    { model | session = { session | user = Just user } }
            in
            updatedModel
                => Cmd.batch
                    [ Request.User.storeSession user
                    , getStripeConnectUrl model.apiUrl
                    ]

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
                            if toString campaignId == campaign.id then
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

                                    _ =
                                        Debug.log "rewards" rewards

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
                                model => getCampaign apiUrl token (toString campaignId)

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
                            model => getCampaign apiUrl token (toString campaignId)

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
                    ( model, updateUserWithStripeInfo model.apiUrl token stripeId user.userId )

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

                        campaignsContributedTo =
                            model.campaignsContributedTo

                        updatedYourCampaigns =
                            case model.yourCampaigns of
                                Success yourCampaigns ->
                                    yourCampaigns

                                _ ->
                                    Campaigns []

                        updatedModelAndCmd =
                            case List.length updatedYourCampaigns.campaigns of
                                0 ->
                                    model => fetchYourCampaigns model.apiUrl user.token user.userId

                                _ ->
                                    { model | page = Dash (Dash.init model.apiUrl token updatedYourCampaigns.campaigns campaignsContributedTo) } => Cmd.none
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

                        updatedPage =
                            Discover (Discover.init token model.apiUrl)
                    in
                    { model | page = updatedPage } => Cmd.none

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
                                    Campaigns []

                        campaign =
                            case List.head yourCampaigns.campaigns of
                                Just campaign ->
                                    campaign

                                Nothing ->
                                    Campaign "0" 0.0 "" "" 0.0 (DateTime.dateTime DateTime.zero) "0" "0"

                        updatedPage =
                            CreateRewards (CreateRewards.init apiUrl token campaign.id)
                    in
                    { model | page = updatedPage } => Cmd.none

                Nothing ->
                    model => Router.modifyUrl Router.HomeRoute

        Just Router.NotFoundRoute ->
            model => Cmd.none

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
        ( HandleFetchRepo data, _ ) ->
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
                            User "" (Data.AuthToken.init "") "" "" ""

                token =
                    updatedUser.token

                campaignId =
                    case model.campaignOfInterest of
                        Success campaign ->
                            campaign.id

                        _ ->
                            "0"

                apiUrl =
                    case model.apiUrl of
                        Nothing ->
                            ""

                        Just url ->
                            url
            in
            ( { model | repoOfInterest = data }
            , getRewards apiUrl token campaignId
            )

        ( HandleFetchCampaign data, _ ) ->
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
                            User "" (Data.AuthToken.init "") "" "" ""

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
            , getRepo apiUrl token githubRepoId
            )

        ( HandleFetchRewards updatedRewards, _ ) ->
            let
                _ =
                    Debug.log "updatedRewards" updatedRewards

                campaignId =
                    case model.campaignOfInterest of
                        Success campaign ->
                            campaign.id

                        _ ->
                            "0"

                campaignIdAsInt =
                    case String.toInt campaignId of
                        Ok id ->
                            id

                        Err message ->
                            0
            in
            ( { model | rewardsOfInterest = updatedRewards }, Router.modifyUrl (ContributeRoute campaignIdAsInt) )

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
            in
            { model | page = Contribute pageModel } => Cmd.map ContributeMsg cmd

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

                                CreateCampaign.FetchCampaigns ->
                                    fetchYourCampaignsForRewards model.apiUrl user.token user.userId

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

        ( BetaSignUpMsg subMsg, BetaSignUp subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    BetaSignUp.update subMsg subModel

                newModel =
                    case msgFromPage of
                        BetaSignUp.NoOp ->
                            model
            in
            { model | page = BetaSignUp pageModel } => Cmd.map BetaSignUpMsg cmd

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

        ( LogoutMsg subMsg, Logout subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Logout.update subMsg subModel

                newModel =
                    case msgFromPage of
                        Logout.NoOp ->
                            model

                        Logout.SetUser user ->
                            let
                                session =
                                    model.session
                            in
                            { model | session = { user = Just user } }
            in
            { model | page = Logout pageModel } => Cmd.map LogoutMsg cmd

        ( HandleStripeIdUpdate data, _ ) ->
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
                            User "" (Data.AuthToken.init "") "" "" ""

                token =
                    updatedUser.token

                userId =
                    updatedUser.userId
            in
            ( model, Router.modifyUrl Router.DashRoute )

        ( FetchGitHubUrl data, _ ) ->
            let
                updatedModel =
                    { model | githubUrl = data }
            in
            ( { updatedModel | page = Home { url = data } }, Cmd.none )

        ( FetchStripeConnectUrl data, _ ) ->
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
                            User "" (Data.AuthToken.init "") "" "" ""

                token =
                    updatedUser.token

                updatedModel =
                    { model | stripeConnectUrl = data }
            in
            ( { updatedModel | page = StripeConnectSignUp { url = data } }
            , getRepos model.apiUrl token
            )

        ( FetchRepos data, _ ) ->
            let
                session =
                    model.session

                cmd =
                    -- If we just signed out, then redirect to Home.
                    if session.user /= Nothing && user == Nothing then
                        Router.modifyUrl Router.HomeRoute
                    else
                        Cmd.none

                user =
                    session.user

                updatedUser =
                    case user of
                        Just user ->
                            user

                        Nothing ->
                            User "" (Data.AuthToken.init "") "" "" ""

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
            , getIssues model.apiUrl token bountifulRepo.id
            )

        ( HandleFetchIssues data, _ ) ->
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
                            User "" (Data.AuthToken.init "") "" "" ""

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

        ( HandleFetchAllCampaigns data, _ ) ->
            ( { model | allCampaigns = data }, Router.modifyUrl Router.DiscoverRoute )

        ( HandleFetchCampaignsForRewards data, _ ) ->
            ( { model | yourCampaigns = data }, Router.modifyUrl Router.CreateRewardsRoute )

        ( HandleFetchCampaigns data, _ ) ->
            ( { model | yourCampaigns = data }, Router.modifyUrl Router.DashRoute )

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

            BetaSignUp subModel ->
                BetaSignUp.view subModel
                    |> frame Page.BetaSignUp
                    |> Html.map BetaSignUpMsg

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

            Logout subModel ->
                Logout.view subModel
                    |> Html.map LogoutMsg

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


fetchYourCampaigns : Maybe String -> AuthToken -> String -> Cmd Msg
fetchYourCampaigns apiUrl token userId =
    let
        campaignUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/campaigns?user_id=" ++ userId
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignUrl HandleFetchCampaigns Campaigns.decoder


fetchYourCampaignsForRewards : Maybe String -> AuthToken -> String -> Cmd Msg
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


getRepos : Maybe String -> AuthToken -> Cmd Msg
getRepos apiUrl token =
    let
        reposUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/github_repos"
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl FetchRepos Repos.decoder


getIssues : Maybe String -> AuthToken -> String -> Cmd Msg
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


getGitHubSignInUrl : Maybe String -> Cmd Msg
getGitHubSignInUrl apiUrl =
    let
        github_oauth_url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/github_oauth_url"
    in
    RemoteData.Http.get github_oauth_url FetchGitHubUrl githubUrlDecoder


githubUrlDecoder : Decoder GitHubUrl
githubUrlDecoder =
    decode GitHubUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""


fetchAllCampaigns : Maybe String -> AuthToken -> Cmd Msg
fetchAllCampaigns apiUrl token =
    let
        updatedUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchAllCampaigns Campaigns.decoder


updateUserWithStripeInfo : Maybe String -> AuthToken -> String -> String -> Cmd Msg
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


getStripeConnectUrl : Maybe String -> Cmd Msg
getStripeConnectUrl apiUrl =
    let
        stripeConnectUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/stripe_oauth_url"
    in
    RemoteData.Http.get stripeConnectUrl FetchStripeConnectUrl stripeConnectUrlUrlDecoder


stripeConnectUrlUrlDecoder : Decoder StripeConnectUrl
stripeConnectUrlUrlDecoder =
    decode StripeConnectUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""


consumeToken : Sub (Maybe Stripe)
consumeToken =
    Ports.consumeToken (Decode.decodeValue Stripe.decoder >> Result.toMaybe)


getRewards : String -> AuthToken -> String -> Cmd Msg
getRewards apiUrl token campaignId =
    let
        updatedUrl =
            apiUrl ++ "/rewards/?campaign_id=" ++ campaignId
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchRewards Rewards.decoder


getCampaign : String -> AuthToken -> String -> Cmd Msg
getCampaign apiUrl token campaignId =
    let
        campaignUrl =
            apiUrl ++ "/campaigns/" ++ campaignId
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignUrl HandleFetchCampaign Campaign.showDecoder


getRepo : String -> AuthToken -> String -> Cmd Msg
getRepo apiUrl token githubRepoId =
    let
        reposUrl =
            apiUrl ++ "/github_repos/" ++ githubRepoId
    in
    RemoteData.Http.getWithConfig (Auth.config token) reposUrl HandleFetchRepo Repo.decoder
