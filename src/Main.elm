module Main exposing (..)

import Data.AuthToken exposing (AuthToken, fallback, init)
import Data.Campaign as Campaign exposing (..)
import Data.Issues as Issues exposing (Issues)
import Data.Repos as Repos exposing (Repos, mostBountifulRepo)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Http exposing (header)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)
import Navigation exposing (Location)
import Pages.BetaSignUp as BetaSignUp
import Pages.CreateCampaign as CreateCampaign
import Pages.Dash as Dash
import Pages.Home as Home exposing (GitHubUrl)
import Pages.Login as Login
import Pages.Logout as Logout
import Pages.NotFound as NotFound
import Pages.TosserSignUp as TosserSignUp
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Request.User exposing (storeSession)
import Routing.Router as Router exposing (Route(..), fromLocation)
import Util exposing ((=>))
import Views.Page as Page exposing (frame)


type Page
    = Home Home.Model
    | TosserSignUp TosserSignUp.Model
    | Dash Dash.Model
    | Login Login.Model
    | BetaSignUp BetaSignUp.Model
    | CreateCampaign CreateCampaign.Model
    | Logout Logout.Model
    | NotFound NotFound.Model


type alias Model =
    { session : Session
    , location : Location
    , page : Page
    , githubUrl : WebData GitHubUrl
    , apiUrl : Maybe String
    , campaigns : WebData Campaigns
    , repos : WebData Repos
    , mostBountifulIssues : WebData Issues
    }


type Msg
    = HomeMsg Home.Msg
    | TosserSignUpMsg TosserSignUp.Msg
    | DashMsg Dash.Msg
    | LoginMsg Login.Msg
    | BetaSignUpMsg BetaSignUp.Msg
    | CreateCampaignMsg CreateCampaign.Msg
    | LogoutMsg Logout.Msg
    | NotFoundMsg NotFound.Msg
    | SetUser (Maybe User)
    | LoginUser String String
    | SetRoute (Maybe Route)
    | JoinChannel
    | ShowJoinedMessage String
    | ShowLeftMessage String
    | ReceiveMessage Encode.Value
    | FetchGitHubUrl (WebData GitHubUrl)
    | FetchCampaigns (WebData Campaigns)
    | FetchRepos (WebData Repos)
    | FetchIssues (WebData Issues)


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
        , campaigns = Loading
        , repos = Loading
        , mostBountifulIssues = Loading
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
                    { email = email, token = updatedToken, userId = userId }

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
                    , getRepos model.apiUrl updatedToken
                    , getIssues model.apiUrl updatedToken bountifulRepo.id
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

        Just Router.TosserSignUpRoute ->
            let
                updatedPage =
                    TosserSignUp TosserSignUp.init
            in
            { model | page = updatedPage } => Cmd.none

        Just Router.DashRoute ->
            let
                updatedPage =
                    Dash Dash.init
            in
            case model.session.user of
                Just user ->
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

                        bountifulRepo =
                            Repos.mostBountifulRepo repos

                        updatedIssues =
                            case model.mostBountifulIssues of
                                Success issues ->
                                    issues.issues

                                _ ->
                                    []

                        updatedPage =
                            CreateCampaign (CreateCampaign.init token userId bountifulRepo updatedIssues)
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

        ( DashMsg subMsg, Dash subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Dash.update subMsg subModel
            in
            { model | page = Dash pageModel } => Cmd.map DashMsg cmd

        ( CreateCampaignMsg subMsg, CreateCampaign subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    CreateCampaign.update subMsg subModel
            in
            { model | page = CreateCampaign pageModel } => Cmd.map CreateCampaignMsg cmd

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

        ( FetchGitHubUrl data, _ ) ->
            let
                updatedModel =
                    { model | githubUrl = data }
            in
            ( { updatedModel | page = Home { url = data } }, Cmd.none )

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
                            User "" (Data.AuthToken.init "") ""

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
            ( updatedModel, Cmd.none )

        ( FetchIssues data, _ ) ->
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
                            User "" (Data.AuthToken.init "") ""

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

                bountifulRepo =
                    Repos.mostBountifulRepo repos

                updatedIssues =
                    case data of
                        Success issues ->
                            issues.issues

                        _ ->
                            []

                updatedPage =
                    CreateCampaign (CreateCampaign.init token userId bountifulRepo updatedIssues)
            in
            ( { model | page = updatedPage, mostBountifulIssues = data }
            , cmd
            )

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

            TosserSignUp subModel ->
                TosserSignUp.view subModel
                    |> frame Page.TosserSignUp
                    |> Html.map TosserSignUpMsg

            Dash subModel ->
                Dash.view session subModel
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


getCampaigns : Maybe String -> AuthToken -> Cmd Msg
getCampaigns apiUrl token =
    let
        campaignUrl =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/campaigns"
    in
    RemoteData.Http.getWithConfig (Auth.config token) campaignUrl FetchCampaigns campaignsDecoder


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
    RemoteData.Http.getWithConfig (Auth.config token) issuesFilteredByRepoId FetchIssues Issues.decoder


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
