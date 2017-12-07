module Main exposing (..)

import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import Navigation exposing (Location)
import Pages.Dash as Dash
import Pages.Home as Home exposing (GitHubUrl)
import Pages.Login as Login
import Pages.Logout as Logout
import Pages.NotFound as NotFound
import Pages.TosserSignUp as TosserSignUp
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Routing.Router as Router exposing (Route(..), fromLocation)
import Util exposing ((=>))
import Views.Page as Page exposing (frame)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt)
import Request.User exposing (storeSession)
import Data.AuthToken exposing (AuthToken, fallback, init)

type Page
    = Home Home.Model
    | TosserSignUp TosserSignUp.Model
    | Dash Dash.Model
    | Login Login.Model
    | Logout Logout.Model
    | NotFound NotFound.Model

type alias Model =
    { session : Session
    , location : Location
    , page : Page
    , githubUrl : WebData (GitHubUrl)
    , apiUrl : Maybe String
    }

type Msg
    = HomeMsg Home.Msg
    | TosserSignUpMsg TosserSignUp.Msg
    | DashMsg Dash.Msg
    | LoginMsg Login.Msg
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

decodeUserFromJson : Decode.Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue ( Decode.field "session" Decode.string )
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
        }

decodeUrlFromJson : Decode.Value -> Maybe String
decodeUrlFromJson json =
    json
        |> Decode.decodeValue ( Decode.field "apiUrl" Decode.string )
        |> Result.toMaybe

setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of

        Just Router.HomeRoute ->
            model => Cmd.batch [ getGitHubSignInUrl model.apiUrl ]

        Just ( Router.SaveTokenRoute ( Just token ) ( Just email ) ) ->
            let
                user = { email = email
                       , token = ( Data.AuthToken.init token )
                       , isMaintainer = False
                       , isTosser =  False }
                session = model.session
                updatedModel = { model | session = { session | user = Just user } }
            in

            updatedModel => Cmd.batch [ Router.modifyUrl Router.DashRoute
                                      , Request.User.storeSession user
                                      ]

        Just ( Router.SaveTokenRoute _ _ ) ->

            model => Cmd.batch [ Router.modifyUrl Router.DashRoute
                               ]

        Just Router.LoginRoute ->
            let
                updatedPage = Login Login.init

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
                updatedPage = TosserSignUp TosserSignUp.init

            in
            { model | page = updatedPage } => Cmd.none

        Just Router.DashRoute ->
            let
                updatedPage = Dash Dash.init

            in
                case model.session.user of
                    Just user ->
                        { model | page = updatedPage } => Cmd.none

                    Nothing ->
                        model => Router.modifyUrl Router.HomeRoute

        Just Router.NotFoundRoute -> model => Cmd.none

        Nothing -> model => Cmd.none

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
            ( { model | page = (toModel newModel) }, Cmd.map toMsg newCmd )

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
                { model | page = ( Home pageModel ) } => Cmd.map HomeMsg cmd

        ( DashMsg subMsg, Dash subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Dash.update subMsg subModel

            in
                { model | page = ( Dash pageModel) } => Cmd.map DashMsg cmd

        ( TosserSignUpMsg subMsg, TosserSignUp subModel )->
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
                { newModel | page = ( TosserSignUp pageModel ) } => Cmd.map TosserSignUpMsg cmd


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
                                { model | session = { user =  Just user } }
            in
                { model | page = ( Login pageModel) } => Cmd.map LoginMsg cmd

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
                                { model | session = { user =  Just user } }
            in
                { model | page = ( Logout pageModel ) } => Cmd.map LogoutMsg cmd

        ( FetchGitHubUrl data, _ ) ->
            let
                updatedModel =
                    { model | githubUrl = data }

            in
            ({ updatedModel | page = Home { url = data } }, Cmd.none)

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
                [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                      [ div [ class "container" ]
                            [ div [ class "columns is-vcentered" ]
                                  [ div [ class "column has-text-centered" ]
                                        [ p [ class "title" ]
                                            [ text "For developers" ]
                                        , p [ class "subtitle" ]
                                            [ text "get paid with"
                                            , strong [][ text " no strings"]
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
        [ (case page of
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

            Login subModel ->
              Login.view subModel
                  |> frame Page.Login
                  |> Html.map LoginMsg

            Logout subModel ->
              Logout.view subModel
                  |> Html.map LogoutMsg

            NotFound subModel ->
              NotFound.view
                  |> Html.map NotFoundMsg

              ) ]

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
        , subscriptions = subscriptions }

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
