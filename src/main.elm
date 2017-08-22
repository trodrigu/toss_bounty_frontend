module Main exposing (..)

import Html exposing (..)
import Navigation exposing (Location)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Routing.Router as Router exposing (Route(..), fromLocation)
import Ports
import Data.Session as Session exposing (Session)
import Pages.Home as Home
import Pages.TosserSignUp as TosserSignUp
import Pages.Login as Login
import Pages.Logout as Logout
import Pages.NotFound as NotFound
import Views.Page as Page exposing (frame)
import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Value)
import Util exposing ((=>))

type Page
    = Home Home.Model
    | TosserSignUp TosserSignUp.Model
    | Login Login.Model
    | Logout Logout.Model
    | NotFound NotFound.Model
    | Blank

type alias Model =
    { session : Session
    , routerModel : Router.Model
    , location : Location
    , page : Page
    }

type Msg
    = NoOp
    | UrlChange Location
    | RouterMsg Router.Msg
    | HomeMsg Home.Msg
    | TosserSignUpMsg TosserSignUp.Msg
    | LoginMsg Login.Msg
    | SetUser (Maybe User)

decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)

init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Router.fromLocation location)
        { session = { user = decodeUserFromJson val }
        , routerModel = routerModel
        , location = location
        , init = Home
        }

routerModel : Router.Model
routerModel =
    { route = HomeRoute
    , homeModel = Home.init
    , tosserSignUpModel = TosserSignUp.init
    , loginModel = Login.init
    , logoutModel = Logout.init
    , notFoundModel = NotFound.init
    }

setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of

        Just Router.HomeRoute ->
            model => Cmd.none

        Just Router.LoginRoute ->
            model => Cmd.none

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
            model => Cmd.none

        Just Router.NotFoundRoute -> model => Cmd.none

        Nothing -> model => Cmd.none

sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RouterMsg routerMsg ->
            updateRouter routerMsg model

        UrlChange location ->
            updateRouter (Router.UrlChange location) { model | location = location }

        HomeMsg _ ->
            model => Cmd.none

        TosserSignUpMsg subMsg ->
            let
                newTosserSignUpModel =
                    TosserSignUp.update subMsg subModel

        LoginMsg _ ->
            model => Cmd.none

updateRouter :  Router.Msg -> Model -> ( Model, Cmd Msg )
updateRouter msg model  =
    let
        session =
            model.session

        routerModel =
            model.routerModel

        (updatedRouterModel, _) =
          Router.update session msg routerModel
    in
        { model | routerModel = updatedRouterModel } => Cmd.none

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

footerArea : Html Msg
footerArea =
    footer [ style [("padding", "3rem 1.5rem 6rem"), ("background-color", "whitesmoke")]]
           [ div [ class "container" ]
                 [ div [ class "content has-text-centered" ]
                       [ p []
                           [ strong []
                                    [ text "Toss Bounty" ]
                           ]
                       ]
                 ]
           ]

view : Model -> Html Msg
view model =
    Router.view model.routerModel
        |> Html.map RouterMsg

main : Program Value Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions }
