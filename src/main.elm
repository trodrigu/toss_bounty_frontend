module Main exposing (..)

import Html exposing (..)
import Navigation exposing (Location)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Routing.Router as Router
import Ports
import Data.Session as Session exposing (Session)
import Pages.Home as Home
import Pages.TosserSignUp as TosserSignUp
import Views.Page as Page exposing (frame)
import Data.User as User exposing (User, Username)
import Json.Decode as Decode exposing (Value)

type Page
    = Blank
    | NotFound
    | Home Home.Model
    | TosserSignup TosserSignUp.Model

type alias Model =
    { location : Location
    , session : Session
    }

type Msg
    = NoOp
    | UrlChange Location
    | RouterMsg Router.Msg

init : Location -> ( Model, Cmd Msg )
init location =
    ( { location = location } )

sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RouterMsg routerMsg ->
            updatePage model routerMsg

updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model  =
    let
        session =
            model.session

    in
        Router.update session msg model

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

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
    viewPage model.session

viewPage : Session -> Page -> Html Msg
viewPage session page =
    let
        frame =
            Page.frame session.user
    in
    case page of

        Home subModel ->
            Home.view session subModel
                |> frame Home
                |> Html.map Router.HomeMsg

        Home subModel ->
            Home.view session subModel
                |> frame Home
                |> Html.map Router.HomeMsg
main : Program Never Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions }
