module Routing.Router exposing (..)

import Navigation exposing (Location)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, style)
import Data.User as User exposing (User)
import Ports
import Json.Decode as Decode exposing (Value)

type Route
    = HomeRoute
    | TosserSignUpRoute
    | DashRoute
    | LoginRoute
    | LogoutRoute
    | NotFoundRoute

type Msg = NoOp

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

sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)

routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                HomeRoute ->
                    []

                TosserSignUpRoute ->
                    [ "tosser-sign-up" ]

                DashRoute ->
                    [ "dash" ]

                LoginRoute ->
                    [ "login" ]

                LogoutRoute ->
                    [ "logout" ]

                NotFoundRoute ->
                    []

    in
        "#/" ++ String.join "/" pieces

routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map TosserSignUpRoute ( UrlParser.s "tosser-sign-up" )
        , UrlParser.map DashRoute ( UrlParser.s "dash" )
        , UrlParser.map LoginRoute ( UrlParser.s "login" )
        ]


parseLocation : Location -> Route
parseLocation location =
    location
        |> UrlParser.parseHash routeParser
        |> Maybe.withDefault NotFoundRoute

modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl

href : Route -> Attribute msg
href route =
    Attributes.href (routeToString route)

fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just HomeRoute
    else
        UrlParser.parseHash routeParser location
