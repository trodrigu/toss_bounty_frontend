module Routing.Helpers exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>))
import Html exposing (Attribute)
import Html.Attributes as Attr


type Route
    = HomeRoute
    | TosserSignUpRoute
    | NotFoundRoute


reverseRoute : Route -> String
reverseRoute route =
    case route of
        TosserSignUpRoute ->
            "#/tosser-sign-up"

        _ ->
            "#/"

routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                HomeRoute ->
                    []

                TosserSignUpRoute ->
                    [ "tosser-sign-up" ]

                NotFoundRoute ->
                    []

    in
        "#/" ++ String.join "/" pieces

routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map HomeRoute Url.top
        , Url.map TosserSignUpRoute ( Url.s "tosser-sign-up" )
        ]


parseLocation : Location -> Route
parseLocation location =
    location
        |> Url.parseHash routeParser
        |> Maybe.withDefault NotFoundRoute

modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl

href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)
