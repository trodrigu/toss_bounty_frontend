module Routing.Router exposing (..)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, style)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Ports
import UrlParser exposing (..)


type Route
    = HomeRoute
    | StripeConnectSignUpRoute
    | TosserSignUpRoute
    | DashRoute
    | CreateCampaignRoute
    | CreateRewardsRoute
    | LoginRoute
    | BetaSignUpRoute
    | SaveTokenRoute (Maybe String) (Maybe String) (Maybe String)
    | SaveStripeRoute (Maybe String)
    | DiscoverRoute
    | LogoutRoute
    | NotFoundRoute


type Msg
    = NoOp


footerArea : Html Msg
footerArea =
    footer [ style [ ( "padding", "3rem 1.5rem 6rem" ), ( "background-color", "whitesmoke" ) ] ]
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
                    [ "home" ]

                StripeConnectSignUpRoute ->
                    [ "stripe-connect-sign-up" ]

                SaveStripeRoute (Just stripeId) ->
                    [ "save-stripe" ++ "?stripe_id" ++ stripeId ]

                SaveStripeRoute _ ->
                    [ "save-stripe" ]

                BetaSignUpRoute ->
                    [ "poop" ]

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

                NotFoundRoute ->
                    []
    in
    "#/" ++ String.join "/" pieces


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        -- [ UrlParser.map BetaSignUpRoute UrlParser.top
        -- The home route will be the base after beta
        [ UrlParser.map HomeRoute (UrlParser.s "home")
        , UrlParser.map TosserSignUpRoute (UrlParser.s "tosser-sign-up")
        , UrlParser.map DashRoute (UrlParser.s "dash")
        , UrlParser.map CreateCampaignRoute (UrlParser.s "create-campaign")
        , UrlParser.map CreateRewardsRoute (UrlParser.s "create-rewards")
        , UrlParser.map StripeConnectSignUpRoute (UrlParser.s "stripe-connect-sign-up")
        , UrlParser.map SaveTokenRoute (UrlParser.s "save-session" <?> UrlParser.stringParam "token" <?> UrlParser.stringParam "email" <?> UrlParser.stringParam "user_id")
        , UrlParser.map SaveStripeRoute (UrlParser.s "save-stripe" <?> UrlParser.stringParam "stripe_id")
        , UrlParser.map LoginRoute (UrlParser.s "login")
        , UrlParser.map BetaSignUpRoute (UrlParser.s "beta-sign-up")
        , UrlParser.map DiscoverRoute (UrlParser.s "discover")
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
        UrlParser.parseHash routeParser (fixLocationQuery location)


fixLocationQuery : Location -> Location
fixLocationQuery location =
    let
        hash =
            String.split "?" location.hash
                |> List.head
                |> Maybe.withDefault ""

        search =
            String.split "?" location.hash
                |> List.drop 1
                |> String.join "?"
                |> String.append "?"
    in
    { location | hash = hash, search = search }
