module Routing.Router exposing (Msg(..), Route(..), footerArea, fromLocation, href, modifyUrl, routeParser, routeToString, sessionChange)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, style)
import Json.Decode as Decode exposing (Value)
import Ports
import Url.Parser as UrlParser exposing (..)
import Url.Parser.Query as Query exposing (string)
import Browser.Navigation as Navigation
import Url exposing (Url)
import Browser.Navigation exposing (Key)


type Route
    = HomeRoute
    | StripeConnectSignUpRoute
    | TosserSignUpRoute
    | DashRoute
    | CreateCampaignRoute
    | CreateRewardsRoute
    | LoginRoute
    | AboutRoute
    | SaveTokenRoute (Maybe String) (Maybe String) (Maybe String)
    | SaveStripeRoute (Maybe String)
    | DiscoverRoute
    | ContributeRoute Int
    | LogoutRoute
    | CreateUserRoleRoute
    | GithubOopsRoute
    | NotFoundRoute


type Msg
    = NoOp


footerArea : Html Msg
footerArea =
    footer [ style "padding" "3rem 1.5rem 6rem", style "background-color" "whitesmoke" ]
        [ div [ class "container" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ strong []
                        [ text "TossBounty" ]
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
                    [ "contribute/" ++ (campaignId |> String.fromInt) ]

                CreateUserRoleRoute ->
                    [ "get-user-type" ]

                GithubOopsRoute ->
                    [ "github-oops" ]

                NotFoundRoute ->
                    []
    in
    "#/" ++ String.join "/" pieces


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        -- [ UrlParser.map AboutRoute UrlParser.top
        -- The home route will be the base after beta
        [ UrlParser.map HomeRoute (UrlParser.s "home")
        , UrlParser.map TosserSignUpRoute (UrlParser.s "tosser-sign-up")
        , UrlParser.map DashRoute (UrlParser.s "dash")
        , UrlParser.map CreateCampaignRoute (UrlParser.s "create-campaign")
        , UrlParser.map CreateRewardsRoute (UrlParser.s "create-rewards")
        , UrlParser.map StripeConnectSignUpRoute (UrlParser.s "stripe-connect-sign-up")
        , UrlParser.map SaveTokenRoute (UrlParser.s "save-session" <?> Query.string "token" <?> Query.string "email" <?> Query.string "user_id")
        , UrlParser.map SaveStripeRoute (UrlParser.s "save-stripe" <?> Query.string "stripe_id")
        , UrlParser.map LoginRoute (UrlParser.s "login")
        , UrlParser.map AboutRoute (UrlParser.s "about")
        , UrlParser.map DiscoverRoute (UrlParser.s "discover")
        , UrlParser.map ContributeRoute (UrlParser.s "contribute" </> UrlParser.int)
        , UrlParser.map CreateUserRoleRoute (UrlParser.s "get-user-type")
        , UrlParser.map LogoutRoute (UrlParser.s "logout")
        , UrlParser.map GithubOopsRoute (UrlParser.s "github-oops")
        , UrlParser.map AboutRoute (UrlParser.s "about")
        ]


modifyUrl : Key -> Route -> Cmd msg
modifyUrl key route =
    Navigation.pushUrl key (route |> routeToString)


href : Route -> Attribute msg
href route =
    Attributes.href (routeToString route)


fromLocation : Url -> Maybe Route
fromLocation location =
    if String.isEmpty location.path then
        Just HomeRoute

    else
        UrlParser.parse routeParser (location)