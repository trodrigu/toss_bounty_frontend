module Routing.Router exposing (..)

import Navigation exposing (Location)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, style)
import Html.Events exposing (..)
-- import Pages.Home as Home
-- import Pages.TosserSignUp as TosserSignUp
-- import Pages.Login as Login
-- import Pages.Logout as Logout
-- import Pages.NotFound as NotFound
import Data.Session as Session exposing (Session)
import UrlParser
import Data.User as User exposing (User)
import Ports
import Util exposing ((=>))
import Json.Decode as Decode exposing (Value)

type Route
    = HomeRoute
    | TosserSignUpRoute
    | DashRoute
    | LoginRoute
    | LogoutRoute
    | NotFoundRoute


-- type alias Model =
--     { route : Route
--     , notFoundModel : NotFound.Model
--     , homeModel : Home.Model
--     , tosserSignUpModel : TosserSignUp.Model
--     , loginModel : Login.Model
--     , logoutModel : Logout.Model
--     }

type Msg = NoOp
--     = SetRoute (Maybe Route)
--     | HomeMsg Home.Msg
--     | TosserSignUpMsg TosserSignUp.Msg
--     | LoginMsg Login.Msg
--     | LogoutMsg Logout.Msg
--     | NotFoundMsg NotFound.Msg
--     | NavigateTo Route
--     | UrlChange Location

-- update : Session -> Msg -> Model -> ( Model, Cmd Msg )
-- update session msg model =
--     case msg of
--         UrlChange location ->
--             ( { model | route = parseLocation location }
--             , Cmd.none )

--         SetRoute route ->
--             setRoute route model

--         NavigateTo route ->
--             ( model
--             , Navigation.newUrl (reverseRoute route)
--             )
    -- let
    --     session =
    --         model.session

    -- case msg of
    --     UrlChange location ->
    --         ( { model | route = parseLocation location }
    --         , Cmd.none
    --         )

    --     NavigateTo route ->
    --         ( model
    --         , Navigation.newUrl (reverseRoute route)
    --         )

        -- NotFoundMsg notFoundMsg ->
        --     let
        --         ( newNotFoundModel, _ ) =
        --             NotFound.update notFoundMsg model.notFoundModel

        --     in
        --         { model | notFoundModel = newNotFoundModel } => Cmd.none

        -- TosserSignUpMsg tosserSignUpMsg ->
        --     let
        --         ( newTosserSignUpModel, _ ) =
        --             TosserSignUp.update tosserSignUpMsg model.tosserSignUpModel

        --     in
        --         { model | tosserSignUpModel = newTosserSignUpModel } => Cmd.none

        -- LoginMsg loginMsg ->
        --     let
        --         ( newLoginModel, _ ) =
        --             Login.update loginMsg model.loginModel

        --     in
        --         { model | loginModel = newLoginModel } => Cmd.none

        -- LogoutMsg logoutMsg ->
        --     let
        --         ( newLogoutModel, _ ) =
        --             Logout.update logoutMsg model.logoutModel

        --     in
        --         { model | homeModel = newLogoutModel } => Cmd.none

        -- HomeMsg homeMsg ->

        --     let
        --         ( newHomeModel, _ ) =
        --             Home.update homeMsg model.homeModel

        --     in
        --         { model | homeModel = newHomeModel } => Cmd.none

-- updateSubModel : Model Msg -> ( Model, Cmd Msg )
-- updateSubModel model msg =
--   let
--       ( newSubModel, _ ) =
--           Sub.update subMsg model.subModel

--   in
--       { model | subModel = newSubModel } => Cmd.none

-- setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
-- setRoute maybeRoute model =
--     case maybeRoute of
--         Nothing ->
--             model => Cmd.none

--         Just HomeRoute ->
--             model => Cmd.none

--         Just TosserSignUpRoute ->
--             model => Cmd.none

--         Just LoginRoute ->
--             model => Cmd.none

--         Just NotFoundRoute ->
--             model => Cmd.none

--         Just LogoutRoute ->
--             model => Cmd.none
            -- let
            --     session =
            --         model.session
            -- in
            -- { model | session = { session | user = Nothing } }
            --     => Cmd.batch
            --         [ Ports.storeSession Nothing
            --         , Route.modifyUrl Route.Home
            --         ]


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

-- reverseRoute : Route -> String
-- reverseRoute route =
--     case route of
--         TosserSignUpRoute ->
--             "#/tosser-sign-up"

--         _ ->
--             "#/"

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
