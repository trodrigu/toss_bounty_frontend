module Routing.Router exposing (..)

import Navigation exposing (Location)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Routing.Helpers exposing (Route(..), parseLocation, reverseRoute, modifyUrl)
import Pages.Home as Home
import Pages.TosserSignUp as TosserSignUp
import Data.Session as Session exposing (Session)


type alias Model =
    { route : Route
    , homeModel : Home.Model
    , tosserSignUpModel : TosserSignUp.Model
    }

type Msg
    = UrlChange Location
    | NavigateTo Route
    | HomeMsg Home.Msg
    | TosserSignUpMsg TosserSignUp.Msg

update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update sessin msg model =
    -- let
    --     session =
    --         model.session

    case msg of
        UrlChange location ->
            ( { model | route = parseLocation location }
            , Cmd.none
            )

        NavigateTo route ->
            ( model
            , Navigation.newUrl (reverseRoute route)
            )

        TosserSignUpMsg tosserSignUpMsg ->
            updateTosserSignUp model tosserSignUpMsg

        HomeMsg homeMsg ->
            updateHome model homeMsg
    -- case ( msg, page ) of
    --     ( SetRoute route, _ ) ->
    --         setRoute route model

    --     ( SetUser user, _ ) ->
    --         let
    --             session =
    --                 model.session

    --             cmd =
    --                 -- If we just signed out, then redirect to Home.
    --                 if session.user /= Nothing && user == Nothing then
    --                     Route.modifyUrl Route.Home
    --                 else
    --                     Cmd.none
    --         in
    --             { model | session = { session | user = user } }
    --                 => cmd

        -- ( LoginMsg subMsg, Login subModel ) ->
        --     let
        --         ( ( pageModel, cmd ), msgFromPage ) =
        --             Login.update subMsg subModel

        --         newModel =
        --             case msgFromPage of
        --                 Login.NoOp ->
        --                     model

        --                 Login.SetUser user ->
        --                     let
        --                         session =
        --                             model.session
        --                     in
        --                     { model | session = { user = Just user } }
        --     in
        --     { newModel | pageState = Loaded (Login pageModel) }
        --         => Cmd.map LoginMsg cmd

        -- ( RegisterMsg subMsg, Register subModel ) ->
        --     let
        --         ( ( pageModel, cmd ), msgFromPage ) =
        --             Register.update subMsg subModel

        --         newModel =
        --             case msgFromPage of
        --                 Register.NoOp ->
        --                     model

        --                 Register.SetUser user ->
        --                     let
        --                         session =
        --                             model.session
        --                     in
        --                     { model | session = { user = Just user } }
        --     in
        --     { newModel | pageState = Loaded (Register pageModel) }
        --         => Cmd.map RegisterMsg cmd

        -- ( _, NotFound ) ->
        --     -- Disregard incoming messages when we're on the
        --     -- NotFound page.
        --     model => Cmd.none

        -- ( _, _ ) ->
        --     -- Disregard incoming messages that arrived for the wrong page
        --     model => Cmd.none

updateTosserSignUp : Model -> TosserSignUp.Msg -> ( Model, Cmd Msg )
updateTosserSignUp model tosserSignUpMsg =
    let
        ( nextTosserSignUpModel, tosserSignUpCmd ) =
            TosserSignUp.update tosserSignUpMsg model.tosserSignUpModel
    in
        ( { model | tosserSignUpModel = nextTosserSignUpModel }
        , Cmd.map TosserSignUpMsg tosserSignUpCmd
        )

updateHome : Model -> Home.Msg -> ( Model, Cmd Msg )
updateHome model homeMsg =
    let
        ( nextHomeModel, homeCmd ) =
            Home.update homeMsg model.homeModel
    in
        ( { model | homeModel = nextHomeModel }
        , Cmd.map HomeMsg homeCmd
        )

renderNav : Html Msg
renderNav =
    div [ class "container" ]
        [ nav [ class "nav" ]
              [ div [ class "nav-left" ]
                    [ a [ class "nav-item", onClick (NavigateTo HomeRoute) ]
                        [ text "Toss Bounty"
                        ]
                    ]
              , div [ class "nav-center" ]
                    [ a [ class "nav-item" ]
                        [ span [ class "icon" ]
                              [ i [ class "fa fa-twitter" ] [] ]
                        ]
                    ]
              , span [ class "nav-toggle" ]
                    [ span [] []
                    , span [] []
                    , span [] []
                    ]
              , div [ class "nav-right nav-menu" ]
                    [ a [ class "nav-item", onClick (NavigateTo TosserSignUpRoute ) ]
                        [ text "Sign up as a tosser"
                        ]
                    ]
              , div [ class "nav-right nav-menu" ]
                    [ a [ class "nav-item" ]
                        [ text "Sign up as a bounty hunter"
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
  div []
      [ pageView model
      ]

pageView : Model -> Html Msg
pageView model =
    div []
        [ (case model.route of
            HomeRoute ->
                Home.view model.homeModel
                    |> Html.map HomeMsg

            TosserSignUpRoute ->
                TosserSignUp.view model.tosserSignUpModel
                    |> Html.map TosserSignUpMsg

            NotFoundRoute ->
                h1 [] [ text "404 :(" ]
          )
        ]
