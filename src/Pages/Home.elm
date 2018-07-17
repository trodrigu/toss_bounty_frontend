module Pages.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import RemoteData exposing (RemoteData(..), WebData)
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (color, cx, cy, fill, height, r, rx, ry, viewBox, width, x, y)
import Util exposing ((=>))


type alias Model =
    { url : WebData GitHubUrl }


type alias GitHubUrl =
    { url : String }


type Msg
    = NoOp


type ExternalMsg
    = ExternalNoOp


init : Model
init =
    { url = Loading }


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none ) => ExternalNoOp


tossBountyLogo : Html.Html msg
tossBountyLogo =
    svg
        [ width "120", height "120", fill "white" ]
        [ rect [ x (toString 10), y (toString 50), width (toString (30 * 0.5)), height (toString (0.5 * 10)) ] []
        , rect [ x (toString 10), y (toString 50), width (toString (10 * 0.5)), height (toString (0.5 * 45)) ] []
        , circle [ r (toString (10 * 0.5)), cx (toString 35), cy (toString 55) ] []
        , circle [ r (toString (10 * 0.5)), cx (toString 35), cy (toString 69) ] []
        ]


viewButton : Model -> Html Msg
viewButton model =
    case model.url of
        Loading ->
            text ""

        Failure _ ->
            text ""

        Success data ->
            a [ class "button is-medium", Html.Attributes.href data.url ]
                [ span [ class "icon is-medium" ]
                    [ i [ class "fab fa-github-alt" ] []
                    ]
                , p []
                    [ text "Sign In With GitHub" ]
                ]

        _ ->
            text ""


view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-primary is-medium" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns is-vcentered" ]
                        [ div [ class "column is-6" ]
                            [ div [ class "section-header" ]
                                [ h1 [ class "title" ]
                                    [ text "Easy crowdfunding for open source" ]
                                , h2 [ class "subtitle is-3" ]
                                    [ text "Support the projects you love." ]
                                , viewButton model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , section [ class "hero is-medium" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-4 is-offset-1" ]
                            []
                        , div [ class "column is-5 is-offset-1" ]
                            []
                        ]
                    ]
                ]
            ]
        ]
