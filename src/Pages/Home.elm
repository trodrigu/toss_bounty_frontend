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
                                    [ text "Easy crowdfunding for open source." ]
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
                        [ div [ class "column is-3" ]
                            [ text "Start your campaign in minutes for your open source project and get funded." ]
                        , div [ class "column is-3 is-offset-1" ]
                            [ text "Backed by Stripe which handles billions of dollars every year." ]
                        , div [ class "column is-3 is-offset-1" ]
                            [ text "Create different rewards levels for your donors." ]
                        ]
                    ]
                ]
            ]
        , section [ class "hero is-primary is-medium" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns is-vcentered" ]
                        [ div [ class "column is-6" ]
                            [ div [ class "section-header" ]
                                [ h1 [ class "title" ]
                                    [ text "TossBounty costs just 10% of the money you make." ]
                                , viewButton model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
