module Pages.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import RemoteData exposing (RemoteData(..), WebData)
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
    section [ class "hero is-primary" ]
        [ div [ class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
            [ div [ class "container" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column has-text-centered" ]
                        [ p [ class "title" ]
                            [ text "For tossers" ]
                        , p [ class "subtitle" ]
                            [ text "prime the"
                            , strong [] [ text " pump" ]
                            ]
                        , viewButton model
                        ]
                    ]
                ]
            ]
        ]
