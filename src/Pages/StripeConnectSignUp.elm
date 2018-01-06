module Pages.StripeConnectSignUp exposing (..)

import Data.StripeConnectUrl as StripeConnectUrl exposing (StripeConnectUrl)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import RemoteData exposing (RemoteData(..), WebData)
import Util exposing ((=>))


type alias Model =
    { url : WebData StripeConnectUrl }


type Msg
    = NoOp


type ExternalMsg
    = ExternalNoOp


init : WebData StripeConnectUrl -> Model
init url =
    { url = url }


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
                    [ i [ class "fa fa-github" ] []
                    ]
                , p []
                    [ text "Sign In With Stripe Connect" ]
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
                            [ text "We use Stripe" ]
                        , viewButton model
                        ]
                    ]
                ]
            ]
        ]
