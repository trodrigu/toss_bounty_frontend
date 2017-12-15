module Pages.BetaSignUp exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode exposing (Value, encode, object, string)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Routing.Router as Router
import Util exposing ((=>))
import Validate exposing (ifBlank)


type Msg
    = UpdateEmailField String
    | ShowBetaSignUp


type ExternalMsg
    = NoOp


type alias Error =
    ( Field, String )


type Field
    = Form
    | Email


type alias Model =
    { subscriber : Subscriber
    , showBetaSignUp : Bool
    , errors : List Error
    }


type alias Subscriber =
    { email : String }


init : Model
init =
    { subscriber = { email = "" }
    , showBetaSignUp = False
    , errors = []
    }


view : Model -> Html Msg
view model =
    betaSignUpForm model


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdateEmailField str ->
            ( { model | subscriber = { email = str } }, Cmd.none ) => NoOp

        ShowBetaSignUp ->
            ( { model | showBetaSignUp = True }, Cmd.none ) => NoOp


betaSignUpForm : Model -> Html Msg
betaSignUpForm model =
    let
        betaSignUp =
            if model.showBetaSignUp then
                section [ class "hero" ]
                    [ div [ id "mc_embed_signup", class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
                        [ div [ class "columns" ]
                            [ div [ class "column is-one-third is-offset-one-third validate" ]
                                [ Html.form [ action "https://tossbounty.us17.list-manage.com/subscribe/post?u=a8cc56dad32e45c9cd3200f83&id=15a40eefae", class "validate", id "mc-embedded-subscribe-form", method "post", name "mc-embedded-subscribe-form", Html.Attributes.novalidate True, target "_blank" ]
                                    [ viewErrors model.errors
                                    , div [ id "mc_embed_signup_scroll" ]
                                        [ h2 [ class "title" ]
                                            [ text "Sign Up For The Beta" ]
                                        , div [ class "mc-field-group field" ]
                                            [ label [ for "mce-EMAIL", class "label" ]
                                                [ p [ class "control" ]
                                                    [ text "Email Address"
                                                    ]
                                                ]
                                            , input [ class "required email input", id "mce-EMAIL", name "EMAIL", type_ "email", onInput UpdateEmailField ]
                                                []
                                            ]
                                        , div [ class "clear", id "mce-responses" ]
                                            [ div [ class "response", id "mce-error-response", attribute "style" "display:none" ]
                                                []
                                            , div [ class "response", id "mce-success-response", attribute "style" "display:none" ]
                                                []
                                            ]
                                        , div [ attribute "aria-hidden" "true", attribute "style" "position: absolute; left: -5000px;" ]
                                            [ input [ name "b_a8cc56dad32e45c9cd3200f83_15a40eefae", attribute "tabindex" "-1", type_ "text", value "" ]
                                                []
                                            ]
                                        , div [ class "clear" ]
                                            [ input [ class "button", id "mc-embedded-subscribe", name "subscribe", type_ "submit", value "Subscribe" ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
            else
                section [ class "hero" ]
                    [ div [ class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
                        [ div [ class "container" ]
                            [ div [ class "columns is-vcentered" ]
                                [ div [ class "column has-text-centered" ]
                                    [ p [ class "title" ]
                                        [ text "For developers" ]
                                    , p [ class "subtitle" ]
                                        [ text "get paid with"
                                        , strong [] [ text " no strings" ]
                                        ]
                                    , p [ class "subtitle" ]
                                        [ text "for the open source software"
                                        , strong [] [ text " you create." ]
                                        ]
                                    , button [ class "button", onClick ShowBetaSignUp ]
                                        [ text "Start winning" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
    in
    div []
        [ betaSignUp ]


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "help is-danger" ]


validate : Subscriber -> List Error
validate =
    Validate.all
        [ .email >> ifBlank (Email => "Email can't be blank.") ]


subscriberDecoder : Decoder Subscriber
subscriberDecoder =
    decode Subscriber
        |> Json.Decode.Pipeline.required "email" Json.Decode.string


encode : { r | email : String } -> Json.Encode.Value
encode subscriber =
    Json.Encode.object
        [ ( "email_address", Json.Encode.string subscriber.email )
        , ( "status", Json.Encode.string "subscribed" )
        ]
