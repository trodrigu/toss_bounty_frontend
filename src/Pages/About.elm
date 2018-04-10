module Pages.About exposing (..)

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
    | ShowAbout


type ExternalMsg
    = NoOp


type alias Error =
    ( Field, String )


type Field
    = Form
    | Email


type alias Model =
    { subscriber : Subscriber
    , showAbout : Bool
    , errors : List Error
    }


type alias Subscriber =
    { email : String }


init : Model
init =
    { subscriber = { email = "" }
    , showAbout = False
    , errors = []
    }


view : Model -> Html Msg
view model =
    aboutForm model


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdateEmailField str ->
            ( { model | subscriber = { email = str } }, Cmd.none ) => NoOp

        ShowAbout ->
            ( { model | showAbout = True }, Cmd.none ) => NoOp


aboutForm : Model -> Html Msg
aboutForm model =
    let
        about =
            if model.showAbout then
                section [ class "hero" ]
                    [ div [ id "mc_embed_signup", class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
                        [ div [ class "columns" ]
                            [ div [ class "column is-one-third is-offset-one-third validate" ]
                                [ Html.form [ action "https://tossbounty.us17.list-manage.com/subscribe/post?u=a8cc56dad32e45c9cd3200f83&id=15a40eefae", class "validate", id "mc-embedded-subscribe-form", method "post", name "mc-embedded-subscribe-form", Html.Attributes.novalidate True, target "_blank" ]
                                    [ viewErrors model.errors
                                    , div [ id "mc_embed_signup_scroll" ]
                                        [ h2 [ class "title" ]
                                            [ text "Stay in the know" ]
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
                            [ div [ class "columns" ]
                                [ div [ class "column is-offset-one-third is-one-third" ]
                                    [ p [ class "title" ]
                                        [ text "For developers" ]
                                    , p [ class "subtitle" ]
                                        [ text "Get paid with"
                                        , strong [] [ text " no strings" ]
                                        ]
                                    , p [ class "subtitle" ]
                                        [ text "For the open source software"
                                        , strong [] [ text " you create." ]
                                        ]
                                    , p []
                                        [ text "Start your campaign in minutes for your open source project and get funded." ]
                                    , p []
                                        [ text "TossBounty will parse your Github data and present you a marketable page to help you." ]
                                    , p []
                                        [ text "You are free to tweak this page and add rewards for your awesome donors." ]
                                    , p []
                                        [ text "TossBounty only takes a small fee of 10% from your subscriptions."
                                        ]
                                    , button [ class "button is-primary is-large", style [ ( "margin-top", "1rem" ) ], onClick ShowAbout ]
                                        [ text "Subscribe" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
    in
    div []
        [ about ]


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
