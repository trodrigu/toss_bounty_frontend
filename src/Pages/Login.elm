module Pages.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput, onClick)
import Navigation
import Json.Encode exposing (encode, Value, object, string)
import RemoteData.Http
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Decode exposing (string, Decoder)

init : Model
init =
    { email = ""
    , password = ""}

type alias Model =
    { email : String
    , password : String }

type Msg
    = SaveLoginForm
    | UpdateEmailField String
    | UpdatePasswordField String

view : Model -> Html Msg
view model =
   loginForm

loginForm : Html Msg
loginForm =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ div [ class "column is-one-third is-offset-one-third"]
                              [ h1 [ class "title" ] [ text "Start Writing Stories" ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Email" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateEmailField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Password" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdatePasswordField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field is-grouped" ]
                                    [ p [ class "control" ]
                                          [ button [ class "button is-primary", onClick SaveLoginForm ]
                                                [ text "Save" ]
                                        ]
                                    ]
                              ]
                        ]
                  ]
            ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (model, Cmd.none)
