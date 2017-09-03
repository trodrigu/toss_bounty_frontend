module Pages.Logout exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput, onClick)
import Navigation
import Json.Encode exposing (encode, Value, object, string)
import RemoteData.Http
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Decode exposing (string, Decoder)
import Data.User as User exposing (User)
import Util exposing ((=>))

init : Model
init =
    {}

type alias Model =
    {}

type ExternalMsg
    = NoOp
    | SetUser User

type Msg
    = UpdateNameField String
    | UpdateEmailField String

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    (model, Cmd.none) => NoOp

view : Model -> Html Msg
view model =
   logoutForm

logoutForm : Html Msg
logoutForm =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ div [ class "column is-one-third is-offset-one-third"]
                              [ h1 [ class "title" ] [ text "Log yourelf out" ]
                              ]
                        ]
                  ]
            ]
