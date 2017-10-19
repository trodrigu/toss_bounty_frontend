module Pages.TosserSignUp exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput, onClick)
import Navigation
import Json.Encode exposing (encode, Value, object, string)
import RemoteData.Http
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Decode exposing (string, Decoder)
import Debug
import Data.User as User exposing (User)
import Request.User as User exposing (storeSession)
import Util exposing ((=>))
import Routing.Router as Router
import Http exposing (Header)

init : Model
init =
    emptyTosserSignUpForm

encodeTosserSignUpFormAsValues : Model -> Json.Encode.Value
encodeTosserSignUpFormAsValues tosserSignUpForm =
  Json.Encode.object
      [ ("email", Json.Encode.string tosserSignUpForm.email)
      , ("password", Json.Encode.string tosserSignUpForm.password) ]

postTosserSignUpForm : Model -> Cmd Msg
postTosserSignUpForm model =
    let
        data =
            { email = model.email
            , password = model.password }

    in
        RemoteData.Http.post "http://api.tossbounty.com/users" HandlePostTosserSignUpForm User.decoder (User.encode data)

type alias Model =
    { email : String
    , password : String }

emptyTosserSignUpForm : Model
emptyTosserSignUpForm =
    { email = ""
    , password = "" }

type Msg
    = SaveTosserForm
    | UpdateEmailField String
    | UpdatePasswordField String
    | HandlePostTosserSignUpForm (WebData User)

type ExternalMsg
    = NoOp
    | SetUser User

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdateEmailField str ->
            ( { model | email = str }, Cmd.none ) => NoOp

        UpdatePasswordField str ->
            ( { model | password = str }, Cmd.none ) => NoOp

        HandlePostTosserSignUpForm data ->
            case data of
                Success user ->
                    model
                      => Cmd.batch [ storeSession user, Router.modifyUrl Router.DashRoute ]
                      => SetUser user

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        SaveTosserForm ->
            ( model, postTosserSignUpForm model ) => NoOp

view : Model -> Html Msg
view model =
    tosserSignUpForm

tosserSignUpForm : Html Msg
tosserSignUpForm =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ Html.form [ Html.Events.onSubmit SaveTosserForm, class "column is-one-third is-offset-one-third"]
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
                                          [ button [ class "button is-primary" ]
                                                [ text "Save" ]
                                        ]
                                    ]
                              ]
                        ]
                  ]
            ]
