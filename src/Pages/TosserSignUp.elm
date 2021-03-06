module Pages.TosserSignUp exposing (ExternalMsg(..), Model, Msg(..), emptyTosserSignUpForm, encodeTosserSignUpFormAsValues, init, postTosserSignUpForm, tosserSignUpForm, update, view)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onInput)
import Http exposing (Header)
import Json.Decode exposing (succeed, Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode exposing (Value, encode, object, string)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Request.User as User exposing (storeSession)
import Routing.Router as Router


init : Model
init =
    emptyTosserSignUpForm


encodeTosserSignUpFormAsValues : Model -> Json.Encode.Value
encodeTosserSignUpFormAsValues tosserSignUpFormRecord =
    Json.Encode.object
        [ ( "email", Json.Encode.string tosserSignUpFormRecord.email )
        , ( "password", Json.Encode.string tosserSignUpFormRecord.password )
        ]


postTosserSignUpForm : Model -> Cmd Msg
postTosserSignUpForm model =
    let
        data =
            { email = model.email
            , password = model.password
            , stripeExternalId = ""
            , stripeAccessToken = ""
            }
    in
    RemoteData.Http.post "http://localhost:4000/users" HandlePostTosserSignUpForm User.decoder (User.encode data)


type alias Model =
    { email : String
    , password : String
    }


emptyTosserSignUpForm : Model
emptyTosserSignUpForm =
    { email = ""
    , password = ""
    }


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
            (( { model | email = str }, Cmd.none ), NoOp)

        UpdatePasswordField str ->
            (( { model | password = str }, Cmd.none ), NoOp)

        HandlePostTosserSignUpForm data ->
            case data of
                Success user ->
                    (
                        (
                        model
                        , Cmd.none
                        )
                        , SetUser user
                    )

                _ ->
                    (
                    ( model, Cmd.none )
                    ,   NoOp
                    )

        SaveTosserForm ->
            (( model, postTosserSignUpForm model ), NoOp)


view : Model -> Html Msg
view model =
    tosserSignUpForm


tosserSignUpForm : Html Msg
tosserSignUpForm =
    section [ class "hero" ]
        [ div [ class "hero-body", style "padding" "7rem 1.5rem" ]
            [ div [ class "columns" ]
                [ Html.form [ Html.Events.onSubmit SaveTosserForm, class "column is-one-third is-offset-one-third" ]
                    [ h1 [ class "title" ] [ text "Start Writing Stories" ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Email" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , onInput UpdateEmailField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Password" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
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
