module Pages.Login exposing (Error, ExternalMsg(..), Field(..), Model, Msg(..), init, loginForm, postSignIn, update, view, viewErrors)

import Data.User as User exposing (User, encodeLogin)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (succeed, Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode exposing (Value, encode, object, string)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Request.User as User exposing (storeSession)
import Routing.Router as Router
import Validate exposing (ifBlank, Validator, validate)


init : Model
init =
    { email = ""
    , password = ""
    , errors = []
    }


type alias Model =
    { email : String
    , password : String
    , errors : List Error
    }


type Msg
    = SaveLoginForm
    | UpdateEmailField String
    | UpdatePasswordField String
    | HandleLogin (WebData User)


type ExternalMsg
    = NoOp
    | SetUser User


view : Model -> Html Msg
view model =
    loginForm model


loginForm : Model -> Html Msg
loginForm model =
    section [ class "hero" ]
        [ div [ class "hero-body", style "padding" "7rem 1.5rem" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third is-offset-one-third" ]
                    [ h1 [ class "title" ] [ text "Start Writing Stories" ]
                    , viewErrors model.errors
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
                            [ button [ class "button is-primary", onClick SaveLoginForm ]
                                [ text "Login" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdatePasswordField str ->
            (( { model | password = str }, Cmd.none ), NoOp)

        UpdateEmailField str ->
            (( { model | email = str }, Cmd.none ), NoOp)

        HandleLogin data ->
            case data of
                Success user ->
                    ((model
                        , Cmd.none)
                        , SetUser user)

                _ ->
                    (( model, Cmd.none ),NoOp)
                        

        SaveLoginForm ->
            case validate modelValidator model of
                Ok subject ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    (( model, postSignIn model ), NoOp)

                Err errors ->
                    (({ model | errors = errors }
                        , Cmd.none)
                        , NoOp)


postSignIn : Model -> Cmd Msg
postSignIn model =
    let
        data =
            { email = model.email
            , password = model.password
            }
    in
    RemoteData.Http.post "http://localhost:4000/token" HandleLogin User.decoder (User.encodeLogin data)


modelValidator : Validator (Field, String) Model
modelValidator =
    Validate.all
        [ ifBlank .email (Email, "Email can't be blank.")
        , ifBlank .password (Password, "Password can't be blank.")
        ]


type Field
    = Form
    | Email
    | Password


type alias Error =
    ( Field, String )


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "help is-danger" ]
