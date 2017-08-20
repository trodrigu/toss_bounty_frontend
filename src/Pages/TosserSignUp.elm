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

init : Model
init =
    emptyTosserSignUpForm

encodeTosserSignUpFormAsValues : Model -> Json.Encode.Value
encodeTosserSignUpFormAsValues tosserSignUpForm =
  Json.Encode.object
      [ ("name", Json.Encode.string tosserSignUpForm.name)
      , ("email", Json.Encode.string tosserSignUpForm.email)
      , ("password", Json.Encode.string tosserSignUpForm.password) ]


postTosserSignUpForm : Model -> Cmd Msg
postTosserSignUpForm tosserSignUpModel =
    RemoteData.Http.post "http://localhost:4000/users" HandlePostTosserSignUpForm tosserDecoder (encodeTosserSignUpFormAsValues tosserSignUpModel)

type alias Model =
    { name : String
    , email : String
    , password : String
    , tosser : WebData Tosser }

type alias Tosser =
    { name : String
    , email : String }

emptyTosserSignUpForm : Model
emptyTosserSignUpForm =
    { name = ""
    , email = ""
    , password = ""
    , tosser = NotAsked }

emptyTosser : Tosser
emptyTosser =
    { name = ""
    , email = "" }

type Msg
    = SaveTosserForm
    | UpdateNameField String
    | UpdateEmailField String
    | UpdatePasswordField String
    | HandlePostTosserSignUpForm (WebData Tosser)

tosserDecoder : Decoder Tosser
tosserDecoder =
  decode Tosser
      |> required "name" Json.Decode.string
      |> required "email" Json.Decode.string

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNameField str -> ( { model | name = str }, Cmd.none )

        UpdateEmailField str -> ( { model | email = str }, Cmd.none )

        UpdatePasswordField str -> ( { model | password = str }, Cmd.none )

        HandlePostTosserSignUpForm data ->
            case data of
                Success tosser ->
                    ({ model | tosser = data }, Cmd.none)

                _ ->
                    ( model, Cmd.none )

        SaveTosserForm ->
            let
                newModel =
                    { model | tosser = Loading }

            in
            ( newModel, postTosserSignUpForm model )

view : Model -> Html Msg
view model =
    case model.tosser of
        NotAsked ->
            tosserSignUpForm

        RemoteData.Loading ->
            div [][ text "Loading..." ]

        RemoteData.Success tosser ->
            div [][]

        RemoteData.Failure error ->
            text ("Oh noes, cat loading failed with error: " ++ toString error)

tosserSignUpForm : Html Msg
tosserSignUpForm =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ Html.form [ Html.Events.onSubmit SaveTosserForm, class "column is-one-third is-offset-one-third"]
                              [ h1 [ class "title" ] [ text "Start Writing Stories" ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Name" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateNameField
                                                ]
                                                []
                                        ]
                                    ]
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
