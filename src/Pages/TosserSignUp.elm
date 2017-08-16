module Pages.TosserSignUp exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput, onClick)
import Navigation
import Routing.Helpers exposing (Route(HomeRoute))
import Json.Encode exposing (encode, Value, object, string)
import RemoteData.Http
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Decode exposing (string, Decoder)

encodeTosserSignUpFormAsValues : Model -> Json.Encode.Value
encodeTosserSignUpFormAsValues tosserSignUpForm =
  Json.Encode.object
      [ ("name", Json.Encode.string tosserSignUpForm.name)
      , ("email", Json.Encode.string tosserSignUpForm.email)
      , ("company", Json.Encode.string tosserSignUpForm.company)
      , ("password", Json.Encode.string tosserSignUpForm.password)
      , ("passwordConfirmation", Json.Encode.string tosserSignUpForm.passwordConfirmation) ]


postTosserSignUpForm : Model -> Cmd Msg
postTosserSignUpForm tosserSignUpForm =
    RemoteData.Http.post "/users" HandlePostTosserSignUpForm tosserDecoder (encodeTosserSignUpFormAsValues tosserSignUpForm)

type alias Model =
    { name : String
    , email : String
    , company : String
    , password : String
    , tosser : WebData Tosser
    , passwordConfirmation : String }

type alias Tosser =
    { name : String
    , email : String
    , company : String }

emptyTosserSignUpForm : Model
emptyTosserSignUpForm =
    { name = ""
    , email = ""
    , company = ""
    , password = ""
    , tosser = NotAsked
    , passwordConfirmation = "" }

emptyTosser : Tosser
emptyTosser =
    { name = ""
    , email = ""
    , company = "" }

type Msg
    = SaveTosserForm
    | UpdateNameField String
    | UpdateEmailField String
    | UpdateCompanyField String
    | UpdatePasswordField String
    | UpdatePasswordConfirmationField String
    | HandlePostTosserSignUpForm (WebData Tosser)

tosserDecoder : Decoder Tosser
tosserDecoder =
  decode Tosser
      |> required "name" Json.Decode.string
      |> required "email" Json.Decode.string
      |> required "company" Json.Decode.string

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNameField str -> ( { model | name = str }, Cmd.none )

        UpdateEmailField str -> ( { model | email = str }, Cmd.none )

        UpdateCompanyField str -> ( { model | company = str }, Cmd.none )

        UpdatePasswordField str -> ( { model | password = str }, Cmd.none )

        UpdatePasswordConfirmationField str -> ( { model | passwordConfirmation = str }, Cmd.none )

        HandlePostTosserSignUpForm data ->
            ({ model | tosser = data }, Cmd.none)

        SaveTosserForm ->
            ( model, postTosserSignUpForm model )

view : Model -> Html Msg
view model =
    tosserSignUpForm

tosserSignUpForm : Html Msg
tosserSignUpForm =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ div [ class "column is-one-third is-offset-one-third"]
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
                                            [ text "Company" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateCompanyField
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
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Password Confirmation" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdatePasswordConfirmationField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field is-grouped" ]
                                    [ p [ class "control" ]
                                          [ button [ class "button is-primary", onClick SaveTosserForm ]
                                                [ text "Save" ]
                                        ]
                                    ]
                              ]
                        ]
                  ]
            ]
