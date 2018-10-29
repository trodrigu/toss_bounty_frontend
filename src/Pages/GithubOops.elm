module Pages.GithubOops exposing (ExternalMsg(..), Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import RemoteData exposing (RemoteData(..), WebData)


init : Model
init =
    {}


type alias Model =
    {}


type Msg
    = NoOp


type ExternalMsg
    = ExternalNoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        NoOp ->
            (( model, Cmd.none ) , ExternalNoOp)


view : Model -> Html Msg
view model =
    section [ class "hero is-primary is-bold is-large" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column has-text-centered" ]
                        [ p [ class "title is-large" ] [ text "Oops!" ]
                        , p [ class "title" ]
                            [ text "Please update your github email to be public at" ]
                        , a [ class "link", href "https://github.com/settings/emails" ]
                            [ text "https://github.com/settings/emails"
                            ]
                        , p [ class "title" ]
                            [ text "Then set your public email on your profile at" ]
                        , a [ class "link", href "https://github.com/settings/profile" ]
                            [ text "https://github.com/settings/profile" ]
                        ]
                    ]
                ]
            ]
        ]
