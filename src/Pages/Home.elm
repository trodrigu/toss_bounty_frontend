module Pages.Home exposing (ExternalMsg(..), GitHubUrl, Model, Msg(..), init, update, view, viewButton)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import RemoteData exposing (RemoteData(..), WebData)
import Http exposing (Error(..), Response)
import RemoteData.Http exposing (..)
import Json.Decode as Decode exposing (succeed, Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, optionalAt, requiredAt)
import Html.Attributes as Attributes exposing (type_)


type alias Model =
    { webDataUrl : WebData GitHubUrl
    , apiUrl : Maybe String }


type alias GitHubUrl =
    { url : String }


type Msg
    = NoOp
    | HandleGithubUrl (WebData GitHubUrl)


type ExternalMsg
    = ExternalNoOp


init : Maybe String -> Model
init apiUrl =
    { apiUrl = apiUrl
    , webDataUrl = Loading }


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        HandleGithubUrl data ->
            let
                updatedModel =
                    { model | webDataUrl = data }
            in
            (( updatedModel, Cmd.none ), ExternalNoOp)


        NoOp ->
            (( model, Cmd.none ), ExternalNoOp)


viewButton : Model -> Html Msg
viewButton model =
    case model.webDataUrl of
        Loading ->
            text "Loading"

        Failure err ->
            case err of
                BadUrl msg ->
                    text ("There was an error with " ++ msg)

                Timeout ->
                    text "There was a timeout."

                NetworkError ->
                    text "There was a Network Error."

                BadStatus response ->
                    text ("There was a Bad Status with " ++ response.body)

                BadPayload msg response ->
                    text ("There was a bad payload with " ++ msg ++ "with a reponse of " ++ response.body)


        Success data ->
            a [ class "button is-medium", Html.Attributes.href data.url, type_ "button" ]
                [ span [ class "icon is-medium" ]
                    [ i [ class "fab fa-github-alt" ] []
                    ]
                , p []
                    [ text "Sign In With GitHub" ]
                ]

        NotAsked ->
            text ""


view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-primary is-medium" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns is-vcentered" ]
                        [ div [ class "column is-6" ]
                            [ div [ class "section-header" ]
                                [ h1 [ class "title" ]
                                    [ text "Easy crowdfunding for open source." ]
                                , h2 [ class "subtitle is-3" ]
                                    [ text "Support the projects you love." ]
                                , viewButton model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , section [ class "hero is-medium" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-3" ]
                            [ text "Start your campaign in minutes for your open source project and get funded." ]
                        , div [ class "column is-3 is-offset-1" ]
                            [ text "Backed by Stripe which handles billions of dollars every year." ]
                        , div [ class "column is-3 is-offset-1" ]
                            [ text "Create different rewards levels for your donors." ]
                        ]
                    ]
                ]
            ]
        , section [ class "hero is-primary is-medium" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns is-vcentered" ]
                        [ div [ class "column is-6" ]
                            [ div [ class "section-header" ]
                                [ h1 [ class "title" ]
                                    [ text "TossBounty costs just 4% of the funds you make." ]
                                , viewButton model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

getGitHubSignInUrl : Maybe String -> Cmd Msg
getGitHubSignInUrl apiUrl =
    let
        github_oauth_url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url ++ "/github_oauth_url"
    in
    RemoteData.Http.get github_oauth_url HandleGithubUrl githubUrlDecoder


githubUrlDecoder : Decoder GitHubUrl
githubUrlDecoder =
    succeed GitHubUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""


