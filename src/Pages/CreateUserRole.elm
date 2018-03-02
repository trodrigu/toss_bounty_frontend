module Pages.CreateUserRole exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.User as User exposing (User, encodeUserRoleUpdate)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, name, src, style, type_)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Util exposing ((=>))


init : AuthToken -> Maybe String -> User -> Model
init token apiUrl user =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { token = token
    , apiUrl = url
    , choice = Both
    , showNext = False
    , user = user
    }


type Choice
    = Maintainer
    | Contributor
    | Both



-- User Types
-- 0 == none
-- 1 == contributor
-- 2 == maintainer
-- 3 == both


choiceToInt : Choice -> Int
choiceToInt choice =
    case choice of
        Contributor ->
            1

        Maintainer ->
            2

        Both ->
            3


type alias Model =
    { token : AuthToken
    , apiUrl : String
    , choice : Choice
    , showNext : Bool
    , user : User
    }


type ExternalMsg
    = NoOp
    | UpdateUserWithRole User


type Msg
    = SwitchTo Choice
    | HandlePutUser (WebData User)
    | SubmitUserRole


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        HandlePutUser data ->
            let
                updatedUser =
                    case data of
                        Success user ->
                            user

                        _ ->
                            User.default
            in
            ( model, Cmd.none ) => UpdateUserWithRole updatedUser

        SubmitUserRole ->
            ( model, putUser model ) => NoOp

        SwitchTo updatedChoice ->
            let
                _ =
                    Debug.log "updatedChoice" updatedChoice
            in
            ( { model
                | choice = updatedChoice
                , showNext = True
              }
            , Cmd.none
            )
                => NoOp


view : Model -> Html Msg
view model =
    section [ class "hero is-primary is-bold is-large" ]
        [ div
            [ class "hero-body" ]
            [ div [ class "container" ]
                [ div [ class "title" ]
                    [ text "What kind of user are you?"
                    ]
                , div [ class "field" ]
                    [ div [ class "control" ]
                        [ radio " I maintain a project" (SwitchTo Maintainer)
                        , radio " I just want to contribute money $$" (SwitchTo Contributor)
                        , radio " Both" (SwitchTo Both)
                        ]
                    ]
                , div [ class "field is-grouped" ]
                    [ p [ class "control" ]
                        [ div [ class "button", onClick SubmitUserRole ] [ text "Next" ]
                        ]
                    ]
                ]
            ]
        ]


radio : String -> msg -> Html msg
radio value msg =
    label
        [ class "radio" ]
        [ input [ type_ "radio", name "user-type", onClick msg, class "is-large" ] []
        , text value
        ]


putUser : Model -> Cmd Msg
putUser model =
    let
        user =
            model.user

        userUrl =
            model.apiUrl ++ "/users/" ++ user.userId

        choiceAsInt =
            choiceToInt model.choice

        _ =
            Debug.log "choiceAsInt" choiceAsInt

        data =
            { role = choiceAsInt
            }
    in
    RemoteData.Http.putWithConfig (Auth.config model.token) userUrl HandlePutUser User.decoder (User.encodeUserRoleUpdate data)
