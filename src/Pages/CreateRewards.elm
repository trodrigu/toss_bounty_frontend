module Pages.CreateRewards exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Reward as Reward exposing (Reward)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onInput)
import List.Zipper exposing (fromList, mapCurrent, toList)
import Maybe
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router
import Util exposing ((=>))
import Validate exposing (ifBlank)


init : Maybe String -> AuthToken -> String -> Model
init apiUrl token campaignId =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { donationLevel = 100.0
    , description = ""
    , apiUrl = url
    , token = token
    , campaignId = campaignId
    , errors = []
    }


type alias Model =
    { donationLevel : Float
    , description : String
    , apiUrl : String
    , token : AuthToken
    , campaignId : String
    , errors : List Error
    }


type Msg
    = SaveRewardForm
    | UpdateDescriptionField String
    | UpdateDonateLevelField String
    | HandleReward (WebData Reward)


type ExternalMsg
    = NoOp


view : Model -> Html Msg
view model =
    div [] [ createRewardForm model ]


createRewardForm : Model -> Html Msg
createRewardForm model =
    section [ class "hero" ]
        [ div [ class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ h1 [ class "title" ] [ text "What kind of rewards will you have?" ]
                    , viewErrors model.errors
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Donation Level" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , Html.Attributes.type_ "number"
                                , onInput UpdateDonateLevelField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Description of reward" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , onInput UpdateDescriptionField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field is-grouped" ]
                        [ p [ class "control" ]
                            [ button [ class "button is-primary", onClick SaveRewardForm ]
                                [ text "Create Reward" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdateDonateLevelField str ->
            let
                updatedDonateLevel =
                    case String.toFloat str of
                        Ok donationLevel ->
                            donationLevel

                        Err error ->
                            0.0
            in
            ( { model | donationLevel = updatedDonateLevel }, Cmd.none ) => NoOp

        UpdateDescriptionField str ->
            ( { model | description = str }, Cmd.none ) => NoOp

        HandleReward data ->
            let
                _ =
                    Debug.log "data in Handle Reward" data
            in
            case data of
                Success reward ->
                    model
                        => Router.modifyUrl Router.StripeConnectSignUpRoute
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        SaveRewardForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }
                    in
                    ( model, postReward model ) => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp


postReward : Model -> Cmd Msg
postReward model =
    let
        rewardUrl =
            model.apiUrl ++ "/rewards"

        data =
            { description = model.description
            , donationLevel = model.donationLevel
            , campaignId = model.campaignId
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) rewardUrl HandleReward Reward.decoder (Reward.encode data)


type Field
    = Form
    | Description
    | DonateLevel


type alias Error =
    ( Field, String )


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "help is-danger" ]


validate : Model -> List Error
validate =
    Validate.all
        [ .description >> ifBlank (Description => "Description can't be blank.")

        -- , .donationLevel >> ifBlank (DonateLevel => "Donate Level can't be blank.")
        ]
