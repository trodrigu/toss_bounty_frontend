module Pages.CreateCampaign exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput, onClick)
import Navigation
import Json.Encode exposing (encode, Value, object, string)
import RemoteData.Http exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Decode exposing (string, Decoder)
import Data.User as User exposing (User, encodeLogin)
import Util exposing ((=>))
import Routing.Router as Router
import Request.User as User exposing (storeSession)
import Validate exposing (ifBlank)
import Task
import Time exposing (Time)
import Data.Campaign as Campaign exposing (..)
import Http exposing (header)
import Data.AuthToken as AuthToken exposing (AuthToken, fallback, toString)
import Time.Date exposing (Date)
import Date exposing (Date, now)

init : AuthToken -> Model
init token =
    { currentFunding = 0.0
    , errors = []
    , fundingEndDate = Time.Date.date 0 0 0
    , fundingGoal = 0.0
    , longDescription = ""
    , shortDescription = ""
    , token = token }

type alias Model =
    { currentFunding : Float
    , fundingEndDate : Time.Date.Date
    , fundingGoal : Float
    , longDescription : String
    , shortDescription : String
    , token : AuthToken
    , errors : List Error }

type Msg
    = SaveCampaignForm
    | UpdateFundingGoalField String
    | UpdateLongDescriptionField String
    | UpdateShortDescriptionField String
    | HandleCampaign (WebData Campaign)
    | RequestDate
    | ReceiveDate Date.Date

type ExternalMsg
    = NoOp
    | SetUser User

view : Model -> Html Msg
view model =
   createCampaignForm model

createCampaignForm : Model -> Html Msg
createCampaignForm model =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ div [ class "column is-one-third is-offset-one-third"]
                              [ h1 [ class "title" ] [ text "Create your first Campaign" ]
                              , viewErrors model.errors
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "FundingGoal" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , Html.Attributes.type_ "number"
                                                , onInput UpdateFundingGoalField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "LongDescription" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateLongDescriptionField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "ShortDescription" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateShortDescriptionField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field is-grouped" ]
                                    [ p [ class "control" ]
                                          [ button [ class "button is-primary", onClick SaveCampaignForm ]
                                                [ text "CreateCampaign" ]
                                        ]
                                    ]
                              ]
                        ]
                  ]
            ]

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UpdateFundingGoalField updatedFundingGoal ->
            let
                updatedFundingGoalFloat =
                    case String.toFloat updatedFundingGoal of
                        Ok updatedFundingGoalFloat ->
                            updatedFundingGoalFloat
                        Err error ->
                            0.0
            in
            ( { model | fundingGoal = updatedFundingGoalFloat }, Cmd.none ) => NoOp

        UpdateLongDescriptionField updatedLongDescription ->
            ( { model | longDescription = updatedLongDescription  }, Cmd.none ) => NoOp

        UpdateShortDescriptionField updatedShortDescription ->
            ( { model | shortDescription = updatedShortDescription }, Cmd.none ) => NoOp

        HandleCampaign data ->
            case data of
                Success campaign ->
                    ( model, Cmd.none )
                        => NoOp

                _ ->
                    ( model, Cmd.none )
                        => NoOp

        SaveCampaignForm ->
            case validate model of
                [] ->
                    let
                        newModel =
                            { model | errors = [] }

                    in
                    ( model, postCampaign model ) => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        RequestDate ->
            (model, Task.perform ReceiveDate now) => NoOp

        ReceiveDate date ->
            let
                convertedTimeDate = Time.Date.date ( Date.year date ) (0) ( Date.day date )
                updatedModel =
                    {model | fundingEndDate = convertedTimeDate}
            in
                (updatedModel, Cmd.none) => NoOp

authHeader : AuthToken -> Http.Header
authHeader token =
    let
        tokenString = AuthToken.toString token

        completeTokenValue =
            "Bearer " ++ tokenString

    in
    Http.header "Authorization" completeTokenValue

authConfig : AuthToken -> Config
authConfig token =
    { headers = [ authHeader token ]
    , withCredentials = True
    , timeout = Nothing
    }


-- curl 'http://localhost:4000/campaigns' -H 'Host: localhost:4000' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:58.0) Gecko/20100101 Firefox/58.0' -H 'Accept: */*' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: http://localhost:8000/' -H 'Authorization: Bearer eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJVc2VyOjEiLCJleHAiOjE1MTU3MDIxMTAsImlhdCI6MTUxMzExMDExMCwiaXNzIjoiVG9zc0JvdW50eSIsImp0aSI6ImRjNDhiMTk3LTEwYWItNGUxYy1hODA5LTYzNTA0YTBlNmIwNyIsInBlbSI6e30sInN1YiI6IlVzZXI6MSIsInR5cCI6InRva2VuIn0.SKE4b3RfdXQU4UA9qYi7UuZ8X5qAkyMQI-3oGO5pMpD20wrA1rDuxygimaRjyJcHvY04IiVriAE20uDt9u0LWA' -H 'Content-Type: application/json' -H 'Origin: http://localhost:8000' -H 'Connection: keep-alive' -H 'Pragma: no-cache' -H 'Cache-Control: no-cache' --data '{"data":{"relationships":{"user" : {"data": {"type": "user", "id": 1}}}, "type": "campaign", "attributes":{"short_description":"sfsdfafsdf","long_description":"asdfsfasdf","funding_goal":23424, "funding_end_date": "2013-03-06T01:25:19+02:00", "current_funding": 0.0}}}'
postCampaign : Model -> Cmd Msg
postCampaign model =
    let
        data =
            { currentFunding = model.currentFunding
            , shortDescription = model.shortDescription
            , longDescription = model.longDescription
            , fundingGoal = model.fundingGoal
            , fundingEndDate = model.fundingEndDate }

    in
        RemoteData.Http.postWithConfig ( authConfig model.token ) "http://localhost:4000/campaigns" HandleCampaign Campaign.decoder (Campaign.encode data)

validate : Model -> List Error
validate =
    Validate.all
        [ .shortDescription >> ifBlank (ShortDescription => "Short Description can't be blank.")
        -- , .longDescription >> ifBlank (LongDescription => "Long Description can't be blank.")
        -- , .fundingGoal >> ifBlank (FundingGoal => "Funding Goal can't be blank.")
        ]

type Field
    = Form
    | ShortDescription
    | LongDescription
    | FundingGoal

type alias Error =
    ( Field, String )

viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "help is-danger" ]
