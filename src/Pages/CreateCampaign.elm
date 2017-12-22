module Pages.CreateCampaign exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback, toString)
import Data.Campaign as Campaign exposing (..)
import Data.Issue as Issue exposing (Issue)
import Data.Issues as Issues exposing (Issues)
import Data.Repo as Repo exposing (Repo)
import Data.User as User exposing (User, encodeLogin)
import Date exposing (Date, now)
import Html exposing (..)
import Html.Attributes exposing (alt, class, datetime, href, src, style)
import Html.Events exposing (onClick, onInput)
import Http exposing (header)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode exposing (Value, encode, object, string)
import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Request.User as User exposing (storeSession)
import Routing.Router as Router
import Task
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))
import Validate exposing (ifBlank)


init : AuthToken -> String -> Repo -> List Issue -> Model
init token userId repo issues =
    { currentFunding = 0.0
    , errors = []
    , fundingEndDate = DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }
    , fundingGoal = 0.0
    , longDescription = ""
    , shortDescription = ""
    , token = token
    , userId = userId
    , bountifulRepo = repo
    , issues = issues
    }


type alias Model =
    { currentFunding : Float
    , fundingEndDate : DateTime
    , fundingGoal : Float
    , longDescription : String
    , shortDescription : String
    , token : AuthToken
    , errors : List Error
    , userId : String
    , bountifulRepo : Repo
    , issues : List Issue
    }


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
    maintainerHero model


maintainerHero : Model -> Html Msg
maintainerHero model =
    let
        name =
            model.bountifulRepo.name

        imageSrc =
            model.bountifulRepo.image

        owner =
            model.bountifulRepo.owner
    in
    div []
        [ section [ class "hero" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ]
                        [ text name ]
                    , h2 [ class "subtitle" ]
                        [ text ("By " ++ owner) ]
                    ]
                ]
            ]
        , issuesView model.issues
        , bioAndPic model
        ]


issuesView : List Issue -> Html Msg
issuesView issues =
    div [] <|
        List.map issueView issues


issueView : Issue -> Html Msg
issueView issue =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ]
                    [ h1 [ class "title" ]
                        [ text issue.title ]
                    , h2 [ class "subtitle" ]
                        [ text issue.body ]
                    ]
                ]
            ]
        ]


bioAndPic : Model -> Html Msg
bioAndPic model =
    section [ class "section" ]
        [ div
            [ class "container" ]
            [ article [ class "media" ]
                [ figure [ class "media-left" ]
                    [ p [ class "image is-128x128" ]
                        [ img [ src model.bountifulRepo.image ]
                            []
                        ]
                    ]
                , div [ class "media-content" ]
                    [ div [ class "content" ]
                        [ p []
                            [ strong []
                                [ text model.bountifulRepo.owner ]
                            , br []
                                []
                            , text "Does code push you?"
                            ]
                        ]
                    , nav [ class "level is-mobile" ]
                        [ div [ class "level-left" ]
                            [ a [ class "level-item" ]
                                [ span [ class "icon is-medium" ]
                                    [ i [ class "fa fa-github" ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


createCampaignForm : Model -> Html Msg
createCampaignForm model =
    section [ class "hero" ]
        [ div [ class "hero-body", style [ ( "padding", "7rem 1.5rem" ) ] ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third is-offset-one-third" ]
                    [ h1 [ class "title" ] [ text "Create your first Campaign" ]
                    , viewErrors model.errors
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "FundingGoal" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
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
                            [ input
                                [ class "input"
                                , onInput UpdateLongDescriptionField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "ShortDescription" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , onInput UpdateShortDescriptionField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field is-grouped" ]
                        [ p [ class "control" ]
                            [ button [ class "button is-primary", onClick SaveCampaignForm, onClick RequestDate ]
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
            ( { model | longDescription = updatedLongDescription }, Cmd.none ) => NoOp

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
            ( model, Task.perform ReceiveDate now ) => NoOp

        ReceiveDate date ->
            let
                stockDateTime =
                    DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }

                convertedTimeDate =
                    stockDateTime

                updatedModel =
                    { model | fundingEndDate = convertedTimeDate }
            in
            ( updatedModel, Cmd.none ) => NoOp


postCampaign : Model -> Cmd Msg
postCampaign model =
    let
        data =
            { currentFunding = model.currentFunding
            , shortDescription = model.shortDescription
            , longDescription = model.longDescription
            , fundingGoal = model.fundingGoal
            , fundingEndDate = model.fundingEndDate
            , userId = model.userId
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) "http://localhost:4000/campaigns" HandleCampaign Campaign.decoder (Campaign.encode data)


validate : Model -> List Error
validate =
    Validate.all
        [ .shortDescription >> ifBlank (ShortDescription => "Short Description can't be blank.")
        , .longDescription >> ifBlank (LongDescription => "Long Description can't be blank.")

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
