module Pages.CreateCampaign exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback, toString)
import Data.Campaign as Campaign exposing (..)
import Data.Issue as Issue exposing (Issue)
import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, mostBountifulRepo)
import Date exposing (Date, now)
import Html exposing (..)
import Html.Attributes exposing (alt, class, datetime, href, src, style)
import Html.Events exposing (on, onClick, onInput)
import Html.Events.Extra exposing (targetSelectedIndex)
import Json.Decode
import List.Extra exposing (getAt)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Task
import Time.DateTime as DateTime exposing (DateTime, dateTime)
import Util exposing ((=>))
import Validate exposing (ifBlank)


init : AuthToken -> String -> Repos -> Maybe String -> Model
init token userId repos apiUrl =
    let
        bountifulRepo =
            Repos.mostBountifulRepo repos

        otherRepos =
            List.filter (\repo -> not (repo == bountifulRepo)) repos.repos

        reposAsSelectList =
            SelectList.singleton bountifulRepo
                |> SelectList.append otherRepos

        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { currentFunding = 0.0
    , errors = []
    , fundingEndDate = DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }
    , fundingGoal = 0.0
    , longDescription = ""
    , shortDescription = ""
    , token = token
    , userId = userId
    , bountifulRepos = reposAsSelectList
    , apiUrl = url
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
    , bountifulRepos : SelectList Repo
    , apiUrl : String
    }


type Msg
    = SaveCampaignForm
    | UpdateFundingGoalField String
    | UpdateLongDescriptionField String
    | UpdateShortDescriptionField String
    | HandleCampaign (WebData Campaign)
    | RequestDate
    | ReceiveDate Date.Date
    | SelectRepo (Maybe Int)


type ExternalMsg
    = NoOp
    | FetchCampaigns


view : Model -> Html Msg
view model =
    maintainerHero model


maintainerHero : Model -> Html Msg
maintainerHero model =
    let
        name =
            model.bountifulRepos
                |> SelectList.selected
                |> .name

        imageSrc =
            model.bountifulRepos
                |> SelectList.selected
                |> .image

        owner =
            model.bountifulRepos
                |> SelectList.selected
                |> .owner

        repos =
            model.bountifulRepos
                |> SelectList.toList
    in
    div []
        [ section [ class "hero is-medium is-primary is-bold" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-half is-offset-one-quarter" ]
                            [ div [ class "title" ]
                                [ text "Pick your Github Repo" ]
                            , div [ class "control" ]
                                [ div [ class "select" ]
                                    [ Html.select [ onChange ] (List.map (\el -> makeOption el.name) repos)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , createCampaignForm model
        , bioAndPic model
        ]


onChange : Attribute Msg
onChange =
    on "change" (Json.Decode.map SelectRepo Html.Events.Extra.targetSelectedIndex)


makeOption : String -> Html Msg
makeOption name =
    option [] [ text name ]


issuesView : List Issue -> Html Msg
issuesView issues =
    div [] <|
        List.map issueView issues


issueView : Issue -> Html Msg
issueView issue =
    section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
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
    let
        imageSrc =
            model.bountifulRepos
                |> SelectList.selected
                |> .image

        name =
            model.bountifulRepos
                |> SelectList.selected
                |> .owner
    in
    section [ class "section" ]
        [ div
            [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ article [ class "media" ]
                        [ figure [ class "media-left" ]
                            [ p [ class "image is-128x128" ]
                                [ img [ src imageSrc ]
                                    []
                                ]
                            ]
                        , div [ class "media-content" ]
                            [ div [ class "content" ]
                                [ p []
                                    [ strong []
                                        [ text name ]
                                    , br []
                                        []
                                    , text "Does code push you?"
                                    ]
                                ]
                            , nav [ class "level is-mobile" ]
                                [ div [ class "level-left" ]
                                    [ a [ class "level-item" ]
                                        [ span [ class "icon is-medium" ]
                                            [ i [ class "fa fa-github-alt" ]
                                                []
                                            ]
                                        ]
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
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ h1 [ class "title" ] [ text "Tell us a little about your Campaign" ]
                    , viewErrors model.errors
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Funding Goal" ]
                        , p [ class "control has-icons-left" ]
                            [ input
                                [ class "input"
                                , Html.Attributes.type_ "number"
                                , onInput UpdateFundingGoalField
                                ]
                                []
                            , span [ class "icon is-left" ]
                                [ i [ class "fas fa-money-bill-alt" ] []
                                ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "A cool headline" ]
                        , p [ class "control" ]
                            [ input
                                [ class "input"
                                , onInput UpdateShortDescriptionField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Summary" ]
                        , p [ class "control" ]
                            [ textarea
                                [ class "textarea"
                                , onInput UpdateLongDescriptionField
                                ]
                                []
                            ]
                        ]
                    , div [ class "field is-grouped" ]
                        [ p [ class "control" ]
                            [ button [ class "button is-primary", onClick RequestDate ]
                                [ text "Create Campaign" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


defaultRepo : Repo
defaultRepo =
    Repo "" "" "" 0 ""


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
                    model
                        => Cmd.none
                        => FetchCampaigns

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

        SelectRepo Nothing ->
            ( model, Cmd.none ) => NoOp

        SelectRepo (Just index) ->
            let
                repos =
                    model.bountifulRepos
                        |> SelectList.toList

                splitList =
                    List.Extra.splitAt index repos

                beforesList =
                    splitList
                        |> Tuple.first

                aftersList =
                    splitList
                        |> Tuple.second

                foundRepo =
                    aftersList
                        |> List.head
                        |> Maybe.withDefault defaultRepo

                aftersWithoutFoundRepo =
                    aftersList
                        |> List.Extra.uncons
                        |> Maybe.withDefault ( defaultRepo, [] )
                        |> Tuple.second

                updatedRepos =
                    SelectList.singleton foundRepo
                        |> SelectList.prepend beforesList
                        |> SelectList.append aftersWithoutFoundRepo
            in
            ( { model | bountifulRepos = updatedRepos }, Cmd.none ) => NoOp

        ReceiveDate time ->
            let
                year =
                    Date.year time

                monthAsInt =
                    time
                        |> Date.month
                        |> monthToInt

                day =
                    Date.day time

                hour =
                    Date.hour time

                minute =
                    Date.minute time

                second =
                    Date.second time

                stockDateTime =
                    DateTime.dateTime { year = year, month = monthAsInt, day = day, hour = hour, minute = minute, second = second, millisecond = 0 }

                dateTimeMonthFromNow =
                    DateTime.addMonths 1 stockDateTime

                updatedModel =
                    { model | fundingEndDate = dateTimeMonthFromNow }
            in
            ( updatedModel, postCampaign updatedModel ) => NoOp


postCampaign : Model -> Cmd Msg
postCampaign model =
    let
        campaignUrl =
            model.apiUrl ++ "/campaigns"

        githubRepoId =
            model.bountifulRepos
                |> SelectList.selected
                |> .id

        data =
            { currentFunding = model.currentFunding
            , shortDescription = model.shortDescription
            , longDescription = model.longDescription
            , fundingGoal = model.fundingGoal
            , fundingEndDate = model.fundingEndDate
            , userId = model.userId
            , githubRepoId = githubRepoId
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) campaignUrl HandleCampaign Campaign.showDecoder (Campaign.encode data)


validate : Model -> List Error
validate =
    Validate.all
        [ .shortDescription >> ifBlank (ShortDescription => "Short Description can't be blank.")
        , .longDescription >> ifBlank (LongDescription => "Long Description can't be blank.")
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


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12
