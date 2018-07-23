module Pages.CreateCampaign exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback, toString)
import Data.Campaign as Campaign exposing (..)
import Data.Issue as Issue exposing (Issue)
import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, SelectListRepos, mostBountifulRepo)
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
import Util exposing ((=>))
import Validate exposing (ifBlank)


init : AuthToken -> String -> WebData SelectListRepos -> Maybe String -> Model
init token userId repos apiUrl =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { currentFunding = 0.0
    , errors = []
    , fundingGoal = 0.0
    , longDescription = ""
    , token = token
    , userId = userId
    , bountifulRepos = repos
    , apiUrl = url
    }


type alias Model =
    { currentFunding : Float
    , fundingGoal : Float
    , longDescription : String
    , token : AuthToken
    , errors : List Error
    , userId : String
    , bountifulRepos : WebData SelectListRepos
    , apiUrl : String
    }


type Msg
    = SaveCampaignForm
    | UpdateFundingGoalField String
    | UpdateLongDescriptionField String
    | HandleCampaign (WebData Campaign)
    | CreateCampaign
    | SelectRepo (Maybe Int)


type ExternalMsg
    = NoOp
    | GoToStripeSignUp


view : Model -> Html Msg
view model =
    maintainerHero model


maintainerHero : Model -> Html Msg
maintainerHero model =
    let
        repoList =
            case model.bountifulRepos of
                NotAsked ->
                    [ div [ class "pageloader is-active" ] [] ]

                Failure err ->
                    [ div [ class "pageloader is-active" ] [] ]

                Loading ->
                    [ div [ class "pageloader is-active" ] [] ]

                Success bountifulRepos ->
                    let
                        repos =
                            bountifulRepos
                                |> .selectListRepos
                                |> SelectList.toList
                    in
                    [ div [ class "title" ]
                        [ text "Pick your Github Repo" ]
                    , div [ class "control" ]
                        [ div [ class "select" ]
                            [ Html.select [ onChange ] (List.map (\el -> makeOption el.name) repos)
                            ]
                        ]
                    ]
    in
    div []
        [ section [ class "hero is-medium is-primary is-bold" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-half is-offset-one-quarter" ]
                            repoList
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
        innerMedia =
            case model.bountifulRepos of
                NotAsked ->
                    [ div [ class "pageloader is-active" ] [] ]

                Failure err ->
                    [ div [ class "pageloader is-active" ] [] ]

                Loading ->
                    [ div [ class "pageloader is-active" ] [] ]

                Success bountifulRepos ->
                    let
                        imageSrc =
                            bountifulRepos
                                |> .selectListRepos
                                |> SelectList.selected
                                |> .image

                        name =
                            bountifulRepos
                                |> .selectListRepos
                                |> SelectList.selected
                                |> .owner
                    in
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
    in
    section [ class "section" ]
        [ div
            [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ article [ class "media" ]
                        innerMedia
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
                            [ button [ class "button is-primary", onClick CreateCampaign ]
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

        HandleCampaign data ->
            case data of
                Success campaign ->
                    model
                        => Cmd.none
                        => GoToStripeSignUp

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

        SelectRepo Nothing ->
            ( model, Cmd.none ) => NoOp

        SelectRepo (Just index) ->
            let
                updatedRepos =
                    case model.bountifulRepos of
                        NotAsked ->
                            SelectList.singleton Repo.default

                        Failure err ->
                            SelectList.singleton Repo.default

                        Loading ->
                            SelectList.singleton Repo.default

                        Success bountifulRepos ->
                            let
                                repos =
                                    bountifulRepos
                                        |> .selectListRepos
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
                            in
                            SelectList.singleton foundRepo
                                |> SelectList.prepend beforesList
                                |> SelectList.append aftersWithoutFoundRepo
            in
            ( { model | bountifulRepos = Success { selectListRepos = updatedRepos } }, Cmd.none ) => NoOp

        CreateCampaign ->
            ( model, postCampaign model ) => NoOp


postCampaign : Model -> Cmd Msg
postCampaign model =
    let
        campaignUrl =
            model.apiUrl ++ "/campaigns"

        githubRepoId =
            case model.bountifulRepos of
                NotAsked ->
                    "0"

                Failure err ->
                    "0"

                Loading ->
                    "0"

                Success bountifulRepos ->
                    bountifulRepos
                        |> .selectListRepos
                        |> SelectList.selected
                        |> .id

        data =
            { currentFunding = model.currentFunding
            , longDescription = model.longDescription
            , fundingGoal = model.fundingGoal
            , userId = model.userId
            , githubRepoId = githubRepoId
            }
    in
    RemoteData.Http.postWithConfig (Auth.config model.token) campaignUrl HandleCampaign Campaign.showDecoder (Campaign.encode data)


validate : Model -> List Error
validate =
    Validate.all
        [ .longDescription >> ifBlank (LongDescription => "Summary can't be blank.")
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
