module Pages.Discover exposing (ExternalMsg(..), Model, Msg(..), columnsWrapper, displayCampaignFormHeader, displayFormContent, displayFormContentWithoutButton, displayStripePayment, fetchAllCampaigns, getSearch, init, navWithPageNumbers, renderPageNumber, renderPageNumbers, renderSearchBar, showYourCampaign, update, view)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Campaign as Campaign exposing (Campaign, default)
import Data.Campaigns as Campaigns exposing (Campaigns, IncludedStuff(..), default, includedRepoDefault)
import Data.Repo as Repo exposing (Repo)
import Data.Rewards as Rewards exposing (Rewards, decoder)
import Data.User as User exposing (User, decoder)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, placeholder, src, style, type_)
import Html.Events exposing (onClick, onInput)
import List.Extra as ListExtra exposing (greedyGroupsOf)
import Ports exposing (createStripeElement)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Request.Auth as Auth exposing (config)
import Routing.Router as Router exposing (Route(..))
import SelectList as SelectList exposing (SelectList, append, select, selected, singleton)
import Http as Http exposing (Error(..), Response)
import Browser.Navigation as Navigation exposing (Key)


init : Key -> AuthToken -> Maybe String -> WebData Campaigns -> Model
init key token apiUrl campaigns =
    let
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just matchedUrl ->
                    matchedUrl
    in
    { campaigns = campaigns
    , token = token
    , apiUrl = url
    , showForm = False
    , user = NotAsked
    , pageNumber = 1
    , pageSize = 4
    , search = ""
    , key = key
    }


type alias Model =
    { campaigns : WebData Campaigns
    , token : AuthToken
    , apiUrl : String
    , showForm : Bool
    , user : WebData User
    , pageNumber : Int
    , pageSize : Int
    , search : String
    , key : Key
    }


type Msg
    = SelectYourCampaign Int
    | HandleFetchAllCampaigns (WebData Campaigns)
    | GetCampaigns
    | HandleFetchUser (WebData User)
    | UpdatePage Int
    | PreviousPage
    | NextPage
    | SendSearch String


type ExternalMsg
    = NoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        SendSearch searchString ->
            let
                updatedModel =
                    { model | search = searchString }
            in
            (( updatedModel, getSearch updatedModel ), NoOp)

        PreviousPage ->
            let
                campaigns =
                    case model.campaigns of
                        Success matchedCampaigns ->
                            matchedCampaigns

                        _ ->
                            Campaigns.default

                totalPages =
                    campaigns.totalPages

                previousPage =
                    if model.pageNumber - 1 == 0 then
                        totalPages

                    else
                        model.pageNumber - 1
            in
            update (UpdatePage previousPage) model

        NextPage ->
            let
                campaigns =
                    case model.campaigns of
                        Success matchedCampaigns ->
                            matchedCampaigns

                        _ ->
                            Campaigns.default

                totalPages =
                    campaigns.totalPages

                nextPage =
                    if model.pageNumber + 1 > totalPages then
                        1

                    else
                        model.pageNumber + 1
            in
            update (UpdatePage nextPage) model

        UpdatePage pageNumber ->
            let
                updatedModel =
                    { model | pageNumber = pageNumber }
            in
            (( updatedModel, fetchAllCampaigns updatedModel ), NoOp)

        HandleFetchUser updatedUser ->
            let
                updatedModel =
                    { model | user = updatedUser }
            in
            (( updatedModel, fetchAllCampaigns updatedModel ), NoOp)

        GetCampaigns ->
            (( { model | campaigns = Loading }, fetchAllCampaigns model ), NoOp)

        HandleFetchAllCampaigns data ->
            (( { model | campaigns = data }, Cmd.none ), NoOp)

        SelectYourCampaign campaignId ->
            (( model, Router.modifyUrl model.key (ContributeRoute campaignId)), NoOp)


view : Model -> Html Msg
view model =
    let
        campaigns =
            model.campaigns

        allCampaigns =
            model.campaigns
    in
    case model.campaigns of
        NotAsked ->
            div [ class "pageloader is-active" ] [ span [ class "title" ] [ text "Loading..." ] ]

        Loading ->
            div [ class "pageloader is-active" ] [ span [ class "title" ] [ text "Loading..." ] ]

        Failure error ->
            div [] [ text ("Failed" ++ (error |> errorToString) ) ]

        Success matchedCampaigns ->
            let
                renderedCampaigns =
                    List.map (\campaign -> showYourCampaign campaign matchedCampaigns.included) matchedCampaigns.campaigns

                campaignsGrouped =
                    ListExtra.greedyGroupsOf 2 renderedCampaigns

                campaignsWithColumnsWrapper =
                    List.map (\campaign -> columnsWrapper campaign) campaignsGrouped
            in
            section [ class "section" ]
                [ div [ class "container" ]
                    ([ h1 [ class "title" ] [ text "Discover Campaigns" ]
                     , renderSearchBar model
                     ]
                        ++ campaignsWithColumnsWrapper
                        ++ navWithPageNumbers model
                    )
                ]


errorToString : Error -> String
errorToString err =
    case err of
        BadUrl urlMessage ->
            "The url " ++ urlMessage ++ "is no bueno!"

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network Error"

        BadStatus _ ->
            "It was a Bad Status."

        BadPayload _ _ ->
            "Payload is not good"

renderSearchBar : Model -> Html Msg
renderSearchBar model =
    div [ class "field has-addons" ]
        [ div [ class "control" ]
            [ input [ class "input", type_ "text", placeholder "Find a campaign", onInput SendSearch ]
                []
            ]
        ]


navWithPageNumbers : Model -> List (Html Msg)
navWithPageNumbers model =
    [ nav [ class "pagination" ]
        [ a [ class "pagination-previous", onClick PreviousPage ]
            [ text "Previous" ]
        , a [ class "pagination-next", onClick NextPage ]
            [ text "Next page" ]
        , ul [ class "pagination-list" ]
            (renderPageNumbers model)
        ]
    ]


renderPageNumbers : Model -> List (Html Msg)
renderPageNumbers model =
    let
        campaigns =
            case model.campaigns of
                Success matchedCampaigns ->
                    matchedCampaigns

                _ ->
                    Campaigns.default

        pageRange =
            List.range 1 campaigns.totalPages
    in
    List.map (\pageNumber -> renderPageNumber pageNumber model.pageNumber) pageRange


renderPageNumber : Int -> Int -> Html Msg
renderPageNumber pageNumber currentPageNumber =
    if pageNumber == currentPageNumber then
        li [ class "pagination-link is-current" ] [ text (String.fromInt pageNumber) ]

    else
        li [ class "pagination-link", onClick (UpdatePage pageNumber) ] [ text (String.fromInt pageNumber) ]


showYourCampaign : Campaign -> List IncludedStuff -> Html Msg
showYourCampaign campaign included =
    let
        repoForCampaign =
            List.filter
                (\includedStuff ->
                    case includedStuff of
                        IncludedGithub includedRepo ->
                            campaign.githubRepoId == includedRepo.id

                        _ ->
                            False
                )
                included
                |> List.head
                |> Maybe.withDefault Campaigns.includedRepoDefault
    in
    div [ class "card" ]
        [ displayCampaignFormHeader repoForCampaign
        , displayFormContent campaign
        ]


displayCampaignFormHeader : IncludedStuff -> Html Msg
displayCampaignFormHeader included =
    let
        repo =
            case included of
                IncludedGithub includedRepo ->
                    includedRepo

                IncludedStripe _ ->
                    { id = ""
                    , name = ""
                    , image = ""
                    , bountifulScore = 0
                    , owner = ""
                    }
    in
    div [ class "card-header" ]
        [ p [ class "card-header-title" ] [ text repo.name ]
        ]


displayStripePayment : Model -> Html Msg
displayStripePayment model =
    let
        formStyle =
            if model.showForm then
                ( "display", "block" )

            else
                ( "display", "none" )
    in
    div [ class "card-content" ]
        [ form
            [ action "/charge", id "payment-form", method "post", (\( a, b ) -> style a b) formStyle ]
            [ div [ class "form-row field" ]
                [ label [ class "label" ]
                    [ text "Credit or debit card    " ]
                , div [ id "card-element" ]
                    [ text "    " ]
                , div [ id "card-errors" ]
                    []
                ]
            , button [ class "button is-success" ]
                [ text "Submit Payment" ]
            ]
        ]


displayFormContent : Campaign -> Html Msg
displayFormContent campaign =
    div [ class "card-content" ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Summary" ]
            , p [ class "control" ]
                [ text campaign.longDescription ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding Goal" ]
            , p []
                [ text (String.fromFloat campaign.fundingGoal) ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Funding Progress" ]
            , progress [ class "progress", Html.Attributes.value (String.fromFloat campaign.currentFunding), Html.Attributes.max (String.fromFloat campaign.fundingGoal) ] [ text (String.fromFloat campaign.currentFunding) ]
            ]
        , a [ id "card-element", class "", onClick (SelectYourCampaign campaign.id) ]
            [ span [] [ text "Select Campaign" ] ]
        ]


displayFormContentWithoutButton : Campaign -> Html Msg
displayFormContentWithoutButton campaign =
    div [ class "card-content" ]
        [ label [ class "label" ]
            [ text "Summary" ]
        , p [ class "control" ]
            [ text (campaign.longDescription) ]
        , label [ class "label" ]
            [ text "Funding Goal" ]
        , p []
            [ text (String.fromFloat campaign.fundingGoal) ]
        ]


fetchAllCampaigns : Model -> Cmd Msg
fetchAllCampaigns model =
    let
        pageSize =
            model.pageSize

        pageNumber =
            model.pageNumber

        updatedUrl =
            model.apiUrl ++ "/campaigns" ++ "?page_size=" ++ String.fromInt model.pageSize ++ "&page=" ++ String.fromInt model.pageNumber

        token =
            model.token
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchAllCampaigns Campaigns.decoder


getSearch : Model -> Cmd Msg
getSearch model =
    let
        search =
            model.search

        pageSize =
            model.pageSize

        pageNumber =
            model.pageNumber

        updatedUrl =
            model.apiUrl ++ "/search" ++ "?page_size=" ++ String.fromInt model.pageSize ++ "&page=" ++ String.fromInt model.pageNumber ++ "&search=" ++ search

        token =
            model.token
    in
    RemoteData.Http.getWithConfig (Auth.config token) updatedUrl HandleFetchAllCampaigns Campaigns.decoder


columnsWrapper : List (Html Msg) -> Html Msg
columnsWrapper campaigns =
    let
        ( firstCampaign, otherCampaigns ) =
            ListExtra.uncons campaigns
                |> Maybe.withDefault ( div [] [], [] )

        ( secondCampaign, emptyCampaigns ) =
            ListExtra.uncons otherCampaigns
                |> Maybe.withDefault ( div [] [], [] )
    in
    div [ class "columns" ]
        [ div [ class "column is-half" ]
            [ firstCampaign
            ]
        , div [ class "column is-half" ]
            [ secondCampaign
            ]
        ]
