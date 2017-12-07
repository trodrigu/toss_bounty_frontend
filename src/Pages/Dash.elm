module Pages.Dash exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style, src)
import Html.Events exposing (onInput, onClick)
import Navigation
import Data.Session as Session exposing (Session)
import Util exposing ((=>))

type alias Model =
    { bounties : List Bounty
    , currentBounty : Int
    , showNewBountyForm : Bool }

init : Model
init =
    emptyDash

emptyDash : Model
emptyDash =
    { bounties = []
    , currentBounty = 0
    , showNewBountyForm = False }

type alias Bounty =
    { repoName : String
    , repoBio : String
    , dollars : Int }

type Msg
    = SaveNewBountyForm
    | ShowNewBountyForm
    | UpdateRepoNameField String

type ExternalMsg
    = NoOp

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        ShowNewBountyForm ->
            -- increment currentBounty by 1
            let
                incrementedBounty =
                    model.currentBounty + 1

            in
            ( { model | currentBounty = incrementedBounty, showNewBountyForm = True }, Cmd.none ) => NoOp

        UpdateRepoNameField str ->
            -- let
            --     -- find the current bounty
            --     model.bounties
            ( model, Cmd.none ) => NoOp

        SaveNewBountyForm ->
            ( model, Cmd.none ) => NoOp

view : Session -> Model -> Html Msg
view session model =
    div
        []
        [ highlyDesiredBounties
        , yourBounties
        ]

yourBounties : Html Msg
yourBounties =
    section
        [ class "section" ]
        [
         div
             [ class "container" ]
             [ h1
                   []
                   [ text "Your Bounties" ]
             , div
                   [ class "columns" ]
                   [ div
                         [ class "column" ][ dashView ]
                   , div
                         [ class "column" ][ dashView ]
                   ]
             ]
        ]

highlyDesiredBounties : Html Msg
highlyDesiredBounties =
    section
        [ class "section" ]
        [
         div
             [ class "container" ]
             [ h1
                   []
                   [ text "Highly Desired Bounties" ]
             , div
                   [ class "columns" ]
                   [ div
                         [ class "column" ][ dashView ]
                   , div
                         [ class "column" ][ dashView ]
                   ]
             ]
        ]

dashView : Html Msg
dashView =
    div
        [ class "box" ]
        [ article
              [ class "media" ]
              [
                div
                    [ class "media-left" ]
                    [
                    figure
                        [ class "image"]
                        [ img
                          [ src "http://placekitten.com.s3.amazonaws.com/homepage-samples/96/139.jpg" ]
                          []
                        ]
                    ]
              , div
                  [ class "media-content" ]
                  [ div
                      [ class "content" ]
                      [ p
                        []
                        [ h1
                          []
                          [ text "Elixir Lang"]
                        , strong
                          []
                          [ text "Infinite loop when compiling eex template"]
                        , small
                          []
                          [ text " #6607"]
                        , br [][]
                        , strong
                          []
                          [ text "$225"]
                        ]
                      ]
                  , nav
                      [ class "level is-mobile" ]
                      [
                      div
                          [ class "level-left" ]
                          [
                                a
                                    [ class "level-item" ]
                                    [
                                        span
                                            [ class "icon is-medium" ]
                                            [ i
                                              [ class "fa fa-gift" ][]
                                            ]
                                    ]
                          ]
                      ]
                  ]
              ]
        ]
