module Pages.Dash exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput, onClick)
import Navigation
import Util exposing ((=>))

type alias Model =
    { bounties : List Bounty, currentBounty : Int, showNewBountyForm : Bool }

init : Model
init =
    emptyDash

emptyDash : Model
emptyDash =
    { bounties = [], currentBounty = 0, showNewBountyForm = False }

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

view : Model -> Html Msg
view model =
    dashView

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
                        [ class "image is-64x64"]
                        [
                            img
                                []
                                []
                        ]
                    ]
              , div
                  [ class "media-content" ]
                  [
                       div
                           [ class "content" ]
                           [
                                p
                                    []
                                    []
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
                                         [ class "icon is-small" ]
                                         [
                                              i
                                                  [ class "fa fa-reply" ][]
                                         ]
                                ]
                       ]
                  ]
              ]
        ]
