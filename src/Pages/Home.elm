module Pages.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)

type alias Model = {}

type Msg
    = NoOp

init : Model
init =
    {}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
  section [ class "hero is-primary" ]
          [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                [ div [ class "container" ]
                      [ div [ class "columns is-vcentered" ]
                            [ div [ class "column has-text-centered" ]
                                  [ p [ class "title" ]
                                      [ text "For tossers" ]
                                  , p [ class "subtitle" ]
                                      [ text "prime the"
                                      , strong [][ text " pump"]
                                      ]
                                  , button [ class "button" ]
                                           [ text "Start a bounty" ]
                                  ]
                          ]
                    ]
              ]
        ]
