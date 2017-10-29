module Pages.NotFound exposing (..)

import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)

init : Model
init = {}

type alias Model = {}

type Msg =
    NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )

view : Html Msg
view =
    main_ [ id "content", class "container" ]
        [ h1 [] [ text "Not Found" ]
        , div [ class "row" ]
            []
        ]
