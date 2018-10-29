module Util exposing (appendErrors, viewIf)

import Html exposing (Attribute, Html)
import Json.Decode as Decode



viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content

    else
        Html.text ""


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }
