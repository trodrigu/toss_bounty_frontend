module Data.Issues exposing (Issues, decoder)

import Data.Issue as Issue exposing (Issue)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Issues =
    { repos : List Issue }


decoder : Decoder Issues
decoder =
    decode Issues
        |> optionalAt [ "data" ] (Decode.list decoder) []
