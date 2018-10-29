module Data.Issues exposing (Issues, decoder)

import Data.Issue as Issue exposing (Issue, decoder)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Issues =
    { issues : List Issue }


decoder : Decoder Issues
decoder =
    succeed Issues
        |> optionalAt [ "data" ] (Decode.list Issue.decoder) []
