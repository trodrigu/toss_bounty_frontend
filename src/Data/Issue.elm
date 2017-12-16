module Data.Issue exposing (Issue)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Issue =
    { title : String
    , body : String
    }


decoder : Decoder Issue
decoder =
    decode Issue
        |> requiredAt [ "attributes", "title" ] Decode.string
        |> optionalAt [ "attributes", "body" ] Decode.string ""
