module Data.Issue exposing (Issue, decoder)

import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Issue =
    { title : String
    , body : String
    }


decoder : Decoder Issue
decoder =
    succeed Issue
        |> requiredAt [ "attributes", "title" ] Decode.string
        |> optionalAt [ "attributes", "body" ] Decode.string ""
