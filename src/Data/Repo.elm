module Data.Repo exposing (Repo)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Repo =
    { name : String
    , image : String
    }


decoder : Decoder Repo
decoder =
    decode Repo
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> optionalAt [ "attributes", "image" ] Decode.string ""
