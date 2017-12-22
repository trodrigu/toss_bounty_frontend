module Data.Repo exposing (Repo, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)


type alias Repo =
    { id : String
    , name : String
    , image : String
    , bountifulScore : Int
    , owner : String
    }


decoder : Decoder Repo
decoder =
    decode Repo
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> optionalAt [ "attributes", "image" ] Decode.string ""
        |> optionalAt [ "attributes", "bountiful-score" ] Decode.int 0
        |> optionalAt [ "attributes", "owner" ] Decode.string ""
