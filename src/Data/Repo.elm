module Data.Repo exposing (Repo, decoder, default)

import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)


type alias Repo =
    { id : String
    , name : String
    , image : String
    , bountifulScore : Int
    , owner : String
    }


decoder : Decoder Repo
decoder =
    succeed Repo
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> optionalAt [ "attributes", "image" ] Decode.string ""
        |> optionalAt [ "attributes", "bountiful-score" ] Decode.int 0
        |> optionalAt [ "attributes", "owner" ] Decode.string ""


default : Repo
default =
    { id = "0"
    , name = ""
    , image = ""
    , bountifulScore = 0
    , owner = ""
    }
