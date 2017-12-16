module Data.Repos exposing (Repos, mostBountifulRepo)

import Data.Repo as Repo
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Repos =
    { repos : List Repo }


decoder : Decoder Repos
decoder =
    decode Repos
        |> optionalAt [ "data" ] (Decode.list decoder) []
