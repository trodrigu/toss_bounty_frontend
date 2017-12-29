module Data.Rewards exposing (Rewards, decoder)

import Data.Rewards as Rewards exposing (Rewards, decoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Rewards =
    { issues : List Reward }


decoder : Decoder Rewards
decoder =
    decode Rewards
        |> optionalAt [ "data" ] (Decode.list Reward.decoder) []
