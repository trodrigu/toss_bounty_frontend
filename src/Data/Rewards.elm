module Data.Rewards exposing (Rewards, decoder, default)

import Data.Reward as Reward exposing (Reward, default, indexDecoder, showDecoder)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)


type alias Rewards =
    { rewards : List Reward }


decoder : Decoder Rewards
decoder =
    succeed Rewards
        |> optionalAt [ "data" ] (Decode.list Reward.indexDecoder) []


default : Rewards
default =
    { rewards =
        [ Reward.default
        ]
    }
