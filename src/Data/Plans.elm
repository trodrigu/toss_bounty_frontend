module Data.Plans exposing (Plans, decoder, default)

import Data.Plan as Plan exposing (Plan, default, indexDecoder, showDecoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)


type alias Plans =
    { plans : List Plan }


decoder : Decoder Plans
decoder =
    decode Plans
        |> optionalAt [ "data" ] (Decode.list Plan.indexDecoder) []


default : Plans
default =
    { plans =
        [ Plan.default
        ]
    }
