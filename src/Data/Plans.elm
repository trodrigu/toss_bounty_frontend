module Data.Plans exposing (Plans, decoder, default)

import Data.Plan as Plan exposing (Plan, default, indexDecoder, showDecoder)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)


type alias Plans =
    { plans : List Plan }


decoder : Decoder Plans
decoder =
    succeed Plans
        |> optionalAt [ "data" ] (Decode.list Plan.indexDecoder) []


default : Plans
default =
    { plans =
        [ Plan.default
        ]
    }
