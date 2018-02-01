module Data.Subscription exposing (Subscription, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)


type alias Subscription =
    { id : String
    , uuid : String
    }


decoder : Decoder Subscription
decoder =
    decode Subscription
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "uuid" ] Decode.string
