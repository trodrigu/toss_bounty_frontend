module Data.Subscriptions exposing (Subscriptions, decoder, default)

import Data.Subscription as Subscription exposing (Subscription, default, indexDecoder, showDecoder)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)


type alias Subscriptions =
    { subscriptions : List Subscription }


decoder : Decoder Subscriptions
decoder =
    succeed Subscriptions
        |> optionalAt [ "data" ] (Decode.list Subscription.indexDecoder) []


default : Subscriptions
default =
    { subscriptions =
        [ Subscription.default
        ]
    }
