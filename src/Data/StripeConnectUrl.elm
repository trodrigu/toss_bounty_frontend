module Data.StripeConnectUrl exposing (StripeConnectUrl, decoder)

import Json.Decode as Decode exposing (Decoder, succeed, Value)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)
import RemoteData.Http exposing (..)


type alias StripeConnectUrl =
    { url : String }


decoder : Decoder StripeConnectUrl
decoder =
    succeed StripeConnectUrl
        |> optionalAt [ "data", "attributes", "url" ] Decode.string ""
