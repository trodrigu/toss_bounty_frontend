module Data.Campaigns exposing (Campaigns, decoder)

import Data.Campaign as Campaign exposing (Campaign, indexDecoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)
import Time.DateTime as DateTime exposing (DateTime, fromISO8601, toISO8601)


type alias Campaigns =
    { campaigns : List Campaign }


decoder : Decoder Campaigns
decoder =
    decode Campaigns
        |> optionalAt [ "data" ] (Decode.list Campaign.indexDecoder) []
