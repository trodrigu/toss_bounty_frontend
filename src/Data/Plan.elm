module Data.Plan exposing (Plan, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)


type alias Plan =
    { id : String
    , uuid : String
    , amount : Float
    , interval : String
    , name : String
    , currency : String
    }


decoder : Decoder Plan
decoder =
    decode Plan
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "uuid" ] Decode.string
        |> requiredAt [ "attributes", "amount" ] Decode.float
        |> requiredAt [ "attributes", "interval" ] Decode.string
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> requiredAt [ "attributes", "currency" ] Decode.string
