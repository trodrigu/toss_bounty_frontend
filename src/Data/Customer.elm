module Data.Customer exposing (Customer, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)


type alias Customer =
    { id : String
    , uuid : String
    }


decoder : Decoder Customer
decoder =
    decode Customer
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "uuid" ] Decode.string
