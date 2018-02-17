module Data.Customer exposing (Customer, decoder, default, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Customer =
    { id : String
    , uuid : String
    , tokenId : String
    }


decoder : Decoder Customer
decoder =
    decode Customer
        |> requiredAt [ "data", "id" ] Decode.string
        |> requiredAt [ "data", "attributes", "uuid" ] Decode.string
        |> optionalAt [ "data", "relationships", "token", "data", "id" ] Decode.string ""


encode : { r | tokenId : String } -> Value
encode customer =
    let
        token_attributes =
            Encode.object
                [ ( "type", Encode.string "token" )
                , ( "id", Encode.string customer.tokenId )
                ]

        token_data_attribute =
            Encode.object [ ( "data", token_attributes ) ]

        relationships =
            Encode.object
                [ ( "token", token_data_attribute )
                ]

        data_attributes =
            Encode.object
                [ ( "type", Encode.string "customer" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


default : Customer
default =
    { id = ""
    , uuid = ""
    , tokenId = ""
    }
