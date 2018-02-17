module Data.Stripe exposing (Stripe, decoder, default, encode, showDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, optionalAt)
import Json.Encode as Encode exposing (Value)


type alias Stripe =
    { id : String
    , uuid : String
    }


decoder : Decoder Stripe
decoder =
    decode Stripe
        |> optional "id" Decode.string ""
        |> optional "uuid" Decode.string ""


showDecoder : Decoder Stripe
showDecoder =
    decode Stripe
        |> optionalAt [ "data", "id" ] Decode.string ""
        |> optionalAt [ "data", "attributes", "uuid" ] Decode.string ""


encode : { r | uuid : String, userId : String } -> Encode.Value
encode stripe =
    let
        stripe_attributes =
            Encode.object
                [ ( "uuid", Encode.string stripe.uuid )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", stripe_attributes )
                , ( "type", Encode.string "token" )
                , ( "relationships", relationships )
                ]

        relationships =
            Encode.object
                [ ( "user", user_data_attribute ) ]

        user_data_attribute =
            Encode.object [ ( "data", user_attributes ) ]

        user_attributes =
            Encode.object
                [ ( "type", Encode.string "user" )
                , ( "id", Encode.string stripe.userId )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


default : Stripe
default =
    { id = ""
    , uuid = ""
    }
