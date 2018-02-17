module Data.Subscription exposing (Subscription, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Subscription =
    { id : String
    , uuid : String
    }


decoder : Decoder Subscription
decoder =
    decode Subscription
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "uuid" ] Decode.string


encode : { r | customerId : String, planId : String } -> Value
encode subscription =
    let
        plan_attributes =
            Encode.object
                [ ( "type", Encode.string "plan" )
                , ( "id", Encode.string subscription.planId )
                ]

        customer_attributes =
            Encode.object
                [ ( "type", Encode.string "customer" )
                , ( "id", Encode.string subscription.customerId )
                ]

        customer_data_attribute =
            Encode.object [ ( "data", customer_attributes ) ]

        plan_data_attribute =
            Encode.object [ ( "data", plan_attributes ) ]

        relationships =
            Encode.object
                [ ( "customer", customer_data_attribute )
                , ( "plan", plan_data_attribute )
                ]

        data_attributes =
            Encode.object
                [ ( "type", Encode.string "subscription" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]
