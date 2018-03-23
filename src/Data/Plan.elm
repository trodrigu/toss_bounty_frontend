module Data.Plan exposing (Plan, default, encode, indexDecoder, showDecoder, updateEncode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


encode : { r | amount : Float, interval : String, name : String, currency : String, rewardId : Int } -> Value
encode plan =
    let
        plan_attributes =
            Encode.object
                [ ( "amount", Encode.float plan.amount )
                , ( "interval", Encode.string plan.interval )
                , ( "name", Encode.string plan.name )
                , ( "currency", Encode.string plan.currency )
                ]

        reward_attributes =
            Encode.object
                [ ( "type", Encode.string "reward" )
                , ( "id", Encode.int plan.rewardId )
                ]

        reward_data_attribute =
            Encode.object [ ( "data", reward_attributes ) ]

        relationships =
            Encode.object
                [ ( "reward", reward_data_attribute )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", plan_attributes )
                , ( "type", Encode.string "plan" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


updateEncode : { r | name : String, rewardId : Int } -> Value
updateEncode plan =
    let
        plan_attributes =
            Encode.object
                [ ( "name", Encode.string plan.name )
                ]

        reward_attributes =
            Encode.object
                [ ( "type", Encode.string "reward" )
                , ( "id", Encode.int plan.rewardId )
                ]

        reward_data_attribute =
            Encode.object [ ( "data", reward_attributes ) ]

        relationships =
            Encode.object
                [ ( "reward", reward_data_attribute )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", plan_attributes )
                , ( "type", Encode.string "plan" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


type alias Plan =
    { id : String
    , uuid : String
    , amount : Float
    , interval : String
    , name : String
    , currency : String
    , rewardId : Int
    }


showDecoder : Decoder Plan
showDecoder =
    decode Plan
        |> requiredAt [ "data", "id" ] Decode.string
        |> requiredAt [ "data", "attributes", "uuid" ] Decode.string
        |> requiredAt [ "data", "attributes", "amount" ] Decode.float
        |> requiredAt [ "data", "attributes", "interval" ] Decode.string
        |> requiredAt [ "data", "attributes", "name" ] Decode.string
        |> requiredAt [ "data", "attributes", "currency" ] Decode.string
        |> optionalAt [ "data", "relationships", "reward", "data", "id" ] Decode.int 0


indexDecoder : Decoder Plan
indexDecoder =
    decode Plan
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "uuid" ] Decode.string
        |> requiredAt [ "attributes", "amount" ] Decode.float
        |> requiredAt [ "attributes", "interval" ] Decode.string
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> requiredAt [ "attributes", "currency" ] Decode.string
        |> optionalAt [ "attributes", "reward-id" ] Decode.int 0


default : Plan
default =
    { id = "0"
    , uuid = ""
    , amount = 0.0
    , interval = ""
    , name = ""
    , currency = ""
    , rewardId = 0
    }
