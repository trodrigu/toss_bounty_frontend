module Data.Reward exposing (Reward, default, encode, indexDecoder, showDecoder, updateEncode)

import Json.Decode as Decode exposing (Decoder, map)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Reward =
    { id : Int
    , description : String
    , donationLevel : Float
    , planId : String
    }


updateEncode : { r | description : String, campaignId : Int } -> Encode.Value
updateEncode reward =
    let
        reward_attributes =
            Encode.object
                [ ( "description", Encode.string reward.description )
                ]

        relationships =
            Encode.object
                [ ( "campaign", campaign_data_attribute ) ]

        campaign_data_attribute =
            Encode.object [ ( "data", campaign_attributes ) ]

        campaign_attributes =
            Encode.object
                [ ( "type", Encode.string "campaign" )
                , ( "id", Encode.int reward.campaignId )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", reward_attributes )
                , ( "type", Encode.string "reward" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


encode : { r | description : String, donationLevel : Float, campaignId : Int } -> Encode.Value
encode reward =
    let
        reward_attributes =
            Encode.object
                [ ( "description", Encode.string reward.description )
                , ( "donation_level", Encode.float reward.donationLevel )
                ]

        relationships =
            Encode.object
                [ ( "campaign", campaign_data_attribute ) ]

        campaign_data_attribute =
            Encode.object [ ( "data", campaign_attributes ) ]

        campaign_attributes =
            Encode.object
                [ ( "type", Encode.string "campaign" )
                , ( "id", Encode.int reward.campaignId )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", reward_attributes )
                , ( "type", Encode.string "reward" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


rewardIdDecoder : Decoder Int
rewardIdDecoder =
    Decode.string
        |> map (\rewardId -> String.toInt rewardId |> Result.withDefault 0)


showDecoder : Decoder Reward
showDecoder =
    decode Reward
        |> requiredAt [ "data", "id" ] rewardIdDecoder
        |> requiredAt [ "data", "attributes", "description" ] Decode.string
        |> requiredAt [ "data", "attributes", "donation-level" ] Decode.float
        |> optionalAt [ "data", "relationships", "plan", "data", "id" ] Decode.string ""


indexDecoder : Decoder Reward
indexDecoder =
    decode Reward
        |> requiredAt [ "id" ] rewardIdDecoder
        |> requiredAt [ "attributes", "description" ] Decode.string
        |> requiredAt [ "attributes", "donation-level" ] Decode.float
        |> optionalAt [ "relationships", "data", "plan", "id" ] Decode.string ""


default : Reward
default =
    { id = 0
    , donationLevel = 0.0
    , description = "Give love :)"
    , planId = ""
    }
