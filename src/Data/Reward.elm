module Data.Reward exposing (Reward, decoder, default, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Reward =
    { id : String
    , description : String
    , donationLevel : Float
    }


encode : { r | description : String, donationLevel : Float, campaignId : String } -> Encode.Value
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
                , ( "id", Encode.string reward.campaignId )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", reward_attributes )
                , ( "type", Encode.string "reward" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


decoder : Decoder Reward
decoder =
    decode Reward
        |> requiredAt [ "data", "id" ] Decode.string
        |> requiredAt [ "data", "attributes", "description" ] Decode.string
        |> requiredAt [ "data", "attributes", "donation-level" ] Decode.float


default : Reward
default =
    { id = ""
    , donationLevel = 100.0
    , description = ""
    }
