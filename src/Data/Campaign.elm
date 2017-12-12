module Data.Campaign exposing (Campaign, Campaigns, decoder, campaignsDecoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, requiredAt, optionalAt)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Json.Decode.Extra exposing (date)
import UrlParser
import Time.Date as Date exposing (toISO8601, fromISO8601, Date)

type alias Campaigns =
    { campaigns : List Campaign }

type alias Campaign =
    { currentFunding : Float
    , shortDescription : String
    , longDescription : String
    , fundingGoal : Float
    , fundingEndDate : Date
    }

campaignsDecoder : Decoder Campaigns
campaignsDecoder =
    decode Campaigns
        |> optionalAt [ "data" ] ( Decode.list decoder ) []

decoder : Decoder Campaign
decoder =
    decode Campaign
        |> requiredAt [ "attributes", "current_funding" ] Decode.float
        |> requiredAt [ "attributes", "short_description" ] Decode.string
        |> requiredAt [ "attributes", "long_description" ] Decode.string
        |> requiredAt [ "attributes",  "funding_goal" ] Decode.float
        |> optionalAt [ "attributes", "funding_end_date" ] dateDecoder (Date.date 0 0 0)

dateDecoder : Decoder Date
dateDecoder =
    let
        convert : String -> Decoder Date
        convert raw =
            case Date.fromISO8601 raw of
                Ok date ->
                    Decode.succeed date

                Err error ->
                    Decode.fail error
    in
        Decode.string |> Decode.andThen convert

encode : { r | shortDescription : String, longDescription : String, fundingGoal : Float, fundingEndDate : Date.Date } -> Encode.Value
encode campaign =
    let
        is8601DateString =
          campaign.fundingEndDate
                  |> Date.toISO8601

        campaign_attributes =
            Encode.object
                [ ("shortDescription", Encode.string campaign.shortDescription)
                , ("longDescription", Encode.string campaign.longDescription)
                , ("fundingGoal", Encode.float campaign.fundingGoal)
                , ("fundingEndDate", Encode.string is8601DateString) ]

        data_attributes =
            Encode.object [ ( "attributes", campaign_attributes)]
    in
        Encode.object [ ("data", data_attributes) ]
