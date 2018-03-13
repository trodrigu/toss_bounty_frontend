module Data.Campaign exposing (Campaign, default, defaultDate, encode, indexDecoder, showDecoder)

import Json.Decode as Decode exposing (Decoder, map)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)
import Time.DateTime as DateTime exposing (DateTime, fromISO8601, toISO8601)


type alias Campaign =
    { id : Int
    , currentFunding : Float
    , longDescription : String
    , fundingGoal : Float
    , fundingEndDate : DateTime
    , userId : String
    , githubRepoId : String
    }


decoder : Decoder Campaign
decoder =
    decode Campaign
        |> requiredAt [ "data", "id" ] campaignIdDecoder
        |> optionalAt [ "data", "attributes", "current-funding" ] Decode.float 0
        |> requiredAt [ "data", "attributes", "long-description" ] Decode.string
        |> requiredAt [ "data", "attributes", "funding-goal" ] Decode.float
        |> optionalAt [ "data", "attributes", "funding-end-date" ] dateDecoder (DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 })
        |> optionalAt [ "relationships", "user", "data", "id" ] Decode.string ""
        |> optionalAt [ "relationships", "github-repo", "data", "id" ] Decode.string ""


campaignIdDecoder : Decoder Int
campaignIdDecoder =
    Decode.string
        |> map (\campaignId -> String.toInt campaignId |> Result.withDefault 0)


dateDecoder : Decoder DateTime
dateDecoder =
    let
        convert : String -> Decoder DateTime
        convert raw =
            case DateTime.fromISO8601 raw of
                Ok date ->
                    Decode.succeed date

                Err error ->
                    Decode.fail error
    in
    Decode.string |> Decode.andThen convert


encode : { r | longDescription : String, fundingGoal : Float, fundingEndDate : DateTime, userId : String, githubRepoId : String } -> Encode.Value
encode campaign =
    let
        is8601DateString =
            campaign.fundingEndDate
                |> DateTime.toISO8601

        campaign_attributes =
            Encode.object
                [ ( "long_description", Encode.string campaign.longDescription )
                , ( "funding_goal", Encode.float campaign.fundingGoal )
                , ( "funding_end_date", Encode.string is8601DateString )
                ]

        relationships =
            Encode.object
                [ ( "user", user_data_attribute )
                , ( "github_repo", github_repo_data_attribute )
                ]

        user_data_attribute =
            Encode.object [ ( "data", user_attributes ) ]

        github_repo_data_attribute =
            Encode.object [ ( "data", github_repo_attributes ) ]

        user_attributes =
            Encode.object
                [ ( "type", Encode.string "user" )
                , ( "id", Encode.string campaign.userId )
                ]

        github_repo_attributes =
            Encode.object
                [ ( "type", Encode.string "github_repo" )
                , ( "id", Encode.string campaign.githubRepoId )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", campaign_attributes )
                , ( "type", Encode.string "campaign" )
                , ( "relationships", relationships )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


indexDecoder : Decoder Campaign
indexDecoder =
    decode Campaign
        |> requiredAt [ "id" ] campaignIdDecoder
        |> optionalAt [ "attributes", "current-funding" ] Decode.float 0
        |> requiredAt [ "attributes", "long-description" ] Decode.string
        |> requiredAt [ "attributes", "funding-goal" ] Decode.float
        |> optionalAt [ "attributes", "funding-end-date" ] dateDecoder (DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 })
        |> optionalAt [ "relationships", "user", "data", "id" ] Decode.string ""
        |> optionalAt [ "relationships", "github-repo", "data", "id" ] Decode.string ""


showDecoder : Decoder Campaign
showDecoder =
    decode Campaign
        |> requiredAt [ "data", "id" ] campaignIdDecoder
        |> optionalAt [ "data", "attributes", "current-funding" ] Decode.float 0
        |> requiredAt [ "data", "attributes", "long-description" ] Decode.string
        |> requiredAt [ "data", "attributes", "funding-goal" ] Decode.float
        |> optionalAt [ "data", "attributes", "funding-end-date" ] dateDecoder (DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 })
        |> optionalAt [ "data", "relationships", "user", "data", "id" ] Decode.string ""
        |> optionalAt [ "data", "relationships", "github-repo", "data", "id" ] Decode.string ""


default : Campaign
default =
    Campaign 0 0.0 "" 0.0 defaultDate "" ""


defaultDate : DateTime
defaultDate =
    DateTime.dateTime { year = 1992, month = 5, day = 29, hour = 0, minute = 0, second = 0, millisecond = 0 }
