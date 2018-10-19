module Data.Campaign exposing (Campaign, default, encode, indexDecoder, showDecoder)

import Json.Decode as Decode exposing (Decoder, map, succeed)
import Json.Encode as Encode exposing (Value)
import Json.Decode.Pipeline as Pipeline exposing (custom, optionalAt, requiredAt)


type alias Campaign =
    { id : Int
    , currentFunding : Float
    , longDescription : String
    , fundingGoal : Float
    , userId : String
    , githubRepoId : String
    }


decoder : Decoder Campaign
decoder =
    succeed Campaign
        |> requiredAt [ "data", "id" ] campaignIdDecoder
        |> optionalAt [ "data", "attributes", "current-funding" ] Decode.float 0
        |> requiredAt [ "data", "attributes", "long-description" ] Decode.string
        |> requiredAt [ "data", "attributes", "funding-goal" ] Decode.float
        |> optionalAt [ "relationships", "user", "data", "id" ] Decode.string ""
        |> optionalAt [ "relationships", "github-repo", "data", "id" ] Decode.string ""


campaignIdDecoder : Decoder Int
campaignIdDecoder =
    Decode.string
        |> map (\campaignId -> String.toInt campaignId |> Maybe.withDefault 0)


encode : { r | longDescription : String, fundingGoal : Float, userId : String, githubRepoId : String } -> Encode.Value
encode campaign =
    let
        campaign_attributes =
            Encode.object
                [ ( "long_description", Encode.string campaign.longDescription )
                , ( "funding_goal", Encode.float campaign.fundingGoal )
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
    succeed Campaign
        |> requiredAt [ "id" ] campaignIdDecoder
        |> optionalAt [ "attributes", "current-funding" ] Decode.float 0
        |> requiredAt [ "attributes", "long-description" ] Decode.string
        |> requiredAt [ "attributes", "funding-goal" ] Decode.float
        |> optionalAt [ "relationships", "user", "data", "id" ] Decode.string ""
        |> optionalAt [ "relationships", "github-repo", "data", "id" ] Decode.string ""


showDecoder : Decoder Campaign
showDecoder =
    succeed Campaign
        |> requiredAt [ "data", "id" ] campaignIdDecoder
        |> optionalAt [ "data", "attributes", "current-funding" ] Decode.float 0
        |> requiredAt [ "data", "attributes", "long-description" ] Decode.string
        |> requiredAt [ "data", "attributes", "funding-goal" ] Decode.float
        |> optionalAt [ "data", "relationships", "user", "data", "id" ] Decode.string ""
        |> optionalAt [ "data", "relationships", "github-repo", "data", "id" ] Decode.string ""


default : Campaign
default =
    Campaign 0 0.0 "" 0.0 "" ""
