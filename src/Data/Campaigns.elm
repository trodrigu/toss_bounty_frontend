module Data.Campaigns exposing (Campaigns, IncludedRepo, IncludedStuff(..), decoder, includedRepoDefault)

import Data.AuthToken as AuthToken exposing (AuthToken, decoder, fallback, init)
import Data.Campaign as Campaign exposing (Campaign, indexDecoder)
import Data.Repo as Repo exposing (Repo)
import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder, map, oneOf)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Time.DateTime as DateTime exposing (DateTime, fromISO8601, toISO8601)


type alias Campaigns =
    { included : List IncludedStuff
    , campaigns : List Campaign
    }


decoder : Decoder Campaigns
decoder =
    decode Campaigns
        |> optionalAt [ "included" ] (Decode.list includedDecoder) []
        |> optionalAt [ "data" ] (Decode.list Campaign.indexDecoder) []


type IncludedStuff
    = IncludedGithub IncludedRepo
    | IncludedStripe IncludedUser


type alias IncludedRepo =
    { id : String
    , name : String
    , image : String
    , bountifulScore : Int
    , owner : String
    }


includedRepoDefault : IncludedStuff
includedRepoDefault =
    IncludedGithub
        { id = ""
        , name = ""
        , image = ""
        , bountifulScore = 0
        , owner = ""
        }


type alias IncludedUser =
    { email : String
    , userId : String
    , stripeExternalId : String
    }


includedDecoder : Decoder IncludedStuff
includedDecoder =
    oneOf
        [ Decode.map IncludedGithub repoIncludedDecoder
        , Decode.map IncludedStripe userIncludedDecoder
        ]


repoIncludedDecoder : Decoder IncludedRepo
repoIncludedDecoder =
    decode IncludedRepo
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> optionalAt [ "attributes", "image" ] Decode.string ""
        |> optionalAt [ "attributes", "bountiful-score" ] Decode.int 0
        |> optionalAt [ "attributes", "owner" ] Decode.string ""


userIncludedDecoder : Decoder IncludedUser
userIncludedDecoder =
    decode IncludedUser
        |> requiredAt [ "attributes", "email" ] Decode.string
        |> optionalAt [ "attributes", "user_id" ] Decode.string ""
        |> optionalAt [ "attributes", "stripe-external-id" ] Decode.string ""
