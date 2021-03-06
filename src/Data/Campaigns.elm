module Data.Campaigns exposing (Campaigns, IncludedRepo, IncludedStuff(..), decoder, default, includedRepoDefault)

import Data.AuthToken as AuthToken exposing (AuthToken, decoder, fallback, init)
import Data.Campaign as Campaign exposing (Campaign, indexDecoder)
import Data.Repo as Repo exposing (Repo)
import Data.User as User exposing (User)
import Json.Decode as Decode exposing (succeed, Decoder, map, oneOf)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)


type alias Campaigns =
    { included : List IncludedStuff
    , campaigns : List Campaign
    , totalPages : Int
    , pageNumber : Int
    }


decoder : Decoder Campaigns
decoder =
    succeed Campaigns
        |> optionalAt [ "included" ] (Decode.list includedDecoder) []
        |> optionalAt [ "data" ] (Decode.list Campaign.indexDecoder) []
        |> optionalAt [ "meta", "total_pages" ] Decode.int 0
        |> optionalAt [ "meta", "page_number" ] Decode.int 0


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
    succeed IncludedRepo
        |> requiredAt [ "id" ] Decode.string
        |> requiredAt [ "attributes", "name" ] Decode.string
        |> optionalAt [ "attributes", "image" ] Decode.string ""
        |> optionalAt [ "attributes", "bountiful-score" ] Decode.int 0
        |> optionalAt [ "attributes", "owner" ] Decode.string ""


userIncludedDecoder : Decoder IncludedUser
userIncludedDecoder =
    succeed IncludedUser
        |> requiredAt [ "attributes", "email" ] Decode.string
        |> optionalAt [ "attributes", "user_id" ] Decode.string ""
        |> optionalAt [ "attributes", "stripe-external-id" ] Decode.string ""


default : Campaigns
default =
    { included = [], campaigns = [], totalPages = 0, pageNumber = 0 }
