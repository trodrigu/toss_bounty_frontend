module Data.Repos exposing (Repos, decoder, mostBountifulRepo)

import Data.Repo as Repo exposing (Repo, decoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optionalAt, requiredAt)
import Json.Encode as Encode exposing (Value)
import List exposing (head, reverse)


type alias Repos =
    { repos : List Repo }


decoder : Decoder Repos
decoder =
    decode Repos
        |> optionalAt [ "data" ] (Decode.list Repo.decoder) []


mostBountifulRepo : Repos -> Repo
mostBountifulRepo repos =
    let
        repo =
            List.sortBy .bountifulScore repos.repos
                |> reverse
                |> head
    in
    case repo of
        Just repo ->
            repo

        Nothing ->
            Repo "" "" 0
