module Data.Repos exposing (Repos, SelectListRepos, decoder, defaultSelectListRepos, mostBountifulRepo, selectListDecoder)

import Data.Repo as Repo exposing (Repo, decoder)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optionalAt, requiredAt)
import List exposing (head, reverse)
import List.Extra exposing (splitAt)
import SelectList as SelectList exposing (SelectList, singleton)


type alias Repos =
    { repos : List Repo }


type alias SelectListRepos =
    { selectListRepos : SelectList Repo
    }


selectList : List Repo -> SelectList Repo
selectList list =
    let
        splitList =
            List.Extra.splitAt 0 list

        beforesList =
            splitList
                |> Tuple.first

        aftersList =
            splitList
                |> Tuple.second

        foundRepo =
            aftersList
                |> List.head
                |> Maybe.withDefault Repo.default

        aftersWithoutFoundRepo =
            aftersList
                |> List.Extra.uncons
                |> Maybe.withDefault ( Repo.default, [] )
                |> Tuple.second

        updatedRepos =
            SelectList.singleton foundRepo
                |> SelectList.prepend beforesList
                |> SelectList.append aftersWithoutFoundRepo
    in
    updatedRepos


selectListDecoder : Decoder SelectListRepos
selectListDecoder =
    succeed SelectListRepos
        |> optionalAt [ "data" ] (Decode.list Repo.decoder |> Decode.map selectList) (SelectList.singleton Repo.default)


defaultSelectListRepos : SelectListRepos
defaultSelectListRepos =
    { selectListRepos = SelectList.singleton Repo.default }





decoder : Decoder Repos
decoder =
    succeed Repos
        |> optionalAt [ "data" ] (Decode.list Repo.decoder) []


mostBountifulRepo : List Repo -> Repo
mostBountifulRepo repos =
    let
        repo =
            List.sortBy .bountifulScore repos
                |> reverse
                |> head
    in
    case repo of
        Just innerRepo ->
            innerRepo

        Nothing ->
            Repo "0" "" "" 0 ""
