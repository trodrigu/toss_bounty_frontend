module ReposTests exposing (..)

import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, mostBountifulRepo)
import Expect exposing (Expectation)
import Test exposing (..)


listOfRepos : Repos
listOfRepos =
    { repos = [ repo ] }


repo : Repo
repo =
    Repo "a title" "an image src"


all : Test
all =
    describe "mostBountifulRepo"
        [ test "returns one repo" <|
            \() ->
                listOfRepos
                    |> mostBountifulRepo
                    |> Expect.equal repo
        ]
