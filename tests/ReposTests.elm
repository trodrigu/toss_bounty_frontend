module ReposTests exposing (..)

import Data.Repo as Repo exposing (Repo)
import Data.Repos as Repos exposing (Repos, mostBountifulRepo)
import Expect exposing (Expectation)
import Test exposing (..)


listOfRepos : Repos
listOfRepos =
    { repos = [ correctRepo, incorrectRepo, anotherIncorrectRepo ] }


correctRepo : Repo
correctRepo =
    Repo "a title" "an image src" 4


incorrectRepo : Repo
incorrectRepo =
    Repo "a title" "an image src" 1


anotherIncorrectRepo : Repo
anotherIncorrectRepo =
    Repo "a title" "an image src" 2


all : Test
all =
    describe "mostBountifulRepo"
        [ test "returns correct repo" <|
            \() ->
                listOfRepos
                    |> mostBountifulRepo
                    |> Expect.equal correctRepo
        ]
