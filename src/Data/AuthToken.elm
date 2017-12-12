module Data.AuthToken exposing (AuthToken, decoder, encode, fallback, init, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type AuthToken
    = AuthToken String


encode : AuthToken -> Value
encode (AuthToken token) =
    Encode.string token


decoder : Decoder AuthToken
decoder =
    Decode.string
        |> Decode.map AuthToken

fallback : AuthToken
fallback =
    AuthToken ""

init : String -> AuthToken
init token =
    AuthToken token

toString : AuthToken -> String
toString authToken =
    let
        tokenString =
            case authToken of
                AuthToken token ->
                    token

    in
        tokenString
