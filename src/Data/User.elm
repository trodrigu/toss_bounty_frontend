module Data.User exposing (User, decoder, encode)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, dict)
import Json.Decode.Pipeline as Pipeline exposing (decode, requiredAt, optionalAt)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import UrlParser
import Util exposing ((=>))


type alias User =
    { name : String
    , email : String
    , token : AuthToken
    }

-- SERIALIZATION --

decoder : Decoder User
decoder =
    decode User
      |> requiredAt [ "data", "attributes", "name" ] Decode.string
      |> requiredAt [ "data", "attributes", "email" ] Decode.string
      |> optionalAt [ "data", "attributes", "token" ] AuthToken.decoder AuthToken.fallback


encode : { r | name : String, email : String, password : String } -> Encode.Value
encode user =

    let

        user_attributes =
            Encode.object
                [ ("name", Encode.string user.name)
                , ("email", Encode.string user.email)
                , ("password", Encode.string user.password) ]

        data_attributes =
            Encode.object [ ( "attributes", user_attributes)]

    in
        Encode.object [ ("data", data_attributes) ]
