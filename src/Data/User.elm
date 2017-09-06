module Data.User exposing (User, decoder, encode)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import UrlParser
import Util exposing ((=>))


type alias User =
    { email : String
    , name : String
    , token : AuthToken
    , createdAt : String
    , updatedAt : String
    }



-- SERIALIZATION --

decoder : Decoder User
decoder =
    decode User
      |> required "name" Decode.string
      |> required "email" Decode.string
      |> required "token" AuthToken.decoder
      |> required "createdAt" Decode.string
      |> required "updatedAt" Decode.string


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
