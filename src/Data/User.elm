module Data.User exposing (User, decoder, default, encode, encodeLogin, encodeStripeUserUpdate, encodeUserRoleUpdate, returnToSessionDecoder)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, dict, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, optionalAt, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Url.Parser


type alias User =
    { email : String
    , token : AuthToken
    , userId : String
    , stripeExternalId : String
    , stripeAccessToken : String
    , role : Int
    }



-- SERIALIZATION --


returnToSessionDecoder : Decoder User
returnToSessionDecoder =
    succeed User
        |> required "email" Decode.string
        |> optional "authToken" AuthToken.decoder AuthToken.fallback
        |> optional "userId" Decode.string ""
        |> optional "stripeExternalId" Decode.string ""
        |> optional "stripeAccessToken" Decode.string ""
        |> optional "role" Decode.int 0


decoder : Decoder User
decoder =
    succeed User
        |> requiredAt [ "data", "attributes", "email" ] Decode.string
        |> optionalAt [ "data", "attributes", "token" ] AuthToken.decoder AuthToken.fallback
        |> optionalAt [ "data", "attributes", "user_id" ] Decode.string ""
        |> optionalAt [ "data", "attributes", "stripe-external-id" ] Decode.string ""
        |> optionalAt [ "data", "attributes", "stripe-access-token" ] Decode.string ""
        |> optionalAt [ "data", "attributes", "role" ] Decode.int 0


encode : { r | email : String, password : String, stripeExternalId : String, stripeAccessToken : String } -> Encode.Value
encode user =
    let
        user_attributes =
            Encode.object
                [ ( "email", Encode.string user.email )
                , ( "password", Encode.string user.password )
                , ( "stripe_external_id", Encode.string user.stripeExternalId )
                , ( "stripe_access_token", Encode.string user.stripeAccessToken )
                ]

        data_attributes =
            Encode.object [ ( "attributes", user_attributes ) ]
    in
    Encode.object [ ( "data", data_attributes ) ]


encodeStripeUserUpdate : { r | stripeExternalId : String, stripeAccessToken : String } -> Encode.Value
encodeStripeUserUpdate user =
    let
        user_attributes =
            Encode.object
                [ ( "stripe_external_id", Encode.string user.stripeExternalId )
                , ( "stripe_access_token", Encode.string user.stripeAccessToken )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", user_attributes )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


encodeUserRoleUpdate : { r | role : Int } -> Encode.Value
encodeUserRoleUpdate user =
    let
        user_attributes =
            Encode.object
                [ ( "role", Encode.int user.role )
                ]

        data_attributes =
            Encode.object
                [ ( "attributes", user_attributes )
                ]
    in
    Encode.object [ ( "data", data_attributes ) ]


encodeLogin : { r | email : String, password : String } -> Encode.Value
encodeLogin user =
    let
        user_attributes =
            Encode.object
                [ ( "email", Encode.string user.email )
                , ( "password", Encode.string user.password )
                ]
    in
    user_attributes


default : User
default =
    User "" (AuthToken.init "") "" "" "" 0
