module Data.User exposing (User, decoder, encode, encodeLogin, encodeStripeUserUpdate, returnToSessionDecoder)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, dict)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, optionalAt, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import UrlParser
import Util exposing ((=>))


type alias User =
    { email : String
    , token : AuthToken
    , userId : String
    , stripeExternalId : String
    , stripeAccessToken : String
    }



-- SERIALIZATION --


returnToSessionDecoder : Decoder User
returnToSessionDecoder =
    decode User
        |> required "email" Decode.string
        |> optional "authToken" AuthToken.decoder AuthToken.fallback
        |> optional "userId" Decode.string ""
        |> optional "stripeExternalId" Decode.string ""
        |> optional "stripeAccessToken" Decode.string ""


decoder : Decoder User
decoder =
    decode User
        |> requiredAt [ "data", "attributes", "email" ] Decode.string
        |> optionalAt [ "data", "attributes", "token" ] AuthToken.decoder AuthToken.fallback
        |> optionalAt [ "data", "attributes", "user_id" ] Decode.string ""
        |> optionalAt [ "data", "attributes", "stripe-external-id" ] Decode.string ""
        |> optionalAt [ "data", "attributes", "stripe-access-token" ] Decode.string ""


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
