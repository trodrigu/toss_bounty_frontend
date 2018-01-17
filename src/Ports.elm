port module Ports exposing (consumeToken, createStripeElement, onSessionChange, storeSession)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


port consumeToken : (Value -> msg) -> Sub msg


port createStripeElement : String -> Cmd msg
