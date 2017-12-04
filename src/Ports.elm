port module Ports exposing (onSessionChange, storeSession, apiUrl)

import Json.Encode exposing (Value)

port storeSession : Maybe String -> Cmd msg

port onSessionChange : (Value -> msg) -> Sub msg

port apiUrl : ( Value -> msg ) -> Sub msg
