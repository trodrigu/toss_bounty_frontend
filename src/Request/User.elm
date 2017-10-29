module Request.User exposing (storeSession)
import Data.AuthToken as AuthToken exposing (AuthToken, fallback)

import Json.Encode as Encode
import Ports

storeSession : { r | email : String, token : AuthToken } -> Cmd msg
storeSession { email, token } =
    let
        encodedUser =
            Encode.object
                [ ("email", Encode.string email)
                , ("authToken", AuthToken.encode token) ]

    in
        encodedUser
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
