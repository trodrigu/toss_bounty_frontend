module Request.User exposing (storeSession)
import Data.AuthToken as AuthToken exposing (AuthToken, fallback)

import Json.Encode as Encode
import Ports

storeSession : { r | email : String, token : AuthToken, userId : String } -> Cmd msg
storeSession { email, token, userId } =
    let
        encodedUser =
            Encode.object
                [ ("email", Encode.string email)
                , ("authToken", AuthToken.encode token)
                , ("userId", Encode.string userId) ]

    in
        encodedUser
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
