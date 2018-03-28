module Request.User exposing (storeSession)

import Data.AuthToken as AuthToken exposing (AuthToken, fallback)
import Json.Encode as Encode
import Ports


storeSession : { r | email : String, token : AuthToken, userId : String, role : Int } -> Cmd msg
storeSession { email, token, userId, role } =
    let
        encodedUser =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "authToken", AuthToken.encode token )
                , ( "userId", Encode.string userId )
                , ( "role", Encode.int role )
                ]
    in
    encodedUser
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
