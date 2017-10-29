module Request.User exposing (storeSession)

import Json.Encode as Encode
import Ports

storeSession : { r | email : String } -> Cmd msg
storeSession { email } =
    let
        encodedUser =
            Encode.object
                [ ("email", Encode.string email) ]

    in
        encodedUser
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
