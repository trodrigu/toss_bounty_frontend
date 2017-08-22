-- module Request.User exposing (register, storeSession)

-- import Data.User as User exposing (User)
-- import Json.Decode as Decode
-- import Json.Encode as Encode
-- import Json.Encode.Extra as EncodeExtra
-- import Ports
-- import Util exposing ((=>))

-- storeSession : User -> Cmd msg
-- storeSession user =
--     User.encode user
--         |> Encode.encode 0
--         |> Just
--         |> Ports.storeSession

-- register : { r | username : String, email : String, password : String } -> Http.Request User
-- register { name, email, password } =
--     let
--         user =
--             Encode.object
--                 [ "name" => Encode.string name
--                 , "email" => Encode.string email
--                 , "password" => Encode.string password
--                 ]

--         body =
--             Encode.object [ "user" => user ]
--                 |> Http.jsonBody
--     in
--     Decode.field "user" User.decoder
--         |> Http.post ("http:localhost:4000/users") body
