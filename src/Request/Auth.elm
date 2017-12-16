module Request.Auth exposing (config)

import Data.AuthToken as AuthToken exposing (AuthToken, toString)
import Http exposing (Header)
import RemoteData.Http exposing (Config)


authHeader : AuthToken -> Http.Header
authHeader token =
    let
        tokenString =
            AuthToken.toString token

        completeTokenValue =
            "Bearer " ++ tokenString
    in
    Http.header "Authorization" completeTokenValue


config : AuthToken -> Config
config token =
    { headers = [ authHeader token ]
    , withCredentials = True
    , timeout = Nothing
    }
