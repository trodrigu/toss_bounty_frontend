module Views.Page exposing (ActivePage(..), frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Data.User as User exposing (User, Username)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Routing.Router as Router
import Routing.Helpers exposing (Route(..), parseLocation, reverseRoute, modifyUrl, href)
import Util exposing ((=>))


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

-}
frame : Maybe User -> ActivePage -> Html msg -> Html msg
frame user page content =
    div [ class "page-frame" ]
        [ viewHeader page user
        , content
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Html msg
viewHeader page user =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Routing.Helpers.href Routing.Helpers.HomeRoute ]
                [ text "conduit" ]
            ]
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
            [ navbarLink (page == Home) Routing.Helpers.HomeRoute [ text "Sign in" ]
            ]

viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", Routing.Helpers.href Routing.Helpers.HomeRoute ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Routing.Helpers.href route ] linkContent ]
