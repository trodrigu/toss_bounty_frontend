module Views.Page exposing (ActivePage(..), frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Routing.Router as Router exposing (Route(..))
import Util exposing ((=>))


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home
    | Dash
    | TosserSignUp


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

-}
frame : Maybe User -> ActivePage -> Html msg -> Html msg
frame user page content =
    div [ class "page-frame" ]
        [ renderNav page user
        , content
        , footerArea
        ]

renderNav : ActivePage -> Maybe User -> Html msg
renderNav page user =
    div [ class "container" ]
        [ nav [ class "nav" ]
              [ div [ class "nav-left" ]
                    [ a [ class "nav-item", Router.href HomeRoute ]
                        [ text "Toss Bounty"
                        ]
                    ]
              , div [ class "nav-center" ]
                    [ a [ class "nav-item" ]
                        [ span [ class "icon" ]
                              [ i [ class "fa fa-twitter" ] [] ]
                        ]
                    ]
              , span [ class "nav-toggle" ]
                    [ span [] []
                    , span [] []
                    , span [] []
                    ]
              , div [ class "nav-right nav-menu" ]
                    [ a [ class "nav-item", Router.href DashRoute ]
                        [ text "Dash"
                        ]
                    ]
              , div [ class "nav-right nav-menu" ]
                    [ a [ class "nav-item", Router.href TosserSignUpRoute ]
                        [ text "Sign up as a tosser"
                        ]
                    ]
              , div [ class "nav-right nav-menu" ]
                    [ a [ class "nav-item" ]
                        [ text "Sign up as a bounty hunter"
                        ]
                    ]
              ]
        ]

viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
            [ navbarLink (page == Home) HomeRoute [ text "Sign in" ]
            ]

footerArea : Html msg
footerArea =
    footer [ style [("padding", "3rem 1.5rem 6rem"), ("background-color", "whitesmoke")]]
           [ div [ class "container" ]
                 [ div [ class "content has-text-centered" ]
                       [ p []
                           [ strong []
                                    [ text "Toss Bounty" ]
                           ]
                       ]
                 ]
           ]



navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Router.href route ] linkContent ]
