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
              -- [ div [ class "nav-left" ]
              --       [ a [ class "nav-item", Router.href HomeRoute ]
              --           [ text "Toss Bounty"
              --           ]
              --       ]
              -- , div [ class "nav-center" ]
              --       [ a [ class "nav-item" ]
              --           [ span [ class "icon" ]
              --                 [ i [ class "fa fa-twitter" ] [] ]
              --           ]
              --       ]
              -- , span [ class "nav-toggle" ]
              --       [ span [] []
              --       , span [] []
              --       , span [] []
              --       ]
              -- ] <| (viewSignIn page user)
              ( viewSignIn page user )
        ]

viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [ navbarHomeLink [ text "Toss Bounty" ]
            , navbarRightLink (page == Home) TosserSignUpRoute [ text "Sign up as a tosser" ]
            , navbarRightLink (page == Home) TosserSignUpRoute [ text "Sign up as a bounty hunter" ]
            ]

        Just user ->
            [ navbarHomeLink [ text "Toss Bounty" ]
            , navbarRightLink (page == Dash) DashRoute [ text "Dash" ]
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



navbarRightLink : Bool -> Route -> List (Html msg) -> Html msg
navbarRightLink isActive route linkContent =
    div [ classList [ ( "nav-right", True ), ( "active", isActive ), ( "nav-menu", True ) ] ]
          [ a [ class "nav-item", Router.href route ]
              linkContent
          ]

navbarHomeLink : List (Html msg) -> Html msg
navbarHomeLink linkContent =
    div [ classList [ ( "nav-left", True ) ] ]
          [ a [ class "nav-item", Router.href HomeRoute ]
              linkContent
          ]
