module Views.Page exposing (ActivePage(..), frame)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Routing.Router as Router exposing (Route(..))
import Util exposing ((=>))


type ActivePage
    = Other
    | Home
    | StripeConnectSignUp
    | Dash
    | Login
    | BetaSignUp
    | CreateCampaign
    | TosserSignUp


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
            (viewSignIn page user)
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [-- [ navbarHomeLink [ text "Toss Bounty" ]
             -- , navbarRightLink (page == Home) LoginRoute [ text "Login" ]
            ]

        Just user ->
            [ navbarHomeLink [ text "Toss Bounty" ]

            -- , navbarRightLink (page == Dash) DashRoute [ text "Dash" ]
            ]


footerArea : Html msg
footerArea =
    footer [ class "footer", style [ ( "padding", "8rem 1.5rem 6rem" ), ( "background-color", "whitesmoke" ) ] ]
        [ div [ class "container" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ strong []
                        [ span []
                            [ text "Made With "
                            , i [ class "fa fa-heart" ] []
                            , text " in San Diego"
                            ]
                        ]
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
