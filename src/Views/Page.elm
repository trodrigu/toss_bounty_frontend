module Views.Page exposing (ActivePage(..), frame)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Routing.Router as Router exposing (Route(..))


type ActivePage
    = Other
    | Home
    | StripeConnectSignUp
    | Dash
    | Login
    | BetaSignUp
    | CreateCampaign
    | CreateRewards
    | TosserSignUp
    | Discover
    | Contribute
    | GithubOops
    | CreateUserRole


frame : Maybe User -> ActivePage -> Html msg -> Html msg
frame user page content =
    div [ class "page-frame" ]
        [ renderNav page user
        , content
        , footerArea page
        ]


renderNav : ActivePage -> Maybe User -> Html msg
renderNav page user =
    if page == Home then
        div [] []
    else
        div [ class "container" ]
            [ nav [ class "nav" ]
                (viewSignIn page user)
            ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [-- [ navbarLeftLinks [ text "Toss Bounty" ]
             -- , navbarLink (page == Home) LoginRoute [ text "Login" ]
            ]

        Just user ->
            [ navbarBrand
                [ a [ class "nav-item", Router.href HomeRoute ]
                    [ text
                        "Toss Bounty"
                    , button [ class "button navbar-burger" ]
                        [ span [] []
                        , span [] []
                        , span [] []
                        ]
                    ]
                ]
            , navbarLeftLinks
                [ a [ class "nav-item", Router.href DiscoverRoute ]
                    [ text
                        "Discover"
                    ]
                , a [ class "nav-item", Router.href DashRoute ]
                    [ text
                        "Dash"
                    ]
                , a [ class "nav-item", Router.href LogoutRoute ]
                    [ text
                        "Logout"
                    ]
                ]
            ]


footerArea : ActivePage -> Html msg
footerArea page =
    let
        innerFooter =
            case page of
                Dash ->
                    [ p []
                        [ strong []
                            [ span []
                                [ text "Made With "
                                , i [ class "fas fa-heart" ] []
                                , text " in San Diego"
                                ]
                            ]
                        ]
                    , a [ class "link", Router.href CreateUserRoleRoute ]
                        [ text "Start a campaign!" ]
                    ]

                Discover ->
                    [ p []
                        [ strong []
                            [ span []
                                [ text "Made With "
                                , i [ class "fas fa-heart" ] []
                                , text " in San Diego"
                                ]
                            ]
                        ]
                    , a [ class "link", Router.href CreateUserRoleRoute ]
                        [ text "Start a campaign!" ]
                    ]

                _ ->
                    [ p []
                        [ strong []
                            [ span []
                                [ text "Made With "
                                , i [ class "fas fa-heart" ] []
                                , text " in San Diego"
                                ]
                            ]
                        ]
                    ]
    in
    footer [ class "footer", style [ ( "padding", "8rem 1.5rem 6rem" ), ( "background-color", "whitesmoke" ) ] ]
        [ div [ class "container" ]
            [ div [ class "content has-text-centered" ]
                innerFooter
            ]
        ]


navbarRightLink : Bool -> Route -> List (Html msg) -> Html msg
navbarRightLink isActive route linkContent =
    div [ classList [ ( "nav-right", True ), ( "active", isActive ), ( "nav-menu", True ) ] ]
        [ a [ class "nav-item", Router.href route ]
            linkContent
        ]


navbarBrand : List (Html msg) -> Html msg
navbarBrand navItems =
    div [ classList [ ( "navbar-brand", True ) ] ]
        navItems


navbarLeftLinks navItems =
    div [ classList [ ( "navbar-menu", True ) ] ]
        navItems
