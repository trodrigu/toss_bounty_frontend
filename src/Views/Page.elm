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
    | About
    | CreateCampaign
    | CreateRewards
    | TosserSignUp
    | Discover
    | Contribute
    | GithubOops
    | CreateUserRole


frame : Html msg -> Maybe User -> ActivePage -> Html msg -> Html msg -> Html msg
frame burgerMenuNavItems user page content burgerMenu =
    div [ class "page-frame" ]
        [ renderNav burgerMenuNavItems page user burgerMenu
        , content
        , footerArea page
        ]


renderNav : Html msg -> ActivePage -> Maybe User -> Html msg -> Html msg
renderNav burgerMenuNavItems page user burgerMenu =
    div [ class "container" ]
        [ nav [ class "navbar" ]
            (viewSignIn burgerMenuNavItems page user burgerMenu)
        ]


viewSignIn : Html msg -> ActivePage -> Maybe User -> Html msg -> List (Html msg)
viewSignIn burgerMenuNavItems page user burgerMenu =
    [ navbarBrand
        [ a [ class "navbar-item", Router.href HomeRoute ]
            [ text
                "Toss Bounty"
            ]
        , burgerMenu
        ]
    , burgerMenuNavItems
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
                    , p []
                        [ a [ class "link", Router.href AboutRoute ]
                            [ text "About" ]
                        ]
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
                    , p []
                        [ a [ class "link", Router.href AboutRoute ]
                            [ text "About" ]
                        ]
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


navbarBrand : List (Html msg) -> Html msg
navbarBrand navItems =
    div [ classList [ ( "navbar-brand", True ) ] ]
        navItems


navbarLeftLinks : List (Html msg) -> Html msg
navbarLeftLinks navItems =
    div [ classList [ ( "navbar-menu", True ) ] ]
        navItems
