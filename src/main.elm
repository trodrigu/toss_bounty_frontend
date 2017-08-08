module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import UrlParser exposing ((</>), (<?>), s, int, stringParam, top)

type alias Choice =
    { choice : String }

type alias Screen =
    { id : Int
    , question : String
    , choices : List Choice
    , conditional : Bool
    , nextQuestionId : Int
    , isEditing : Bool
    }

type alias Model =
    { screens : List Screen
    , questionFormVisible : Bool
    , field : String
    , conditional : Bool
    , id : Int
    , choices : List Choice
    , nextQuestionId : Int
    , tosserSignUpFormVisible : Bool
    , tosserSignUpForm : TosserSignUpForm
    }

type alias TosserSignUpForm =
    { name : String
    , email : String
    , company : String
    , password : String
    , passwordConfirmation : String }

type Msg
    = NoOp
    | ShowQuestionForm
    | SaveQuestion
    | Add
    | UpdateField String
    | ShowTosserSignUpForm
    | UpdateTosserFormNameField String
    | UpdateTosserFormEmailField String
    | UpdateTosserFormCompanyField String
    | UpdateTosserFormPasswordField String
    | UpdateTosserFormPasswordConfirmationField String
    | SaveTosserForm

emptyModel : Model
emptyModel =
    { screens = []
    , questionFormVisible = False
    , field = ""
    , id = 0
    , conditional = False
    , choices = []
    , nextQuestionId = 1
    , tosserSignUpFormVisible = False
    , tosserSignUpForm = emptyTosserSignUpForm
    }

emptyTosserSignUpForm : TosserSignUpForm
emptyTosserSignUpForm =
    { name = ""
    , email = ""
    , company = ""
    , password = ""
    , passwordConfirmation = "" }

newScreen : Int -> String -> Bool -> List Choice -> Int -> Bool -> Screen
newScreen id question conditional choices nextQuestionId isEditing =
    { id = id
    , question = question
    , conditional = conditional
    , choices = choices
    , isEditing = isEditing
    , nextQuestionId = nextQuestionId }

init : ( Model, Cmd Msg )
init = ( emptyModel, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Add ->
            let
                totalScreens =
                        model.screens ++ [ newScreen model.id model.field model.conditional model.choices model.nextQuestionId True ]
            in
                ( { model | screens = totalScreens, questionFormVisible = True }, Cmd.none )

        ShowQuestionForm -> showQuestionForm model

        SaveQuestion ->
            let
                updatedModel =
                    { model | questionFormVisible = False }

            in
                ( updatedModel, Cmd.none )

        UpdateField str ->
                ( { model | field = str }, Cmd.none )

        ShowTosserSignUpForm -> showTosserSignUpForm model

        UpdateTosserFormNameField str ->
            let
                currentTosserSignUpForm = model.tosserSignUpForm
                updatedTosserForm =
                    { currentTosserSignUpForm | name = str }

            in
                ( { model | tosserSignUpForm = updatedTosserForm }, Cmd.none )

        UpdateTosserFormEmailField str ->
            let
                currentTosserSignUpForm = model.tosserSignUpForm
                updatedTosserForm =
                    { currentTosserSignUpForm | email = str }

            in
                ( { model | tosserSignUpForm = updatedTosserForm }, Cmd.none )

        UpdateTosserFormCompanyField str ->
            let
                currentTosserSignUpForm = model.tosserSignUpForm
                updatedTosserForm =
                    { currentTosserSignUpForm | company = str }

            in
                ( { model | tosserSignUpForm = updatedTosserForm }, Cmd.none )


        UpdateTosserFormPasswordField str ->
            let
                currentTosserSignUpForm = model.tosserSignUpForm
                updatedTosserForm =
                    { currentTosserSignUpForm | password = str }

            in
                ( { model | tosserSignUpForm = updatedTosserForm }, Cmd.none )


        UpdateTosserFormPasswordConfirmationField str ->
            let
                currentTosserSignUpForm = model.tosserSignUpForm
                updatedTosserForm =
                    { currentTosserSignUpForm | passwordConfirmation = str }

            in
                ( { model | tosserSignUpForm = updatedTosserForm }, Cmd.none )

        SaveTosserForm ->
            let
                updatedModel =
                    { model | tosserSignUpFormVisible = False }

            in
                ( updatedModel, Cmd.none )

        UrlChange location ->

            let
                newRoute =
                    Maybe.withDefault HomeRoute ( UrlParser.parsePath matchers location )

            in
              ( { model | route = newRoute  }
              , Cmd.none
              )

showTosserSignUpForm : Model -> ( Model, Cmd Msg )
showTosserSignUpForm model =
    let
        updatedModel = { model |
                             tosserSignUpFormVisible = True }
    in
        ( updatedModel
        , Cmd.none
        )

showQuestionForm : Model -> ( Model, Cmd Msg )
showQuestionForm model =
    let
        updatedModel = { model |
                             questionFormVisible = True, id = model.id + 1 }
    in
        ( updatedModel
        , Cmd.none
        )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    span []
         [ renderNav
         , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "bulma.css" ] []
         , tosserHeroArea model
         , developerHeroArea
         , footerArea ]

renderTosserSignUpForm : Html Msg
renderTosserSignUpForm =
    section [ class "hero" ]
            [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                  [ div [ class "columns" ]
                        [ div [ class "column is-one-third is-offset-one-third"]
                              [ h1 [ class "title" ] [ text "Start Writing Stories" ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Name" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateTosserFormNameField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Email" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateTosserFormEmailField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Company" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateTosserFormCompanyField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Password" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateTosserFormPasswordField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field" ]
                                    [ label [ class "label" ]
                                            [ text "Password Confirmation" ]
                                    , p [ class "control" ]
                                        [ input [ class "input"
                                                , onInput UpdateTosserFormPasswordConfirmationField
                                                ]
                                                []
                                        ]
                                    ]
                              , div [ class "field is-grouped" ]
                                    [ p [ class "control" ]
                                          [ button [ class "button is-primary", onClick SaveTosserForm ]
                                                [ text "Save" ]
                                        ]
                                    ]
                              ]
                        ]
                  ]
            ]

tosserHeroArea : Model -> Html Msg
tosserHeroArea model =
    case model.tosserSignUpFormVisible of
        True ->
            renderTosserSignUpForm

        False ->
            section [ class "hero is-primary" ]
                    [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                          [ div [ class "container" ]
                                [ div [ class "columns is-vcentered" ]
                                      [ div [ class "column has-text-centered" ]
                                            [ p [ class "title" ]
                                                [ text "For tossers" ]
                                            , p [ class "subtitle" ]
                                                [ text "prime the"
                                                , strong [][ text " pump"]
                                                ]
                                            , button [ class "button" , onClick ShowTosserSignUpForm ]
                                                     [ text "Start a bounty" ]
                                            ]
                                    ]
                              ]
                        ]
                  ]

developerHeroArea : Html Msg
developerHeroArea =
        section [ class "hero" ]
                [ div [ class "hero-body", style[ ( "padding", "7rem 1.5rem" ) ] ]
                      [ div [ class "container" ]
                            [ div [ class "columns is-vcentered" ]
                                  [ div [ class "column has-text-centered" ]
                                        [ p [ class "title" ]
                                            [ text "For developers" ]
                                        , p [ class "subtitle" ]
                                            [ text "get paid with"
                                            , strong [][ text " no strings"]
                                            ]
                                        , button [ class "button" ]
                                            [ text "Start winning" ]
                                        ]
                                 ]
                           ]
                     ]
               ]

footerArea : Html Msg
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

newQuestionForm : Model -> Html Msg
newQuestionForm model =
    case model.questionFormVisible of
        True ->
            div [ class "section" ]
                [
                  div [ class "columns" ]
                      [
                      div [ class "column is-two-thirds"]
                          [ div [ class "field" ]
                                [ label [ class "label" ]
                                        [ text "Question" ]
                                , p [ class "control" ]
                                    [ input [ class "input"
                                            , style [ ( "width", "500px" ) ]
                                            , onInput UpdateField
                                            , onEnter Add
                                            ]
                                            []
                                    ]
                                ]
                          , div [ class "field is-grouped" ]
                                [ p [ class "control" ]
                                      [ button [ class "button is-primary", onClick Add ]
                                            [ text "Save" ]
                                    ]
                                , p [ class "control" ]
                                    [ button [ class "button is-link" ]
                                            [ text "Cancel"]
                                    ]
                                ]
                          ]
                      , div [] ( List.map renderChoices model.choices )
                      ]
                ]

        False -> div [][]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not Enter"

     in
         on "keydown" (Json.andThen isEnter keyCode)

renderNav : Html Msg
renderNav =
    div [ class "container" ]
        [ nav [ class "nav" ]
              [ div [ class "nav-left" ]
                    [ a [ class "nav-item" ]
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
                    [ a [ class "nav-item", onClick ShowTosserSignUpForm ]
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

screens : Model -> Html Msg
screens model =
    div []
        [ screen model.screens
        ]

screen : List Screen -> Html Msg
screen screens =
    div [ class "" ]
        ( List.map renderScreen screens )

renderScreen : Screen -> Html Msg
renderScreen screen =
    case screen.conditional of
        True ->
            div [ class "section" ]
                [ div [ class "columns" ]
                      [
                      div [ class "column has-text-centered"]
                          [
                            p [] [ text screen.question ]
                          ]
                      ]
                , div [ class "level" ] ( List.map renderChoicesAsLevelItems screen.choices ) ]

        False ->
            div [ class "section" ]
                [
                  div [ class "columns" ]
                      [
                      div [ class "column is-two-thirds has-text-centered"]
                          [
                            p [] [ text screen.question ]
                          ]
                      , div [] ( List.map renderChoices screen.choices )
                      ]
                ]

renderChoicesAsLevelItems : Choice -> Html Msg
renderChoicesAsLevelItems choice =
    div [ class "level-item" ]
        [
         a [ class "button is-primary" ]
           [ text(choice.choice |> addWhen) ]
        ]

addWhen : String -> String
addWhen choiceValue =
    "When " ++ choiceValue

renderChoices : Choice -> Html Msg
renderChoices model =
    div [ class "column" ]
        [ text model.choice ]

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions }
