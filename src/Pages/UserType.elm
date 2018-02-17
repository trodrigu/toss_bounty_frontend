module Pages.UserType exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Html exposing (..)
import Html.Attributes exposing (action, class, id, method, src, style)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (..)
import Util exposing ((=>))

init : AuthToken -> Maybe String -> Model
init token apiUrl =
        url =
            case apiUrl of
                Nothing ->
                    ""

                Just url ->
                    url
    in
    { token = token
    , apiUrl = url
    , maintainer = False
    , contributor = False
    , both = False}
    }

type alias Model =
  { notifications : Bool
  , autoplay : Bool
  , location : Bool
  }


-- MODEL


type alias Model =
  { token : AuthToken
  , apiUrl : String
  , maintainer : Bool
  , contributor : Bool
  , both : Bool
  }




-- UPDATE


type Msg
  = ToggleMaintainer
  | ToggleContributor
  | ToggleBoth


update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleMaintainer ->
      { model | notifications = not model.notifications }

    ToggleContributor ->
      { model | autoplay = not model.autoplay }

    ToggleBoth ->
      { model | location = not model.location }



-- VIEW


view : Model -> Html Msg
view model =
  fieldset []
    [ checkbox ToggleMaintainer "Maintainer"
    , checkbox ToggleContributor "Autoplay"
    , checkbox ToggleBoth "Both"
    ]


checkbox : msg -> String -> Html msg
checkbox msg name =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", onClick msg ] []
    , text name
    ]
