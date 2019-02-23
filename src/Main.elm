import Browser
import Html exposing (Html, button, div, text, input, header, h1, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = String

init : Model
init =
  ""

-- UPDATE

type Msg = ModifyName String

update : Msg -> Model -> Model
update msg model =
  case msg of
    ModifyName newContent ->
      newContent


-- VIEW

view : Model -> Html Msg
view model =
  header [ class "header"]
    [
      h1 [ class "header__logo"] [
        span [] [ text "Pan"],
        span [ class "header__logo__line"] [],
        span [] [ text "Try"]
      ],
      input [ onInput ModifyName, value model, class "header__title" ] []
    ]