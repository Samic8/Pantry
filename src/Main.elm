import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value)


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
  div []
    [ input [ onInput ModifyName, value model ] [], div [] [ text model ] ]