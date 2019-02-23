module Main exposing (Item, Model, Msg(..), init, main, toRow, update, updateItem, view)

import Browser
import Html exposing (Html, button, div, h1, header, input, li, section, span, text, ul)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Item =
    { id : Int, name : String, estimateOnHand : Int, maxOnHand : Int, unit : String }


type alias Model =
    { title : String
    , items : List Item
    }


init : Model
init =
    { title = "Sam's Kitchen Pantry"
    , items =
        [ { id = 1, name = "Chickpeas", estimateOnHand = 400, maxOnHand = 500, unit = "grams" }
        , { id = 2, name = "Red Lentils", estimateOnHand = 200, maxOnHand = 700, unit = "grams" }
        , { id = 3, name = "Cinnamon", estimateOnHand = 10, maxOnHand = 100, unit = "grams" }
        , { id = 4, name = "Chocolate", estimateOnHand = 40, maxOnHand = 150, unit = "grams" }
        ]
    }



-- UPDATE


type Msg
    = ModifyName String
    | ModifyEstimateOnHand Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ModifyName newTitle ->
            { model | title = newTitle }

        ModifyEstimateOnHand id newEstimate ->
            { model | items = model.items |> List.map (\item -> updateItem id newEstimate item) }


updateItem : Int -> String -> Item -> Item
updateItem id newEstimate item =
    if item.id == id then
        { item | estimateOnHand = Maybe.withDefault 0 (newEstimate |> String.toInt) }

    else
        item



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header [ class "header" ]
            [ h1 [ class "header__logo" ]
                [ span [] [ text "Pan" ]
                , span [ class "header__logo__line" ] []
                , span [] [ text "try" ]
                ]
            , input [ onInput ModifyName, value model.title, class "header__title" ] []
            ]
        , section [ class "mainContent" ]
            [ ul [ class "listContainer" ] (model.items |> List.map toRow)
            ]
        ]


toRow : Item -> Html Msg
toRow item =
    li []
        [ input [ value item.name ] []
        , input [ onInput (ModifyEstimateOnHand item.id), value (item.estimateOnHand |> String.fromInt) ] []
        , input [] [ text (item.maxOnHand |> String.fromInt) ]
        , span [] [ text item.unit ]
        ]
