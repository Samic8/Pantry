module Main exposing (Item, Model, Msg(..), init, main, toRow, update, updateItem, view)

import Browser
import Html exposing (Html, button, div, h1, header, input, label, li, section, span, text, ul)
import Html.Attributes exposing (class, tabindex, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Id =
    Int


type alias EstimateOnHand =
    Int


type alias MaxOnHand =
    Int


type alias Item =
    { id : Id, name : String, estimateOnHand : EstimateOnHand, maxOnHand : MaxOnHand, unit : String }


type Prop
    = EstimateOnHand
    | MaxOnHand
    | Name


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
    = ModifyTitle String
    | ModifyEstimateOnHand Id String
    | ModifyMaxOnHand Id String
    | ModifyName Id String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ModifyTitle newTitle ->
            { model | title = newTitle }

        ModifyName id newName ->
            updateModel model id newName Name

        ModifyEstimateOnHand id newEstimate ->
            updateModel model id newEstimate EstimateOnHand

        ModifyMaxOnHand id newMax ->
            updateModel model id newMax MaxOnHand


updateModel : Model -> Id -> String -> Prop -> Model
updateModel model id newVal prop =
    { model | items = model.items |> List.map (\item -> updateItem item newVal id prop) }


updateItem : Item -> String -> Int -> Prop -> Item
updateItem item newVal id prop =
    if item.id == id then
        case prop of
            EstimateOnHand ->
                { item | estimateOnHand = newVal |> parseOnHand }

            MaxOnHand ->
                { item | maxOnHand = newVal |> parseOnHand }

            Name ->
                { item | name = newVal }

    else
        item


parseOnHand : String -> Int
parseOnHand stringVal =
    Maybe.withDefault 0 (stringVal |> String.toInt)



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
            , input [ onInput ModifyTitle, value model.title, class "header__title" ] []
            ]
        , section [ class "mainContent" ]
            [ ul [ class "listContainer" ] (model.items |> List.map toRow)
            ]
        ]


toRow : Item -> Html Msg
toRow item =
    li [ class "row" ]
        [ inputDropdownCombo item
        , input [ onInput (ModifyEstimateOnHand item.id), value (item.estimateOnHand |> String.fromInt) ] []
        , input [ onInput (ModifyMaxOnHand item.id), value (item.maxOnHand |> String.fromInt) ] [ text (item.maxOnHand |> String.fromInt) ]
        , span [] [ text item.unit ]
        , span [] [ text ((toFloat item.estimateOnHand / toFloat item.maxOnHand) |> String.fromFloat) ]
        ]


inputDropdownCombo : Item -> Html Msg
inputDropdownCombo item =
    div [ class "inputDropdownCombo" ]
        [ input [ class "inputDropdownCombo__input", onInput (ModifyName item.id), value item.name ] []
        , label [ class "inputDropdownCombo__arrow", tabindex 0 ] []
        ]
