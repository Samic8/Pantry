module Main exposing (Item, Model, Msg(..), init, main, toRow, update, updateItem, view)

import Browser
import Html exposing (Html, button, div, h1, header, input, label, li, section, span, text, ul)
import Html.Attributes exposing (class, classList, style, tabindex, value)
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
        [ { id = 1, name = "Chickpeas", estimateOnHand = 400, maxOnHand = 500, unit = "g" }
        , { id = 2, name = "Red Lentils", estimateOnHand = 200, maxOnHand = 700, unit = "g" }
        , { id = 3, name = "Cinnamon", estimateOnHand = 10, maxOnHand = 100, unit = "g" }
        , { id = 4, name = "Chocolate", estimateOnHand = 40, maxOnHand = 150, unit = "g" }
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
        [ input [ class "inputBox", onInput (ModifyName item.id), value item.name ] []
        , div [ class "quantity inputBox" ]
            [ input [ class "quantity__edit", classList [ ( "quantity__edit--excessive", isOverstocked item ) ], onInput (ModifyEstimateOnHand item.id), value (item.estimateOnHand |> String.fromInt) ] []
            , span [] [ text "/" ]
            , input [ class "quantity__edit", onInput (ModifyMaxOnHand item.id), value (item.maxOnHand |> String.fromInt) ] [ text (item.maxOnHand |> String.fromInt) ]
            , span [ class "quantity__unit" ] [ text item.unit ]
            ]
        , div [ class "bar" ]
            [ div [ class "bar__quantityUsed", onClick (ModifyEstimateOnHand item.id (item.maxOnHand |> String.fromInt)) ] []
            , div [ class "bar__quantityExcessive", style "width" (buildQuantityExcessiveWidth item) ] []
            , div [ classList (quanitiyLeftClassList item), style "width" (buildQuantityLeftWidth item) ] []
            ]
        ]


quanitiyLeftClassList : Item -> List ( String, Bool )
quanitiyLeftClassList item =
    [ ( "bar__quantityLeft", True )
    , ( "bar__quantityLeft--low", calcEstimateRemainingPercentage item <= 20 )
    , ( "bar__quantityLeft--excessive", isQuantityExcessive item )
    ]


buildQuantityExcessiveWidth : Item -> String
buildQuantityExcessiveWidth item =
    if isQuantityExcessive item then
        ((100 - (toFloat item.maxOnHand / toFloat item.estimateOnHand) * 100) |> String.fromFloat) ++ "%"

    else
        "0%"


isQuantityExcessive : Item -> Bool
isQuantityExcessive item =
    calcEstimateRemainingPercentage item > 100


buildQuantityLeftWidth : Item -> String
buildQuantityLeftWidth item =
    if isQuantityExcessive item then
        (((toFloat item.maxOnHand / toFloat item.estimateOnHand) * 100) |> String.fromFloat) ++ "%"

    else
        (min (calcEstimateRemainingPercentage item) 100 |> String.fromFloat) ++ "%"


calcEstimateRemainingPercentage : Item -> Float
calcEstimateRemainingPercentage item =
    toFloat item.estimateOnHand / toFloat item.maxOnHand * 100


isOverstocked : Item -> Bool
isOverstocked item =
    calcEstimateRemainingPercentage item > 100
