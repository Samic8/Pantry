module Main exposing (Item, Model, Msg(..), init, main, toRow, update, updateItem, view)

import Browser
import Browser.Dom as Dom
import Browser.Events
import DOM exposing (Rectangle, boundingClientRect, offsetLeft, offsetParent, offsetWidth, parentElement, target)
import Debug
import Html exposing (Attribute, Html, button, div, h1, header, img, input, label, li, section, span, text, ul)
import Html.Attributes exposing (class, classList, id, placeholder, src, style, tabindex, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode as Json
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int


type alias EstimateOnHand =
    Int


type alias MaxOnHand =
    Int


type alias Items =
    List Item


type alias Item =
    { id : Id
    , name : String
    , estimateOnHand : EstimateOnHand
    , maxOnHand : MaxOnHand
    , unit : String
    , isNew : Maybe Bool
    , estimateTime : Maybe String
    }


type Prop
    = EstimateOnHand
    | MaxOnHand
    | Name
    | EstimateTime


type MouseMoveFocus
    = FilterBarMove
    | EstimateOnHandMove


type alias Model =
    { title : String
    , items : Items
    , hasChanges : Bool
    , barDragingItemId : Maybe Id
    , barDragingWidth : Maybe Float
    , barDragingLeft : Maybe Float
    , filterPercentage : Int
    , mouseMoveFocus : Maybe MouseMoveFocus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = "Sam's Kitchen Pantry"
      , items =
            [ { id = 1, name = "Chickpeas", estimateOnHand = 400, maxOnHand = 500, unit = "g", isNew = Nothing, estimateTime = Nothing }
            , { id = 2, name = "Red Lentils", estimateOnHand = 200, maxOnHand = 700, unit = "g", isNew = Nothing, estimateTime = Nothing }
            , { id = 3, name = "Cinnamon", estimateOnHand = 10, maxOnHand = 100, unit = "g", isNew = Nothing, estimateTime = Nothing }
            , { id = 4, name = "Chocolate", estimateOnHand = 40, maxOnHand = 150, unit = "g", isNew = Nothing, estimateTime = Nothing }
            , getNewItem 5
            ]
      , hasChanges = False
      , barDragingItemId = Nothing
      , barDragingWidth = Nothing
      , barDragingLeft = Nothing
      , filterPercentage = 100
      , mouseMoveFocus = Nothing
      }
    , Cmd.none
    )


getNewItem : Id -> Item
getNewItem id =
    { id = id, name = "", estimateOnHand = 0, maxOnHand = 500, unit = "g", isNew = Just True, estimateTime = Just "4 weeks" }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mouseMoveFocus of
        Just FilterBarMove ->
            subscribeToMouseMove

        Just EstimateOnHandMove ->
            subscribeToMouseMove

        Nothing ->
            Sub.none


subscribeToMouseMove : Sub Msg
subscribeToMouseMove =
    Sub.batch
        [ Browser.Events.onMouseMove mouseDecoder
        , Browser.Events.onMouseUp (Json.succeed OnBarMouseUp)
        ]


mouseDecoder : Json.Decoder Msg
mouseDecoder =
    Json.map toStringValue (Json.at [ "clientX" ] Json.float)


toStringValue : Float -> Msg
toStringValue mouseMove =
    BarDragingMouseMove mouseMove



-- UPDATE


type Msg
    = ModifyTitle String
    | ModifyEstimateOnHand Id String
    | ModifyMaxOnHand Id String
    | ModifyName Id String
    | ModifyEstimateTime Id String
    | SaveNewItem Id
    | OnBarMouseDown Id Float Rectangle
    | OnBarMouseUp
    | BarDragingMouseMove Float
    | OnFilterBarMouseDown Float Rectangle
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModifyTitle newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        ModifyName id newName ->
            ( updateModel model id newName Name, Cmd.none )

        ModifyEstimateOnHand id newEstimate ->
            ( updateModel model id newEstimate EstimateOnHand, Cmd.none )

        ModifyMaxOnHand id newMax ->
            ( updateModel model id newMax MaxOnHand, Cmd.none )

        ModifyEstimateTime id newTime ->
            ( updateModel model id newTime EstimateTime, Cmd.none )

        SaveNewItem id ->
            ( updateSaveNewModel model id, focusElement )

        OnBarMouseDown id barWidth barLeft ->
            ( { model | barDragingWidth = Just barWidth, barDragingLeft = Just barLeft.left, barDragingItemId = Just id, mouseMoveFocus = Just EstimateOnHandMove }, Cmd.none )

        OnBarMouseUp ->
            ( { model | barDragingWidth = Nothing, barDragingLeft = Nothing, barDragingItemId = Nothing, mouseMoveFocus = Nothing }, Cmd.none )

        BarDragingMouseMove mouseMove ->
            case model.mouseMoveFocus of
                Just EstimateOnHandMove ->
                    let
                        id =
                            model.barDragingItemId |> Maybe.withDefault 0
                    in
                    ( updateModel model id (buildNewEstimateFromMouseMove model id mouseMove) EstimateOnHand, Cmd.none )

                Just FilterBarMove ->
                    ( { model | filterPercentage = round (buildPercentageFromMouseMove model mouseMove * 100) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OnFilterBarMouseDown barWidth barLeft ->
            ( { model | barDragingWidth = Just barWidth, barDragingLeft = Just barLeft.left, mouseMoveFocus = Just FilterBarMove }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


buildNewEstimateFromMouseMove : Model -> Id -> Float -> String
buildNewEstimateFromMouseMove model id mouseMove =
    let
        percentageFloat =
            buildPercentageFromMouseMove model mouseMove

        itemMaxOnHand =
            List.sum (getItemFromId model.items id |> List.map (\item -> item.maxOnHand)) |> toFloat
    in
    round (itemMaxOnHand * percentageFloat) |> String.fromInt


buildPercentageFromMouseMove : Model -> Float -> Float
buildPercentageFromMouseMove model mouseMove =
    let
        pixelsFromRight =
            (Maybe.withDefault 0 model.barDragingLeft + Maybe.withDefault 0 model.barDragingWidth) - mouseMove

        percentageFloat =
            pixelsFromRight / (model.barDragingWidth |> Maybe.withDefault 0)
    in
    percentageFloat


focusElement : Cmd Msg
focusElement =
    Task.attempt (\_ -> NoOp) (Dom.focus "new-item-name-input")


updateModel : Model -> Id -> String -> Prop -> Model
updateModel model id newVal prop =
    { model | items = model.items |> List.map (\item -> updateItem item newVal id prop), hasChanges = isItemNew model.items id }


isItemNew : Items -> Id -> Bool
isItemNew items id =
    getItemFromId items id |> List.any (\item -> item.isNew /= Just True)


getItemFromId : Items -> Id -> Items
getItemFromId items id =
    items |> List.filter (\item -> item.id == id)


updateSaveNewModel : Model -> Id -> Model
updateSaveNewModel model id =
    { model | items = List.append (model.items |> List.map (\item -> updateSaveNewItem item id)) [ getNewItem (List.length model.items + 1) ] }


updateSaveNewItem : Item -> Id -> Item
updateSaveNewItem item id =
    if id == item.id then
        { item | isNew = Just False }

    else
        item


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

            EstimateTime ->
                { item | estimateTime = Just newVal }

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
            [ section [ class "filters" ]
                [ button [ class "filters__confirmButton", classList [ ( "filters__confirmButton--hide", model.hasChanges == False ) ] ] [ text "Confirm" ]
                , div [ class "bar" ]
                    [ div [ class "bar__quantityUsed" ] []
                    , div [ class "bar__quantityRemaining", style "width" ((model.filterPercentage |> String.fromInt) ++ "%") ]
                        [ div
                            [ class "bar__quantityRemaining__lever"
                            , on "mousedown" (onLeverMouseDown OnFilterBarMouseDown)
                            ]
                            []
                        ]
                    ]
                ]
            , ul [ class "listContainer" ] (model.items |> List.filter (\item -> calcEstimateRemainingPercentage item <= toFloat model.filterPercentage) |> List.map toRow)
            ]
        ]


toRow : Item -> Html Msg
toRow item =
    li [ class "row" ]
        [ input
            [ class "inputBox"
            , id
                (if item.isNew == Just True then
                    "new-item-name-input"

                 else
                    ""
                )
            , onInput (ModifyName item.id)
            , value item.name
            , placeholder (getPlaceholderText item)
            ]
            []
        , div [ class "quantity inputBox", classList [ ( "inputBox--covered", shouldCoverInputBox item ) ] ]
            [ input [ class "quantity__edit inputBox__innerEdit", classList [ ( "quantity__edit--excessive", isOverstocked item ) ], onInput (ModifyEstimateOnHand item.id), value (item.estimateOnHand |> String.fromInt) ] []
            , span [] [ text "/" ]
            , input [ class "quantity__edit inputBox__innerEdit", onInput (ModifyMaxOnHand item.id), value (item.maxOnHand |> String.fromInt) ] [ text (item.maxOnHand |> String.fromInt) ]
            , span [ class "quantity__unit" ] [ input [ class "quantity__unit__innerEdit inputBox__innerEdit", value item.unit ] [] ]
            ]
        , div [ class "bar", classList [ ( "bar--hidden", item.isNew == Just True ) ] ]
            [ div [ class "bar__quantityUsed", onClick (ModifyEstimateOnHand item.id (item.maxOnHand |> String.fromInt)) ] []
            , div [ class "bar__quantityExcessive", style "width" (buildQuantityExcessiveWidth item) ] []
            , div [ classList (quanitiyLeftClassList item), style "width" (buildQuantityRemainingWidth item) ]
                [ div
                    [ class "bar__quantityRemaining__lever"
                    , on "mousedown" (onLeverMouseDown (OnBarMouseDown item.id))
                    ]
                    []
                ]
            ]
        , div [ class "inputBox time", classList [ ( "time--hidden", item.isNew == Nothing || item.isNew == Just False ), ( "inputBox--covered", shouldCoverInputBox item ) ] ]
            [ div [ class "time__helpText" ] [ text "Estimate" ]
            , input [ class "time__input inputBox__innerEdit", value (Maybe.withDefault "" item.estimateTime), onInput (ModifyEstimateTime item.id) ] []
            ]
        , div
            [ classList (getConfirmTickClassList item)
            , onClick (SaveNewItem item.id)
            , onKeyDown
                (\key -> onConfirmKeyDown key item)
            , tabindex 0
            ]
            [ img [ src "./src/svg/tick.svg" ] []
            ]
        ]


onLeverMouseDown : (Float -> Rectangle -> Msg) -> Json.Decoder Msg
onLeverMouseDown msg =
    Json.map2 msg
        (target <| parentElement <| parentElement <| offsetWidth)
        (target <| parentElement <| parentElement <| boundingClientRect)


shouldCoverInputBox : Item -> Bool
shouldCoverInputBox item =
    item.isNew == Just True && item.name == ""


onConfirmKeyDown : Int -> Item -> Msg
onConfirmKeyDown key item =
    if key == 13 then
        SaveNewItem item.id

    else
        NoOp


getConfirmTickClassList : Item -> List ( String, Bool )
getConfirmTickClassList item =
    [ ( "row__confirmTick", True )
    , ( "row__confirmTick--hidden", shouldUseHiddenClass item )
    , ( "row__confirmTick--doesNotHaveName", item.name == "" )
    , ( "row__confirmTick--allwaysHidden", shouldUseAlwaysHiddenClass item )
    ]


shouldUseHiddenClass : Item -> Bool
shouldUseHiddenClass item =
    case item.isNew of
        Just isNew ->
            if isNew == True then
                False

            else
                True

        Nothing ->
            False


shouldUseAlwaysHiddenClass : Item -> Bool
shouldUseAlwaysHiddenClass item =
    case item.isNew of
        Just isNew ->
            False

        Nothing ->
            True


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


getPlaceholderText : Item -> String
getPlaceholderText item =
    case item.isNew of
        Just isNew ->
            if isNew then
                "Add new item....."

            else
                ""

        Nothing ->
            ""


quanitiyLeftClassList : Item -> List ( String, Bool )
quanitiyLeftClassList item =
    [ ( "bar__quantityRemaining", True )
    , ( "bar__quantityRemaining--low", calcEstimateRemainingPercentage item <= 20 )
    , ( "bar__quantityRemaining--excessive", isQuantityExcessive item )
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


buildQuantityRemainingWidth : Item -> String
buildQuantityRemainingWidth item =
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
