module Main exposing (Item, Model, Msg(..), init, main, toRow, update, updateItem, view)

import Browser
import Browser.Dom as Dom
import Browser.Events
import DOM exposing (Rectangle, boundingClientRect, offsetLeft, offsetParent, offsetWidth, parentElement, target)
import Debug
import Html exposing (Attribute, Html, button, div, h1, header, img, input, label, li, section, span, text, ul)
import Html.Attributes exposing (class, classList, id, placeholder, src, style, tabindex, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onInput, onMouseDown, onMouseUp, onSubmit)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    String


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
    , userEstimateRunOut : Maybe String
    , initialEstimateOnHand : Maybe Int
    , beforeRestock : Int
    }


type Prop
    = EstimateOnHand
    | MaxOnHand
    | Name
    | EstimateTime
    | BeforeRestock


type MouseMoveFocus
    = FilterBarMove
    | EstimateOnHandMove


type alias Model =
    { title : String
    , items : Items
    , hasChanges : Bool
    , barDragingItemId : String
    , barDragingWidth : Maybe Float
    , barDragingLeft : Maybe Float
    , filterPercentage : Int
    , mouseMoveFocus : Maybe MouseMoveFocus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , items = []
      , hasChanges = False
      , barDragingItemId = ""
      , barDragingWidth = Nothing
      , barDragingLeft = Nothing
      , filterPercentage = 100
      , mouseMoveFocus = Nothing
      }
    , Http.get
        { url = "http://localhost:8000/cupboard"
        , expect = Http.expectJson Initialise cupboardDecoder
        }
    )


type alias ItemsResponse =
    List ItemResponse


type alias ItemResponse =
    { id : String, name : String, estimateOnHand : Int, maxOnHand : Int, unit : String }


type alias CupboardResult =
    { title : String, itemsResponse : ItemsResponse }


cupboardDecoder : Json.Decoder CupboardResult
cupboardDecoder =
    Json.map2 CupboardResult
        (Json.field "title" Json.string)
        (Json.field "items" (Json.list mapItems))


mapItems : Json.Decoder ItemResponse
mapItems =
    Json.map5 ItemResponse
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.field "estimateOnHand" Json.int)
        (Json.field "maxOnHand" Json.int)
        (Json.field "unit" Json.string)


getNewItem : Id -> Item
getNewItem id =
    { id = id, name = "", estimateOnHand = 0, maxOnHand = 500, unit = "g", isNew = Just True, userEstimateRunOut = Just "4 weeks", initialEstimateOnHand = Nothing, beforeRestock = 0 }



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
    = Initialise (Result Http.Error CupboardResult)
    | ModifyTitle String
    | SaveTitle
    | GotTitle (Result Http.Error String)
    | ModifyEstimateOnHand Id String
    | ModifyMaxOnHand Id String
    | ModifyName Id String
    | ModifyEstimateTime Id String
    | ModifyBeforeRestock Id String
    | SaveNewItem Item
    | GotNewItem (Result Http.Error ItemResponse)
    | OnBarMouseDown Id Float Rectangle
    | OnBarMouseUp
    | BarDragingMouseMove Float
    | OnFilterBarMouseDown Float Rectangle
    | ConfirmChanges
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise result ->
            case result of
                Ok cupboard ->
                    ( { model | title = cupboard.title, items = List.append (transformItemsReponse cupboard.itemsResponse) [ getNewItem "new-item" ] }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ModifyTitle newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        SaveTitle ->
            ( model
            , Http.post
                { url = "http://localhost:8000/cupboard"
                , body = Http.jsonBody (Encode.object [ ( "title", Encode.string model.title ) ])
                , expect = Http.expectJson GotTitle (Json.field "title" Json.string)
                }
            )

        GotTitle result ->
            case result of
                Ok newTitle ->
                    ( { model | title = newTitle }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ModifyName id newName ->
            ( updateModel model id newName Name, Cmd.none )

        ModifyEstimateOnHand id newEstimate ->
            ( updateModel model id newEstimate EstimateOnHand, Cmd.none )

        ModifyMaxOnHand id newMax ->
            ( updateModel model id newMax MaxOnHand, Cmd.none )

        ModifyEstimateTime id newTime ->
            ( updateModel model id newTime EstimateTime, Cmd.none )

        ModifyBeforeRestock id newBeforeRestock ->
            ( updateModel model id newBeforeRestock BeforeRestock, Cmd.none )

        SaveNewItem item ->
            ( updateSaveNewModel model item.id
            , Cmd.batch
                [ focusElement
                , Http.post
                    { url = "http://localhost:8000/cupboard/new-item"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "name", Encode.string item.name )
                                , ( "maxOnHand", Encode.int item.maxOnHand )
                                , ( "onHand", Encode.int item.estimateOnHand )
                                , ( "unit", Encode.string item.unit )
                                , ( "userEstimateRunOut", Encode.string (Maybe.withDefault "" item.userEstimateRunOut) )
                                ]
                            )
                    , expect = Http.expectJson GotNewItem mapItems
                    }
                ]
            )

        GotNewItem result ->
            case result of
                Ok newItem ->
                    ( { model | items = processNewItem model newItem }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        OnBarMouseDown id barWidth barLeft ->
            ( { model | barDragingWidth = Just barWidth, barDragingLeft = Just barLeft.left, barDragingItemId = id, mouseMoveFocus = Just EstimateOnHandMove }, Cmd.none )

        OnBarMouseUp ->
            ( { model | barDragingWidth = Nothing, barDragingLeft = Nothing, barDragingItemId = "", mouseMoveFocus = Nothing }, Cmd.none )

        BarDragingMouseMove mouseMove ->
            case model.mouseMoveFocus of
                Just EstimateOnHandMove ->
                    let
                        id =
                            model.barDragingItemId
                    in
                    ( updateModel model id (buildNewEstimateFromMouseMove model id mouseMove) EstimateOnHand, Cmd.none )

                Just FilterBarMove ->
                    ( { model | filterPercentage = round (buildPercentageFromMouseMove model mouseMove * 100) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OnFilterBarMouseDown barWidth barLeft ->
            ( { model | barDragingWidth = Just barWidth, barDragingLeft = Just barLeft.left, mouseMoveFocus = Just FilterBarMove }, Cmd.none )

        ConfirmChanges ->
            ( model
            , Http.post
                { url = "http://localhost:8000/cupboard/items"
                , body = Http.jsonBody (Encode.list Encode.object (buildEncodedItemList (model.items |> filterOutUnchanged)))
                , expect = Http.expectJson GotNewItem mapItems
                }
            )

        NoOp ->
            ( model, Cmd.none )


filterOutUnchanged : Items -> List Item
filterOutUnchanged items =
    List.filter (\item -> hasEstimateOnHandChanged item True) items


buildEncodedItemList : Items -> List (List ( String, Encode.Value ))
buildEncodedItemList items =
    List.filter (\item -> item.isNew /= Just True) items
        |> List.map
            (\item ->
                [ ( "id", Encode.string item.id )
                , ( "name", Encode.string item.name )
                , ( "maxOnHand", Encode.int item.maxOnHand )
                , ( "onHand", Encode.int item.estimateOnHand )
                , ( "unit", Encode.string item.unit )
                , ( "beforeRestock", Encode.int item.beforeRestock )
                ]
            )


processNewItem : Model -> ItemResponse -> List Item
processNewItem model newItem =
    List.append (model.items |> List.filter (\item -> item.id /= "new-item")) [ transformItemResponse newItem, getNewItem "new-item" ]


transformItemsReponse : ItemsResponse -> List Item
transformItemsReponse itemsResponse =
    List.map
        (\itemRes ->
            transformItemResponse itemRes
        )
        itemsResponse


transformItemResponse : ItemResponse -> Item
transformItemResponse itemResponse =
    Item itemResponse.id itemResponse.name itemResponse.estimateOnHand itemResponse.maxOnHand itemResponse.unit Nothing Nothing (Just itemResponse.estimateOnHand) 0


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


updateModel : Model -> String -> String -> Prop -> Model
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
    { model | items = List.append (model.items |> List.map (\item -> updateSaveNewItem item id)) [ getNewItem "new-item" ] }


updateSaveNewItem : Item -> Id -> Item
updateSaveNewItem item id =
    if id == item.id then
        { item | isNew = Just False }

    else
        item


updateItem : Item -> String -> String -> Prop -> Item
updateItem item newVal id prop =
    if item.id == id then
        case prop of
            EstimateOnHand ->
                { item | estimateOnHand = newVal |> parseValueToInt }

            MaxOnHand ->
                { item | maxOnHand = newVal |> parseValueToInt }

            Name ->
                { item | name = newVal }

            EstimateTime ->
                { item | userEstimateRunOut = Just newVal }

            BeforeRestock ->
                { item | beforeRestock = newVal |> parseValueToInt }

    else
        item


parseValueToInt : String -> Int
parseValueToInt stringVal =
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
            , input [ onInput ModifyTitle, onBlur SaveTitle, value model.title, class "header__title", placeholder "Enter Cupboard Title" ] []
            ]
        , section [ class "mainContent" ]
            [ section [ class "filters" ]
                [ button
                    [ class "filters__confirmButton"
                    , classList [ ( "filters__confirmButton--hide", model.hasChanges == False ) ]
                    , onClick ConfirmChanges
                    ]
                    [ text "Confirm" ]
                , div [ class "filterBar bar" ]
                    [ div [ class "bar__used filterBar__used" ] []
                    , div [ class "bar__mainPercentage filterBar__mainPercentage", style "width" ((model.filterPercentage |> String.fromInt) ++ "%") ]
                        [ div
                            [ class "bar__mainPercentage__lever"
                            , on "mousedown" (onLeverMouseDown OnFilterBarMouseDown)
                            ]
                            []
                        ]
                    ]
                ]
            , ul [ class "listContainer" ] (model.items |> filterUsingPercentage model |> List.map toRow)
            ]
        ]


filterUsingPercentage : Model -> Items -> List Item
filterUsingPercentage model items =
    items |> List.filter (\item -> shouldIncludeItemInView model item)


shouldIncludeItemInView : Model -> Item -> Bool
shouldIncludeItemInView model item =
    if calcEstimateRemainingPercentage item <= toFloat model.filterPercentage then
        True

    else if isOverstocked item then
        True

    else
        False


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
            [ div [ class "bar__used bar__quantityUsed", onClick (ModifyEstimateOnHand item.id (item.maxOnHand |> String.fromInt)) ] []
            , div [ class "bar__quantityExcessive", style "width" (buildQuantityExcessiveWidth item) ] []
            , div [ classList (quanitiyLeftClassList item), style "width" (buildQuantityRemainingWidth item) ]
                [ div
                    [ class "bar__mainPercentage__lever"
                    , on "mousedown" (onLeverMouseDown (OnBarMouseDown item.id))
                    ]
                    []
                ]
            ]
        , div [ class "inputBox time", classList [ ( "time--hidden", item.isNew == Nothing || item.isNew == Just False ), ( "inputBox--covered", shouldCoverInputBox item ) ] ]
            [ div [ class "time__helpText" ] [ text "Estimate" ]
            , input [ class "time__input inputBox__innerEdit", value (Maybe.withDefault "" item.userEstimateRunOut), onInput (ModifyEstimateTime item.id) ] []
            ]
        , div
            [ classList (getConfirmTickClassList item)
            , onClick (SaveNewItem item)
            , onKeyDown
                (\key -> onConfirmKeyDown key item)
            , tabindex 0
            ]
            [ img [ src "/src/svg/tick.svg" ] []
            ]
        , div [ class "row__beforeRestock inputBox quantity", classList [ ( "hidden", hasEstimateOnHandChanged item False == False ) ] ]
            [ input [ class "quantity__edit inputBox__innerEdit", value (item.beforeRestock |> String.fromInt), onInput (ModifyBeforeRestock item.id) ] []
            , span [ class "quantity__unit" ]
                [ input [ class "quantity__unit__innerEdit inputBox__innerEdit", value "g" ] []
                ]
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
        SaveNewItem item

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
    [ ( "bar__mainPercentage bar__quantityRemaining", True )
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


hasEstimateOnHandChanged : Item -> Bool -> Bool
hasEstimateOnHandChanged item default =
    case item.initialEstimateOnHand of
        Just initialEstimateOnHand ->
            initialEstimateOnHand /= item.estimateOnHand

        Nothing ->
            default
