module Main exposing (CupboardResult, EstimateDays, EstimateOnHand, Id, Item, ItemResponse, Items, ItemsResponse, MaxOnHand, Model, MouseMoveFocus(..), Msg(..), NewItem, Prop(..), Toggle(..), buildEncodedItemList, buildListHeader, buildNewEstimateFromMouseMove, buildNewItemHeader, buildNewItemsFromResponse, buildPercentageFromMouseMove, buildQuantityExcessiveWidth, buildQuantityRemainingWidth, buildRows, calcEstimateRemainingPercentage, cupboardDecoder, filterOutUnchanged, filterUsingPercentage, focusElement, getConfirmTickClassList, getItemFromId, hasEstimateOnHandChanged, init, isOverstocked, isQuantityExcessive, main, mapItem, maybeSaveChangedItems, mouseDecoder, newItemRowHeaderMarginTop, onConfirmKeyDown, onKeyDown, onLeverMouseDown, parseValueToInt, processNewItem, quantityLeftClassList, shouldCoverInputBox, shouldHideNewRowHeader, shouldIncludeItemInView, subscribeToMouseMove, subscriptions, toStringValue, toggleOnOff, transformItemResponse, transformItemsResponse, update, updateItem, updateItems, view, viewItemRow, viewNewRow)

import Browser
import Browser.Dom as Dom
import Browser.Events
import DOM exposing (Rectangle, boundingClientRect, offsetLeft, offsetParent, offsetWidth, parentElement, target)
import Html exposing (Attribute, Html, button, div, h1, header, img, input, label, li, section, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, placeholder, src, style, tabindex, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onInput, onMouseDown, onMouseUp, onSubmit)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    String


type alias EstimateDays =
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
    , estimateDays : EstimateDays
    , estimateOnHand : EstimateOnHand
    , maxOnHand : MaxOnHand
    , unit : String
    , userEstimateRunOut : Maybe String
    , initialEstimateOnHand : Maybe Int
    }


type alias NewItem =
    { name : String
    , onHand : Int
    , maxOnHand : Int
    , unit : String
    , userEstimateRunOut : String
    , confirmed : Bool
    }


type Prop
    = EstimateOnHand
    | MaxOnHand
    | Name
    | EstimateTime
    | Unit


type MouseMoveFocus
    = FilterBarMove
    | EstimateOnHandMove


type Toggle
    = On
    | Off


type alias Model =
    { title : String
    , items : Items
    , newItem : NewItem
    , newItemWarning : String
    , barDragingItemId : String
    , barDragingWidth : Maybe Float
    , barDragingLeft : Maybe Float
    , filterPercentage : Int
    , mouseMoveFocus : Maybe MouseMoveFocus
    , restockMode : Toggle
    , settings : Toggle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , items = []
      , newItem = NewItem "" 500 500 "g" "4 weeks" False
      , newItemWarning = ""
      , barDragingItemId = ""
      , barDragingWidth = Nothing
      , barDragingLeft = Nothing
      , filterPercentage = 100
      , mouseMoveFocus = Nothing
      , restockMode = Off
      , settings = Off
      }
    , Http.get
        { url = "http://localhost:8000/cupboard"
        , expect = Http.expectJson Initialise cupboardDecoder
        }
    )


type alias ItemsResponse =
    List ItemResponse


type alias ItemResponse =
    { id : String, name : String, estimateDays : EstimateDays, estimateOnHand : EstimateOnHand, maxOnHand : MaxOnHand, unit : String }


type alias CupboardResult =
    { title : String, itemsResponse : ItemsResponse }


cupboardDecoder : Json.Decoder CupboardResult
cupboardDecoder =
    Json.map2 CupboardResult
        (Json.field "title" Json.string)
        (Json.field "items" (Json.list mapItem))


mapItem : Json.Decoder ItemResponse
mapItem =
    Json.map6 ItemResponse
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.field "estimateDays" Json.int)
        (Json.field "estimateOnHand" Json.int)
        (Json.field "maxOnHand" Json.int)
        (Json.field "unit" Json.string)



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.mouseMoveFocus of
            Just FilterBarMove ->
                subscribeToMouseMove

            Just EstimateOnHandMove ->
                subscribeToMouseMove

            Nothing ->
                Sub.none
        , Time.every 5000 (always (maybeSaveChangedItems model))
        ]


maybeSaveChangedItems : Model -> Msg
maybeSaveChangedItems model =
    if (List.length (model.items |> filterOutUnchanged) > 0) && model.mouseMoveFocus == Nothing then
        SaveChangedItems

    else
        NoOp


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
    | ModifyEstimateOnHandBar Id String
    | ModifyEstimateOnHand Id String
    | ModifyMaxOnHand Id String
    | ModifyName Id String
    | ModifyUnit Id String
    | ModifyNewName String
    | ModifyNewOnHand String
    | ModifyNewEstimateTime String
    | ValidateNewEstimateTime
    | ModifyNewMaxOnHand String
    | ModifyNewUnit String
    | SaveNewItem
    | SaveNewItemServer
    | GotNewItem (Result Http.Error ItemResponse)
    | OnBarMouseDown Id Float Rectangle
    | OnBarMouseUp
    | BarDragingMouseMove Float
    | OnFilterBarMouseDown Float Rectangle
    | ToggleRestockMode
    | ToggleSettings
    | SaveChangedItems
    | GotNewItems (Result Http.Error ItemsResponse)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise result ->
            case result of
                Ok cupboard ->
                    ( { model | title = cupboard.title, items = buildNewItemsFromResponse cupboard.itemsResponse }, Cmd.none )

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
            let
                updatedItems =
                    updateItems model id newName Name
            in
            ( { model | items = updatedItems }, Cmd.none )

        ModifyEstimateOnHandBar id newEstimate ->
            case model.restockMode of
                On ->
                    ( { model | items = updateItems model id newEstimate EstimateOnHand }, Cmd.none )

                Off ->
                    ( model, Cmd.none )

        ModifyEstimateOnHand id newEstimate ->
            ( { model | items = updateItems model id newEstimate EstimateOnHand }, Cmd.none )

        ModifyMaxOnHand id newMax ->
            ( { model | items = updateItems model id newMax MaxOnHand }, Cmd.none )

        ModifyUnit id newUnit ->
            ( { model | items = updateItems model id newUnit Unit }, Cmd.none )

        ModifyNewName name ->
            let
                newItem =
                    model.newItem

                updatedNewItem =
                    { newItem | name = name }

                settings =
                    if name /= "" then
                        On

                    else
                        model.settings
            in
            ( { model | newItem = updatedNewItem, settings = settings }, Cmd.none )

        ModifyNewOnHand onHand ->
            let
                newItem =
                    model.newItem

                updatedNewItem =
                    { newItem | onHand = onHand |> parseValueToInt }
            in
            ( { model | newItem = updatedNewItem }, Cmd.none )

        ModifyNewEstimateTime estimateTime ->
            let
                newItem =
                    model.newItem

                updatedNewItem =
                    { newItem | userEstimateRunOut = estimateTime }
            in
            ( { model | newItem = updatedNewItem }, Cmd.none )

        ValidateNewEstimateTime ->
            if isValidTimeString model.newItem.userEstimateRunOut then
                ( { model | newItemWarning = "" }, Cmd.none )

            else
                ( { model | newItemWarning = "Estimated Empty needs to be in the format of \"4 weeks\"" }, Cmd.none )

        ModifyNewMaxOnHand maxOnHand ->
            let
                newItem =
                    model.newItem

                updatedNewItem =
                    { newItem | maxOnHand = maxOnHand |> parseValueToInt }
            in
            ( { model | newItem = updatedNewItem }, Cmd.none )

        ModifyNewUnit unit ->
            let
                newItem =
                    model.newItem

                updatedNewItem =
                    { newItem | unit = unit }
            in
            ( { model | newItem = updatedNewItem }, Cmd.none )

        SaveNewItem ->
            if isValidTimeString model.newItem.userEstimateRunOut then
                let
                    newItem =
                        model.newItem

                    updatedNewItem =
                        { newItem | confirmed = True }
                in
                update SaveNewItemServer { model | newItem = updatedNewItem }

            else
                ( model, Cmd.none )

        SaveNewItemServer ->
            ( model
            , Cmd.batch
                [ focusElement
                , Http.post
                    { url = "http://localhost:8000/cupboard/new-item"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "name", Encode.string model.newItem.name )
                                , ( "maxOnHand", Encode.int model.newItem.maxOnHand )
                                , ( "onHand", Encode.int model.newItem.onHand )
                                , ( "unit", Encode.string model.newItem.unit )
                                , ( "userEstimateRunOut", Encode.string model.newItem.userEstimateRunOut )
                                ]
                            )
                    , expect = Http.expectJson GotNewItem mapItem
                    }
                ]
            )

        GotNewItem result ->
            case result of
                Ok item ->
                    let
                        newItem =
                            model.newItem

                        updatedNewItem =
                            { newItem | confirmed = False, name = "" }
                    in
                    ( { model | items = processNewItem model item, newItem = updatedNewItem }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        OnBarMouseDown id barWidth barLeft ->
            ( { model | barDragingWidth = Just barWidth, barDragingLeft = Just barLeft.left, barDragingItemId = id, mouseMoveFocus = Just EstimateOnHandMove }, Cmd.none )

        OnBarMouseUp ->
            ( { model | barDragingWidth = Nothing, barDragingLeft = Nothing, barDragingItemId = "", mouseMoveFocus = Nothing }, Cmd.none )

        BarDragingMouseMove mouseMove ->
            case model.mouseMoveFocus of
                Just EstimateOnHandMove ->
                    case model.restockMode of
                        On ->
                            let
                                id =
                                    model.barDragingItemId
                            in
                            ( { model | items = updateItems model id (buildNewEstimateFromMouseMove model id mouseMove) EstimateOnHand }, Cmd.none )

                        Off ->
                            ( model, Cmd.none )

                Just FilterBarMove ->
                    ( { model | filterPercentage = round (buildPercentageFromMouseMove model mouseMove * 100) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OnFilterBarMouseDown barWidth barLeft ->
            ( { model | barDragingWidth = Just barWidth, barDragingLeft = Just barLeft.left, mouseMoveFocus = Just FilterBarMove }, Cmd.none )

        ToggleRestockMode ->
            ( { model | restockMode = toggleOnOff model.restockMode }, Cmd.none )

        ToggleSettings ->
            ( { model | settings = toggleOnOff model.settings }, Cmd.none )

        SaveChangedItems ->
            ( model
            , Http.post
                { url = "http://localhost:8000/cupboard/items"
                , body = Http.jsonBody (Encode.list Encode.object (buildEncodedItemList (model.items |> filterOutUnchanged)))
                , expect = Http.expectJson GotNewItems (Json.list mapItem)
                }
            )

        GotNewItems result ->
            case result of
                Ok itemsResponse ->
                    ( { model | items = buildNewItemsFromResponse itemsResponse }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


isValidTimeString : String -> Bool
isValidTimeString timeString =
    let
        splitTimeString =
            String.split " " timeString

        amount =
            List.head splitTimeString |> Maybe.withDefault ""

        unit =
            List.drop 1 splitTimeString |> List.head |> Maybe.withDefault "" |> String.toLower
    in
    String.toFloat amount /= Nothing && List.member unit [ "week", "day", "month", "year", "weeks", "days", "months", "years" ]


buildNewItemsFromResponse : ItemsResponse -> List Item
buildNewItemsFromResponse itemsResponse =
    transformItemsResponse itemsResponse


toggleOnOff : Toggle -> Toggle
toggleOnOff toggle =
    if toggle == On then
        Off

    else
        On


filterOutUnchanged : Items -> List Item
filterOutUnchanged items =
    List.filter (\item -> hasEstimateOnHandChanged item False) items


buildEncodedItemList : Items -> List (List ( String, Encode.Value ))
buildEncodedItemList =
    List.map
        (\item ->
            [ ( "id", Encode.string item.id )
            , ( "name", Encode.string item.name )
            , ( "maxOnHand", Encode.int item.maxOnHand )
            , ( "onHand", Encode.int item.estimateOnHand )
            , ( "unit", Encode.string item.unit )
            ]
        )


processNewItem : Model -> ItemResponse -> List Item
processNewItem model newItem =
    List.append (model.items |> List.filter (\item -> item.id /= "new-item")) [ transformItemResponse newItem ]


transformItemsResponse : ItemsResponse -> List Item
transformItemsResponse itemsResponse =
    List.map
        (\itemRes ->
            transformItemResponse itemRes
        )
        itemsResponse


transformItemResponse : ItemResponse -> Item
transformItemResponse itemResponse =
    Item itemResponse.id itemResponse.name itemResponse.estimateDays itemResponse.estimateOnHand itemResponse.maxOnHand itemResponse.unit Nothing (Just itemResponse.estimateOnHand)


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


updateItems : Model -> String -> String -> Prop -> Items
updateItems model id newVal prop =
    model.items |> List.map (\item -> updateItem item newVal id prop)


getItemFromId : Items -> Id -> Items
getItemFromId items id =
    items |> List.filter (\item -> item.id == id)


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

            Unit ->
                { item | unit = newVal }

    else
        item


parseValueToInt : String -> Int
parseValueToInt stringVal =
    Maybe.withDefault 0 (stringVal |> String.toInt)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container", classList [ ( "container--settingsOn", model.settings == On ) ] ]
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
                    [ class "filters__button"
                    , classList [ ( "filters__button--grey", model.restockMode == On ) ]
                    , onClick ToggleRestockMode
                    ]
                    [ text
                        (if model.restockMode == Off then
                            "Restock"

                         else
                            "Exit Restock"
                        )
                    ]
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
                , div [ class "centerBoth" ] [ img [ class "filters__settingsCog", src "/src/svg/cog.svg", onClick ToggleSettings ] [] ]
                ]
            , div [ class "listContainer" ]
                [ div [ class "listContainer__header" ]
                    (if List.length model.items > 0 then
                        buildListHeader model.settings

                     else
                        buildNewItemHeader model.settings
                    )
                , ul [ class "listContainer__rows", classList [ ( "hidden", List.length model.items == 0 ) ] ] (buildRows model)
                , viewNewRow model.newItem model
                ]
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


buildListHeader : Toggle -> List (Html Msg)
buildListHeader settings =
    [ span [] [ text "Item" ]
    , span [] [ text "Estimated Empty In" ]
    , span [] [ text "Estimated Remaining Today" ]
    , span [ classList [ ( "hidden", settings == Off ) ] ] [ text "Restock Quantity" ]
    ]


buildNewItemHeader : Toggle -> List (Html Msg)
buildNewItemHeader settings =
    [ span [] [ text "Item" ]
    , span [] [ text "I might run out in" ]
    , span [] [ text "I currently have" ]
    , span [ classList [ ( "hidden", settings == Off ) ] ] [ text "When I restock I want" ]
    ]


buildRows : Model -> List (Html Msg)
buildRows model =
    model.items |> filterUsingPercentage model |> List.map (\item -> viewItemRow item model.restockMode model.settings)


viewItemRow : Item -> Toggle -> Toggle -> Html Msg
viewItemRow item restockMode settings =
    li [ class "row" ]
        [ input
            [ class "inputBox"
            , onInput (ModifyName item.id)
            , value item.name
            ]
            []
        , div [ class "quantity inputBox" ]
            [ input
                [ class "quantity__edit inputBox__innerEdit"
                , classList [ ( "quantity__edit--excessive", isOverstocked item ) ]
                , value (item.estimateDays |> String.fromInt)
                , disabled True
                ]
                []
            , span [ class "quantity__unit" ] [ text "Days" ]
            ]
        , div [ class "bar tooltipParent", classList [ ( "bar--disabled", restockMode == Off ) ] ]
            [ div [ class "bar__used bar__quantityUsed", onClick (ModifyEstimateOnHandBar item.id (item.maxOnHand |> String.fromInt)) ] []
            , div [ class "bar__quantityExcessive", style "width" (buildQuantityExcessiveWidth item) ] []
            , div [ classList (quantityLeftClassList item), style "width" (buildQuantityRemainingWidth item) ]
                [ div
                    [ class "bar__mainPercentage__lever"
                    , on "mousedown" (onLeverMouseDown (OnBarMouseDown item.id))
                    ]
                    []
                ]
            , div [ class "tooltip" ] [ text ("Estimated Remaining: " ++ (item.estimateOnHand |> String.fromInt) ++ " " ++ item.unit) ]
            ]
        , div [ class "quantity inputBox", classList [ ( "hidden", settings == Off ) ] ]
            [ input
                [ class "quantity__edit inputBox__innerEdit"
                , onInput (ModifyMaxOnHand item.id)
                , value (item.maxOnHand |> String.fromInt)
                ]
                [ text (item.maxOnHand |> String.fromInt) ]
            , span [ class "quantity__unit" ] [ input [ class "quantity__unit__innerEdit inputBox__innerEdit", value item.unit, onInput (ModifyUnit item.id) ] [] ]
            ]
        ]


viewNewRow : NewItem -> Model -> Html Msg
viewNewRow newItem model =
    div []
        [ div
            [ class "listContainer__header"
            , classList [ ( "hidden", shouldHideNewRowHeader newItem model.items ) ]
            , style "margin-top" (newItemRowHeaderMarginTop newItem)
            ]
            (buildNewItemHeader model.settings)
        , div [ class "row" ]
            [ input
                [ class "inputBox"
                , id "new-item-name-input"
                , onInput ModifyNewName
                , value newItem.name
                , placeholder "Add new item..."
                ]
                []
            , div [ class "inputBox time", classList [ ( "inputBox--covered", newItem.name == "" ) ] ]
                [ input [ class "time__input inputBox__innerEdit", value newItem.userEstimateRunOut, onInput ModifyNewEstimateTime, onBlur ValidateNewEstimateTime ] []
                ]
            , div [ class "quantity inputBox", classList [ ( "inputBox--covered", newItem.name == "" ) ] ]
                [ input
                    [ class "quantity__edit inputBox__innerEdit"
                    , onInput ModifyNewOnHand
                    , value (newItem.onHand |> String.fromInt)
                    ]
                    []
                , span [ class "quantity__unit" ] [ input [ class "quantity__unit__innerEdit inputBox__innerEdit", value newItem.unit, onInput ModifyNewUnit ] [] ]
                ]
            , div [ class "quantity inputBox", classList [ ( "hidden", model.settings == Off ), ( "inputBox--covered", newItem.name == "" ) ] ]
                [ input
                    [ class "quantity__edit inputBox__innerEdit"
                    , onInput ModifyNewMaxOnHand
                    , value (newItem.maxOnHand |> String.fromInt)
                    ]
                    [ text (newItem.maxOnHand |> String.fromInt) ]
                , span [ class "quantity__unit" ] [ input [ class "quantity__unit__innerEdit inputBox__innerEdit", value newItem.unit, onInput ModifyNewUnit ] [] ]
                ]
            , div
                [ classList (getConfirmTickClassList newItem)
                , onClick SaveNewItem
                , onKeyDown (\key -> onConfirmKeyDown key)
                , tabindex 0
                ]
                [ img [ src "/src/svg/tick.svg" ] []
                ]
            ]
        , div [ class "warningMessage", classList [ ( "visibilityHidden", model.newItemWarning == "" ) ] ] [ text model.newItemWarning ]
        ]


shouldHideNewRowHeader : NewItem -> Items -> Bool
shouldHideNewRowHeader newItem items =
    if newItem.name /= "" && List.length items == 0 then
        True

    else
        newItem.name == ""


newItemRowHeaderMarginTop : NewItem -> String
newItemRowHeaderMarginTop newItem =
    if newItem.name == "" then
        "0"

    else
        "20px"


onLeverMouseDown : (Float -> Rectangle -> Msg) -> Json.Decoder Msg
onLeverMouseDown msg =
    Json.map2 msg
        (target <| parentElement <| parentElement <| offsetWidth)
        (target <| parentElement <| parentElement <| boundingClientRect)


shouldCoverInputBox : Item -> Bool
shouldCoverInputBox item =
    item.name == ""


onConfirmKeyDown : Int -> Msg
onConfirmKeyDown key =
    if key == 13 then
        SaveNewItem

    else
        NoOp


getConfirmTickClassList : NewItem -> List ( String, Bool )
getConfirmTickClassList newItem =
    [ ( "row__confirmTick", True )
    , ( "hidden", newItem.name == "" )
    , ( "row__confirmTick--hidden", newItem.confirmed )
    ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


quantityLeftClassList : Item -> List ( String, Bool )
quantityLeftClassList item =
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
