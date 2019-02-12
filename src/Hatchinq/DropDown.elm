module Hatchinq.DropDown exposing (Config, Message, State, configure, dropDownCount, filled, init, label, outlined, searchable, update)

{-|


# Exposed

@docs Config, Message, State, configure, dropDownCount, filled, init, label, outlined, searchable, update

-}

import Dict
import Element exposing (Element, Length, alignRight, below, centerY, column, el, fill, focused, height, htmlAttribute, inFront, mouseOver, padding, paddingEach, paddingXY, pointer, px, scale, scrollbarY, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig, toWidth)
import Hatchinq.Theme as Theme exposing (Theme, arrowTransition, icon, textWithEllipsis, transition)
import Hatchinq.Util exposing (arrowDownKeyCode, arrowUpKeyCode, enterKeyCode, escapeKeyCode, keysDownAttribute)
import Html.Attributes
import Task



-- TYPES


type DropDownType
    = Outlined
    | Filled


type alias InternalConfig =
    { dropDownType : DropDownType
    , searchable : Bool
    , dropDownCount : Int
    , label : Maybe String
    }


{-| -}
type alias Config item msg =
    { theme : Theme
    , lift : Message item msg -> msg
    }


type Query
    = Query String


type UiState
    = Closed
    | Open Query


{-| -}
type alias State =
    { uiState : UiState
    , focusedItem : Maybe Int
    }


{-| -}
init : State
init =
    State Closed Nothing



-- MESSAGES


{-| -}
type Message item msg
    = OpenSelect (Maybe Int)
    | CloseSelect
    | Select item (Maybe msg)
    | SearchInput Query
    | ArrowUpPress (Maybe Int)
    | ArrowDownPress (Maybe Int)
    | Noop



-- UPDATE


{-| -}
update : Message item msg -> State -> ( State, Cmd msg )
update msg model =
    case msg of
        OpenSelect maybeFocusedIndex ->
            ( { model | uiState = Open (Query ""), focusedItem = maybeFocusedIndex }, Cmd.none )

        CloseSelect ->
            ( { model | uiState = Closed, focusedItem = Nothing }, Cmd.none )

        Select value onSelect ->
            ( { model | uiState = Closed, focusedItem = Nothing }
            , case onSelect of
                Just onSelectMsg ->
                    Task.succeed onSelectMsg |> Task.perform identity

                Nothing ->
                    Cmd.none
            )

        SearchInput query ->
            ( { model | uiState = Open query, focusedItem = Nothing }, Cmd.none )

        ArrowUpPress maybeFocusedIndex ->
            let
                uiState =
                    case model.uiState of
                        Closed ->
                            Open (Query "")

                        _ ->
                            model.uiState

                focusedItem =
                    case model.focusedItem of
                        Just index ->
                            Just (index - 1)

                        Nothing ->
                            Just (Maybe.withDefault -1 maybeFocusedIndex)
            in
            ( { model | uiState = uiState, focusedItem = focusedItem }, Cmd.none )

        ArrowDownPress maybeFocusedIndex ->
            let
                uiState =
                    case model.uiState of
                        Closed ->
                            Open (Query "")

                        _ ->
                            model.uiState

                focusedItem =
                    case model.focusedItem of
                        Just index ->
                            Just (index + 1)

                        Nothing ->
                            Just (Maybe.withDefault 0 maybeFocusedIndex)
            in
            ( { model | uiState = uiState, focusedItem = focusedItem }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


{-| -}
type alias View item msg =
    { items : List item
    , itemToString : item -> String
    , value : Maybe item
    , onChange : Maybe (item -> msg)
    , state : State
    }


{-| -}
configure : Config item msg -> (List (Attribute InternalConfig) -> View item msg -> Element msg)
configure config =
    view config


{-| -}
outlined : Attribute InternalConfig
outlined =
    custom (\v -> { v | dropDownType = Outlined })


{-| -}
filled : Attribute InternalConfig
filled =
    custom (\v -> { v | dropDownType = Filled })


{-| -}
searchable : Attribute InternalConfig
searchable =
    custom (\v -> { v | searchable = True })


{-| -}
dropDownCount : Int -> Attribute InternalConfig
dropDownCount count =
    custom (\v -> { v | dropDownCount = count })


{-| -}
label : String -> Attribute InternalConfig
label labelString =
    custom (\v -> { v | label = Just labelString })


view : Config item msg -> List (Attribute InternalConfig) -> View item msg -> Element msg
view config attributes data =
    let
        { theme, lift } =
            config

        defaultConfig =
            { dropDownType = Filled
            , searchable = False
            , dropDownCount = 10
            , label = Nothing
            }

        internalConfig =
            defaultConfig |> toInternalConfig attributes

        disabled =
            data.onChange == Nothing

        elementAttributes =
            toElement attributes

        widthLength =
            Maybe.withDefault (px 280) (toWidth attributes)

        state =
            data.state

        converter =
            data.itemToString

        standardLabelAttributes =
            [ width fill, height shrink, Font.family [ theme.font.main ], htmlAttribute transition, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ]

        labelColor =
            if disabled then
                theme.colors.gray.color

            else
                theme.colors.gray.dark

        ( ( query, text ), placeholder, labelAttributes ) =
            case state.uiState of
                Open (Query q) ->
                    ( ( Query q, q )
                    , if q == "" then
                        Maybe.map
                            (\t ->
                                Input.placeholder [] (el [ width fill, height fill, paddingEach { left = 0, top = 5, right = 0, bottom = 0 } ] (textWithEllipsis <| converter t))
                            )
                            data.value

                      else
                        Nothing
                    , paddingXY 12 8 :: Font.size theme.font.smallerSize :: Font.color theme.colors.secondary.color :: standardLabelAttributes
                    )

                Closed ->
                    ( ( Query "", Maybe.withDefault "" <| Maybe.map converter data.value )
                    , Nothing
                    , case data.value of
                        Just _ ->
                            pointer :: paddingXY 12 8 :: Font.size theme.font.smallerSize :: Font.color labelColor :: standardLabelAttributes

                        Nothing ->
                            pointer :: paddingXY 12 20 :: Font.size theme.font.defaultSize :: Font.color labelColor :: standardLabelAttributes
                    )

        labelElement =
            Element.el labelAttributes (textWithEllipsis (Maybe.withDefault "" internalConfig.label))

        inputStyledAttributes =
            case internalConfig.dropDownType of
                Filled ->
                    [ Background.color theme.colors.gray.lighter
                    , Border.roundEach { topLeft = 4, topRight = 4, bottomLeft = 0, bottomRight = 0 }
                    , Border.widthEach { left = 0, top = 0, right = 0, bottom = 2 }
                    , Border.color
                        (if disabled then
                            Theme.transparent

                         else
                            theme.colors.gray.color
                        )
                    , Font.color
                        (if disabled then
                            theme.colors.gray.withAlpha 0.48

                         else
                            Theme.black
                        )
                    , focused
                        [ Background.color theme.colors.gray.lightest
                        , Border.color theme.colors.secondary.color
                        ]
                    ]

                Outlined ->
                    [ Border.rounded 4
                    , Border.width
                        (if state.uiState == Closed then
                            1

                         else
                            2
                        )
                    , Border.color
                        (if disabled then
                            theme.colors.gray.lightest

                         else
                            theme.colors.gray.color
                        )
                    , Font.color
                        (if disabled then
                            theme.colors.gray.withAlpha 0.48

                         else
                            Theme.black
                        )
                    , focused
                        [ Border.color theme.colors.secondary.color ]
                    ]

        inputHoverAttributes =
            case internalConfig.dropDownType of
                Filled ->
                    if not disabled && state.uiState == Closed then
                        [ pointer
                        , mouseOver
                            [ Background.color theme.colors.gray.light
                            , Border.color theme.colors.gray.color
                            ]
                        ]

                    else
                        []

                Outlined ->
                    if not disabled && state.uiState == Closed then
                        [ pointer
                        , mouseOver
                            [ Border.color Theme.black
                            ]
                        ]

                    else
                        []

        items =
            case state.uiState of
                Open (Query q) ->
                    filteredValues data q

                Closed ->
                    data.items

        itemsSize =
            List.length data.items

        focusedItem =
            Maybe.withDefault Nothing (Maybe.map (\index -> List.head (List.drop (modBy itemsSize index) data.items)) data.state.focusedItem)

        selectedItemIndex =
            Maybe.withDefault Nothing
                (Maybe.map
                    (\selectedItem ->
                        List.head
                            (List.filterMap
                                (\( i, item ) ->
                                    if selectedItem == item then
                                        Just i

                                    else
                                        Nothing
                                )
                                (List.indexedMap (\i item -> ( i, item )) data.items)
                            )
                    )
                    data.value
                )

        selectMessage =
            case focusedItem of
                Just item ->
                    Select item (Maybe.map (\onChange -> onChange item) data.onChange)

                Nothing ->
                    case state.uiState of
                        Closed ->
                            OpenSelect selectedItemIndex

                        _ ->
                            Noop

        keyDownAttributes =
            [ keysDownAttribute
                (Dict.fromList
                    [ ( arrowUpKeyCode, lift (ArrowUpPress selectedItemIndex) )
                    , ( arrowDownKeyCode, lift (ArrowDownPress selectedItemIndex) )
                    , ( enterKeyCode, lift selectMessage )
                    , ( escapeKeyCode, lift CloseSelect )
                    ]
                )
            ]

        inputAttributes =
            Font.family [ theme.font.main ]
                :: Font.size theme.font.defaultSize
                :: width (px 280)
                :: height (px 56)
                :: (htmlAttribute <| Html.Attributes.style "display" "inline-block")
                :: (htmlAttribute <| Html.Attributes.style "overflow" "hidden")
                :: (htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis")
                :: paddingEach { left = 12, top = 20, right = 32, bottom = 4 }
                :: inFront labelElement
                :: Events.onClick (lift <| OpenSelect selectedItemIndex)
                :: (Events.onLoseFocus <|
                        lift <|
                            if disabled then
                                Noop

                            else
                                CloseSelect
                   )
                :: (Element.htmlAttribute <| Html.Attributes.disabled disabled)
                :: keyDownAttributes

        arrowAttributes =
            (htmlAttribute <| Html.Attributes.style "will-change" "transform")
                :: pointer
                :: alignRight
                :: centerY
                :: padding 8
                :: htmlAttribute arrowTransition
                :: (case state.uiState of
                        Open _ ->
                            [ Font.color theme.colors.secondary.color
                            , htmlAttribute <| Html.Attributes.style "transform" "rotate(180deg) translateY(-5px)"
                            ]

                        Closed ->
                            [ Font.color labelColor, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ]
                   )
    in
    el
        (below (dropdownBody config internalConfig widthLength state query data)
            :: inFront
                (el arrowAttributes (icon "arrow_drop_down"))
            :: elementAttributes
        )
        (Input.text
            (inputStyledAttributes ++ inputAttributes ++ inputHoverAttributes ++ elementAttributes)
            { onChange = \newQuery -> lift <| SearchInput (Query newQuery)
            , text = text
            , placeholder = placeholder
            , label = Input.labelHidden (Maybe.withDefault "" internalConfig.label)
            }
        )


dropdownItem : Config item msg -> Length -> Bool -> item -> Maybe item -> View item msg -> Element msg
dropdownItem { theme, lift } widthLength focused value selectedItem data =
    let
        onSelectMessage =
            Maybe.map (\onChange -> onChange value) data.onChange

        itemAttributes =
            (Events.onMouseDown <| lift <| Select value onSelectMessage)
                :: pointer
                :: Font.family [ theme.font.main ]
                :: Font.size theme.font.defaultSize
                :: width widthLength
                :: height (px 56)
                :: paddingEach { left = 12, top = 20, right = 12, bottom = 4 }
                :: (if Just value == selectedItem then
                        if focused then
                            [ Background.color theme.colors.secondary.light ]

                        else
                            [ Background.color theme.colors.secondary.lighter ]

                    else if focused then
                        [ Background.color theme.colors.gray.light
                        , mouseOver [ Background.color theme.colors.gray.color ]
                        ]

                    else
                        [ mouseOver [ Background.color theme.colors.gray.lighter ] ]
                   )
    in
    el itemAttributes (textWithEllipsis <| data.itemToString value)


dropdownBody : Config item msg -> InternalConfig -> Length -> State -> Query -> View item msg -> Element msg
dropdownBody config internalConfig widthLength { uiState, focusedItem } (Query query) data =
    let
        items =
            filteredValues data query

        itemHeightPx =
            56

        bodyHeight =
            px (min (max 1 internalConfig.dropDownCount) (List.length items) * itemHeightPx + 16)

        standardBodyAttributes =
            [ Border.rounded 4
            , Background.color Theme.white
            , height bodyHeight
            , width widthLength
            , paddingXY 0 8
            , htmlAttribute <| Html.Attributes.style "will-change" "transform, opacity"
            , htmlAttribute <| Html.Attributes.style "transform-origin" "center top 0px"
            , htmlAttribute <| Html.Attributes.style "box-shadow" "0 5px 5px -3px rgba(0,0,0,.2), 0 8px 10px 1px rgba(0,0,0,.14), 0 3px 14px 2px rgba(0,0,0,.12)"
            , htmlAttribute <| Html.Attributes.style "-webkit-box-shadow" "0 5px 5px -3px rgba(0,0,0,.2), 0 8px 10px 1px rgba(0,0,0,.14), 0 3px 14px 2px rgba(0,0,0,.12)"
            , htmlAttribute transition
            ]

        bodyAttributes =
            case uiState of
                Open _ ->
                    htmlAttribute (Html.Attributes.style "opacity" "1")
                        :: scale 1
                        :: standardBodyAttributes

                Closed ->
                    htmlAttribute (Html.Attributes.style "opacity" "0")
                        :: scale 0
                        :: standardBodyAttributes

        itemsSize =
            List.length items

        focusedIndex =
            Maybe.map (\index -> modBy itemsSize index) focusedItem
    in
    el
        bodyAttributes
        (column [ width widthLength, scrollbarY ] (List.indexedMap (\index value -> dropdownItem config widthLength (Just index == focusedIndex) value data.value data) items))


filteredValues : View item msg -> String -> List item
filteredValues data query =
    List.filter (\it -> matchQuery query (data.itemToString it)) data.items


matchQuery : String -> String -> Bool
matchQuery needle haystack =
    String.contains (String.toLower needle) (String.toLower haystack)
