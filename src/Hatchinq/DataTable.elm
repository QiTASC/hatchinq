module Hatchinq.DataTable exposing
    ( Config, InfiniteView, LoadingDirection(..), Message, State, View
    , column, configure, expansion, infinite, init, lightenOrDarkenOnHover, plain, rowColor, selection, sortableColumn, externalSortableColumn, update
    )

{-|


# Exposed

@docs Config, InfiniteView, LoadingDirection, Message, State, View
@docs column, configure, expansion, infinite, init, lightenOrDarkenOnHover, plain, rowColor, selection, sortableColumn, externalSortableColumn, update

-}

import Browser.Dom as Dom
import Element exposing (Color, Element, centerX, centerY, fill, height, htmlAttribute, mouseDown, mouseOver, none, paddingEach, pointer, px, scrollbarX, scrollbarY, scrollbars, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toId, toInternalConfig)
import Hatchinq.Checkbox as Checkbox
import Hatchinq.IconButton as IconButton exposing (..)
import Hatchinq.Theme exposing (Theme, arrowTransition, black, icon, lightenOrDarken)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Task



-- TYPES


type DataTableType msg
    = Plain
    | Infinite (InfiniteView msg)


type alias InternalConfig item msg =
    { dataTableType : DataTableType msg
    , selection : Maybe ( item -> Bool, item -> Bool -> msg, Bool -> msg )
    , expansion : Maybe ( item -> Bool, item -> Bool -> msg, item -> Element msg )
    , rowColoring : Maybe (item -> Maybe Color)
    , lighterOrDarkerAmountOnHover : Float -- for colored rows
    }


{-| -}
type alias Config item msg =
    { theme : Theme
    , lift : Message item msg -> msg
    }


type SortMethod item msg
    = Lambda (List item -> List item)
    | Update (Int -> Maybe Bool -> msg)


type Sort item
    = NoSort
    | Increasing Int (Maybe (List item -> List item))
    | Decreasing Int (Maybe (List item -> List item))


{-| -}
type alias State item =
    { hoveredHeader : Maybe Int
    , sort : Sort item
    , scrollPos : ScrollPos
    }


{-| -}
type LoadingDirection
    = Up
    | Down


{-| -}
type alias InfiniteView msg =
    { loadingTop : Maybe Int
    , loadingBottom : Maybe Int
    , loadExtraItems : LoadingDirection -> Maybe { loadCount : Int, excessCount : Int, loadMsg : msg }
    }


{-| -}
init : State item
init =
    { hoveredHeader = Nothing
    , sort = NoSort
    , scrollPos = { scrollTop = 0, contentHeight = 0, containerHeight = 0 }
    }


type Column item msg
    = Column (InnerColumn item msg)


type alias InnerColumn item msg =
    { header : Element msg
    , width : Element.Length
    , viewFunc : Int -> item -> Element msg
    , sorter : Maybe (SortMethod item msg)
    }


{-| -}
column : Element msg -> Element.Length -> (Int -> item -> Element msg) -> Column item msg
column header width toElement =
    Column
        { header = header
        , width = width
        , viewFunc = toElement
        , sorter = Nothing
        }


{-| -}
sortableColumn : Element msg -> Element.Length -> (Int -> item -> Element msg) -> (List item -> List item) -> Column item msg
sortableColumn header width toElement sorter =
    Column
        { header = header
        , width = width
        , viewFunc = toElement
        , sorter = Just (Lambda sorter)
        }


{-| -}
externalSortableColumn : Element msg -> Element.Length -> (Int -> item -> Element msg) -> (Int -> Maybe Bool -> msg) -> Column item msg
externalSortableColumn header width toElement sorter =
    Column
        { header = header
        , width = width
        , viewFunc = toElement
        , sorter = Just (Update sorter)
        }



-- MESSAGES


{-| -}
type Message item msg
    = Sort Int (SortMethod item msg)
    | TableScroll msg Int String (InfiniteView msg) ScrollPos
    | NoOp



-- UPDATE


{-| -}
update : Message item msg -> State item -> ( State item, Cmd msg )
update msg model =
    case msg of
        Sort columnIndex sorter ->
            let
                ( sorterFunc, command ) =
                    case sorter of
                        Lambda func ->
                            ( Just func, \_ -> Cmd.none )

                        Update updateMsg ->
                            ( Nothing
                            , \sortOrder ->
                                Task.perform identity <| Task.succeed (updateMsg columnIndex sortOrder)
                            )
            in
            case model.sort of
                NoSort ->
                    ( { model | sort = Increasing columnIndex sorterFunc }, command (Just True) )

                Increasing index _ ->
                    if index == columnIndex then
                        ( { model | sort = Decreasing columnIndex sorterFunc }, command (Just False) )

                    else
                        ( { model | sort = Increasing columnIndex sorterFunc }, command (Just True) )

                Decreasing index _ ->
                    if index == columnIndex then
                        ( { model | sort = NoSort }, command Nothing )

                    else
                        ( { model | sort = Increasing columnIndex sorterFunc }, command (Just True) )

        TableScroll noOp rowHeight elementId infiniteView scrollPos ->
            if model.scrollPos.scrollTop /= scrollPos.scrollTop then
                if infiniteView.loadingBottom == Nothing && scrolledToBottom 0 scrollPos then
                    let
                        loadExtraItems =
                            infiniteView.loadExtraItems Down

                        cmd =
                            case loadExtraItems of
                                Just { loadCount, excessCount, loadMsg } ->
                                    Cmd.batch
                                        [ Task.perform identity <| Task.succeed loadMsg
                                        , if excessCount > 0 then
                                            Dom.setViewportOf elementId 0 (toFloat (round scrollPos.scrollTop - excessCount * rowHeight))
                                                |> Task.attempt (\_ -> noOp)

                                          else
                                            Cmd.none
                                        ]

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | scrollPos = scrollPos }, cmd )

                else if infiniteView.loadingTop == Nothing && scrolledToTop 0 scrollPos then
                    let
                        loadExtraItems =
                            infiniteView.loadExtraItems Up

                        cmd =
                            case loadExtraItems of
                                Just { loadCount, excessCount, loadMsg } ->
                                    Cmd.batch
                                        [ Task.perform identity <| Task.succeed loadMsg
                                        , if loadCount > 0 then
                                            Dom.setViewportOf elementId 0 (toFloat (round scrollPos.scrollTop + loadCount * rowHeight))
                                                |> Task.attempt (\_ -> noOp)

                                          else
                                            Cmd.none
                                        ]

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | scrollPos = scrollPos }, cmd )

                else if infiniteView.loadingBottom /= Nothing && scrolledToBottom 1 scrollPos then
                    ( { model | scrollPos = scrollPos }
                    , Dom.setViewportOf elementId 0 (toFloat (scrollPos.contentHeight - scrollPos.containerHeight - 1))
                        |> Task.attempt (\_ -> noOp)
                    )

                else if infiniteView.loadingTop /= Nothing && scrolledToTop 1 scrollPos then
                    ( { model | scrollPos = scrollPos }
                    , Dom.setViewportOf elementId 0 (toFloat 1)
                        |> Task.attempt (\_ -> noOp)
                    )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


scrolledToTop : Int -> ScrollPos -> Bool
scrolledToTop tolerance pos =
    round pos.scrollTop <= tolerance


scrolledToBottom : Int -> ScrollPos -> Bool
scrolledToBottom tolerance pos =
    pos.contentHeight - pos.containerHeight - tolerance <= round pos.scrollTop



-- VIEW


{-| -}
type alias View item msg =
    { columns : List (Column item msg)
    , items : List item
    , state : State item
    }


{-| -}
configure : Config item msg -> (List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg)
configure config =
    view config


{-| -}
plain : Attribute (InternalConfig item msg)
plain =
    custom (\v -> { v | dataTableType = Plain })


{-| -}
infinite : InfiniteView msg -> Attribute (InternalConfig item msg)
infinite infiniteView =
    custom (\v -> { v | dataTableType = Infinite infiniteView })


{-| -}
selection : (item -> Bool) -> (item -> Bool -> msg) -> (Bool -> msg) -> Attribute (InternalConfig item msg)
selection selectable select selectAll =
    custom (\v -> { v | selection = Just ( selectable, select, selectAll ) })


{-| -}
expansion : (item -> Bool) -> (item -> Bool -> msg) -> (item -> Element msg) -> Attribute (InternalConfig item msg)
expansion expandable expand expandedContent =
    custom (\v -> { v | expansion = Just ( expandable, expand, expandedContent ) })


{-| -}
rowColor : (item -> Maybe Element.Color) -> Attribute (InternalConfig item msg)
rowColor coloring =
    custom (\v -> { v | rowColoring = Just coloring })


{-| -}
lightenOrDarkenOnHover : Float -> Attribute (InternalConfig item msg)
lightenOrDarkenOnHover amount =
    custom (\v -> { v | lighterOrDarkerAmountOnHover = amount })


view : Config item msg -> List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg
view { theme, lift } attributes data =
    let
        elementAttributes =
            toElement attributes

        defaultConfig =
            { dataTableType = Plain
            , selection = Nothing
            , expansion = Nothing
            , rowColoring = Nothing
            , lighterOrDarkerAmountOnHover = -0.05
            }

        internalConfig =
            defaultConfig |> toInternalConfig attributes

        tableAttributes =
            [ Font.family [ theme.font.main ]
            , Font.size 14
            ]

        createHeader : Column item msg -> Int -> Element msg
        createHeader (Column { header, width, viewFunc, sorter }) columnIndex =
            let
                sortIcon =
                    Element.el
                        ([ centerX
                         , centerY
                         , htmlAttribute <| Html.Attributes.style "will-change" "transform, opacity"
                         , htmlAttribute arrowTransition
                         ]
                            ++ (case data.state.sort of
                                    Increasing index _ ->
                                        if columnIndex == index then
                                            []

                                        else
                                            [ htmlAttribute <| Html.Attributes.style "opacity" "0" ]

                                    Decreasing index _ ->
                                        if columnIndex == index then
                                            [ htmlAttribute <| Html.Attributes.style "transform" "rotate(180deg)" ]

                                        else
                                            [ htmlAttribute <| Html.Attributes.style "opacity" "0" ]

                                    _ ->
                                        [ htmlAttribute <| Html.Attributes.style "opacity" "0" ]
                               )
                        )
                        (icon "expand_less")
            in
            Element.el
                ([ Element.width width ]
                    ++ cellAttributes
                    ++ (case sorter of
                            Just s ->
                                [ Events.onMouseDown <| lift <| Sort columnIndex s
                                , Font.size theme.font.smallerSize
                                , mouseOver [ Font.color black ]
                                , pointer
                                ]

                            Nothing ->
                                [ Font.size theme.font.smallerSize ]
                       )
                )
                (Element.row [] [ header, sortIcon ])

        sorterFunc =
            case data.state.sort of
                NoSort ->
                    identity

                Increasing _ sorter ->
                    Maybe.withDefault identity sorter

                Decreasing _ sorter ->
                    Maybe.withDefault identity sorter

        items =
            sorterFunc data.items

        expansionWidth =
            Element.px theme.sizes.table.rowHeight

        checkbox =
            Checkbox.configure { theme = theme } [ Checkbox.stopPropagation ]

        expansionHeader =
            case internalConfig.expansion of
                Nothing ->
                    []

                Just _ ->
                    [ Element.el [ width expansionWidth ] none ]

        expansionColumn : item -> List (Element msg)
        expansionColumn it =
            case internalConfig.expansion of
                Nothing ->
                    []

                Just ( expanded, onExpansion, expansionContent ) ->
                    [ Element.el
                        ([ width expansionWidth
                         , htmlAttribute <| Html.Attributes.style "will-change" "transform"
                         , htmlAttribute arrowTransition
                         ]
                            ++ (if expanded it then
                                    [ htmlAttribute <| Html.Attributes.style "transform" "rotate(90deg)" ]

                                else
                                    []
                               )
                        )
                        (Element.el [ centerX, centerY ]
                            (IconButton.configure { theme = theme }
                                []
                                { icon = "arrow_right", onPress = Just (onExpansion it (not (expanded it))) }
                            )
                        )
                    ]

        selectionHeader =
            case internalConfig.selection of
                Nothing ->
                    []

                Just ( selected, onSelected, selectAll ) ->
                    let
                        checkboxValue =
                            if List.all selected data.items then
                                Just True

                            else if not (List.any selected data.items) then
                                Just False

                            else
                                Nothing
                    in
                    [ Element.el cellAttributes (checkbox { value = checkboxValue, onChange = Just selectAll }) ]

        selectionColumn : item -> List (Element msg)
        selectionColumn it =
            case internalConfig.selection of
                Nothing ->
                    []

                Just ( selected, onSelected, selectAll ) ->
                    [ Element.el cellAttributes (checkbox { value = Just (selected it), onChange = Just (onSelected it) }) ]

        rowAttributes =
            [ height (Element.px theme.sizes.table.rowHeight), paddingEach theme.sizes.table.rowPadding, Element.width fill ]

        rowHeaderAttributes =
            rowAttributes
                ++ [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                   , Border.color theme.colors.gray.lighter
                   , Font.color theme.colors.gray.dark
                   ]

        cellAttributes =
            [ paddingEach theme.sizes.table.cellPadding ]

        itemAttributes : item -> List (Element.Attribute msg)
        itemAttributes it =
            let
                expansionAttr =
                    case internalConfig.expansion of
                        Nothing ->
                            case internalConfig.selection of
                                Nothing ->
                                    []

                                Just ( selected, onSelected, _ ) ->
                                    [ pointer
                                    , Events.onClick (onSelected it (not (selected it)))
                                    ]

                        Just ( expanded, onExpansion, _ ) ->
                            [ pointer
                            , Events.onClick (onExpansion it (not (expanded it)))
                            ]

                ( rowColorAttr, mouseOverColor ) =
                    case internalConfig.rowColoring of
                        Just rowColoring ->
                            case rowColoring it of
                                Just color ->
                                    ( [ Background.color color ], lightenOrDarken color internalConfig.lighterOrDarkerAmountOnHover )

                                _ ->
                                    ( [], theme.colors.gray.lightest )

                        _ ->
                            ( [], theme.colors.gray.lightest )
            in
            expansionAttr
                ++ rowColorAttr
                ++ (case internalConfig.selection of
                        Nothing ->
                            [ mouseOver [ Background.color mouseOverColor ] ]

                        Just ( selected, onSelected, _ ) ->
                            [ mouseDown [ Background.color theme.colors.secondary.lighter ] ]
                                ++ (if selected it then
                                        [ Background.color theme.colors.secondary.lightest ]

                                    else
                                        [ mouseOver [ Background.color mouseOverColor ] ]
                                   )
                   )
                ++ rowAttributes

        elementId =
            Maybe.withDefault "" (toId attributes) ++ "-internal-scroll"

        extraItems =
            \maybeCount ->
                List.repeat (Maybe.withDefault 0 maybeCount)
                    (Element.row
                        (rowAttributes
                            ++ [ Border.widthEach { bottom = 0, top = 1, left = 0, right = 0 }
                               , Border.color theme.colors.gray.lighter
                               , Background.color theme.colors.gray.lightest
                               ]
                        )
                        []
                    )

        ( extraItemsTop, extraItemsBottom ) =
            case internalConfig.dataTableType of
                Infinite infiniteView ->
                    ( extraItems infiniteView.loadingTop, extraItems infiniteView.loadingBottom )

                _ ->
                    ( [], [] )

        rowDisplay : Int -> item -> Element msg
        rowDisplay rowIndex it =
            let
                borderAttributes =
                    if rowIndex == 0 && List.isEmpty extraItemsTop then
                        []

                    else
                        [ Border.widthEach { bottom = 0, top = 1, left = 0, right = 0 }
                        , Border.color theme.colors.gray.lighter
                        ]
            in
            Element.row
                (itemAttributes it ++ borderAttributes ++ [ width fill ])
                (expansionColumn it
                    ++ selectionColumn it
                    ++ List.indexedMap (\columnIndex (Column { header, width, viewFunc, sorter }) -> Element.el ([ Element.width width ] ++ cellAttributes) (viewFunc rowIndex it)) data.columns
                )

        itemDisplay : Int -> item -> Element msg
        itemDisplay rowIndex it =
            case internalConfig.expansion of
                Nothing ->
                    rowDisplay rowIndex it

                Just ( expanded, onExpansion, expansionContent ) ->
                    if expanded it then
                        Element.column [ width fill ]
                            [ rowDisplay rowIndex it
                            , Element.row [ paddingEach theme.sizes.table.expansionPadding ]
                                [ Element.el [ width expansionWidth ] none
                                , expansionContent it
                                ]
                            ]

                    else
                        Element.column [ width fill ]
                            [ rowDisplay rowIndex it ]

        scrollingAttribute =
            case internalConfig.dataTableType of
                Infinite infiniteView ->
                    [ Html.Events.on "scroll" (Decode.map (lift << TableScroll (lift NoOp) theme.sizes.table.rowHeight elementId infiniteView) decodeScrollPos) |> htmlAttribute ]

                _ ->
                    []
    in
    Element.column
        (tableAttributes
            ++ elementAttributes
            ++ scrollingAttribute
            ++ [ scrollbars ]
        )
        [ Element.row rowHeaderAttributes
            (expansionHeader
                ++ selectionHeader
                ++ List.indexedMap (\columnIndex headerColumn -> createHeader headerColumn columnIndex) data.columns
            )
        , Element.el [ height fill, width fill ]
            (Element.column [ height fill, width fill, Html.Attributes.id elementId |> htmlAttribute ]
                (extraItemsTop
                    ++ List.indexedMap
                        itemDisplay
                        items
                    ++ extraItemsBottom
                )
            )
        ]


type alias ScrollPos =
    { scrollTop : Float
    , contentHeight : Int
    , containerHeight : Int
    }


decodeScrollPos : Decode.Decoder ScrollPos
decodeScrollPos =
    Decode.map3 ScrollPos
        (Decode.at [ "target", "scrollTop" ] Decode.float)
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.map2 Basics.max offsetHeight clientHeight)


offsetHeight : Decode.Decoder Int
offsetHeight =
    Decode.at [ "target", "offsetHeight" ] Decode.int


clientHeight : Decode.Decoder Int
clientHeight =
    Decode.at [ "target", "clientHeight" ] Decode.int
