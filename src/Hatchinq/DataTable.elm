module Hatchinq.DataTable exposing (Config, Message, State, View, column, configure, expansion, infinite, init, plain, selection, sortableColumn, update)

{-|


# Exposed

@docs Config, Message, State, View, column, configure, expansion, infinite, init, plain, selection, sortableColumn, update

-}

import Element exposing (Element, centerX, centerY, fill, height, htmlAttribute, mouseDown, mouseOver, none, paddingEach, pointer, scrollbarX, scrollbarY, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toElement, toInternalView)
import Hatchinq.Checkbox as Checkbox
import Hatchinq.Theme exposing (Theme, arrowTransition, black, icon)
import Html.Attributes



-- TYPES


type DataTableType
    = Plain
    | Infinite


type alias InternalConfig item msg =
    { dataTableType : DataTableType
    , selection : Maybe ( item -> Bool, item -> Bool -> msg, Bool -> msg )
    , expansion : Maybe ( item -> Bool, item -> Bool -> msg, item -> Element msg )
    }


{-| -}
type alias Config item msg =
    { theme : Theme
    , lift : Message item msg -> msg
    }


type Sort item
    = NoSort
    | Increasing Int (List item -> List item)
    | Decreasing Int (List item -> List item)


{-| -}
type alias State item =
    { hoveredHeader : Maybe Int
    , sort : Sort item
    }


{-| -}
init : State item
init =
    { hoveredHeader = Nothing
    , sort = NoSort
    }


type Column item msg
    = Column (InnerColumn item msg)


type alias InnerColumn item msg =
    { header : Element msg
    , width : Element.Length
    , viewFunc : Int -> item -> Element msg
    , sorter : Maybe (List item -> List item)
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
        , sorter = Just sorter
        }



-- MESSAGES


{-| -}
type Message item msg
    = Sort Int (List item -> List item)
    | Select item msg
    | Noop



-- UPDATE


{-| -}
update : Message item msg -> State item -> ( State item, Cmd msg )
update msg model =
    case msg of
        Sort columnIndex sorter ->
            case model.sort of
                NoSort ->
                    ( { model | sort = Increasing columnIndex sorter }, Cmd.none )

                Increasing index _ ->
                    if index == columnIndex then
                        ( { model | sort = Decreasing columnIndex sorter }, Cmd.none )

                    else
                        ( { model | sort = Increasing columnIndex sorter }, Cmd.none )

                Decreasing index _ ->
                    if index == columnIndex then
                        ( { model | sort = NoSort }, Cmd.none )

                    else
                        ( { model | sort = Increasing columnIndex sorter }, Cmd.none )

        _ ->
            ( model, Cmd.none )



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
infinite : Attribute (InternalConfig item msg)
infinite =
    custom (\v -> { v | dataTableType = Infinite })


{-| -}
selection : (item -> Bool) -> (item -> Bool -> msg) -> (Bool -> msg) -> Attribute (InternalConfig item msg)
selection selectable select selectAll =
    custom (\v -> { v | selection = Just ( selectable, select, selectAll ) })


{-| -}
expansion : (item -> Bool) -> (item -> Bool -> msg) -> (item -> Element msg) -> Attribute (InternalConfig item msg)
expansion expandable expand expandedContent =
    custom (\v -> { v | expansion = Just ( expandable, expand, expandedContent ) })


view : Config item msg -> List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg
view { theme, lift } attributes data =
    let
        elementAttributes =
            toElement attributes

        defaultConfig =
            { dataTableType = Plain
            , selection = Nothing
            , expansion = Nothing
            }

        internalConfig =
            defaultConfig |> toInternalView attributes

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
                    sorter

                Decreasing _ sorter ->
                    sorter >> List.reverse

        items =
            sorterFunc data.items

        expansionWidth =
            theme.sizes.table.rowHeight

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
                         , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                         , htmlAttribute arrowTransition
                         ]
                            ++ (if expanded it then
                                    [ htmlAttribute <| Html.Attributes.style "transform" "rotate(90deg)" ]

                                else
                                    []
                               )
                        )
                        (Element.el [ centerX, centerY ] (icon "arrow_right"))
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
            [ height theme.sizes.table.rowHeight, paddingEach theme.sizes.table.rowPadding, Element.width fill ]

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
            in
            expansionAttr
                ++ (case internalConfig.selection of
                        Nothing ->
                            [ mouseOver [ Background.color theme.colors.gray.lightest ] ]

                        Just ( selected, onSelected, _ ) ->
                            [ mouseDown [ Background.color theme.colors.secondary.lighter ] ]
                                ++ (if selected it then
                                        [ Background.color theme.colors.secondary.lightest ]

                                    else
                                        [ mouseOver [ Background.color theme.colors.gray.lightest ] ]
                                   )
                   )
                ++ rowAttributes

        rowDisplay : Int -> item -> Element msg
        rowDisplay rowIndex it =
            let
                borderAttributes =
                    if rowIndex == 0 then
                        []

                    else
                        [ Border.widthEach { bottom = 0, top = 1, left = 0, right = 0 }
                        , Border.color theme.colors.gray.lighter
                        ]
            in
            Element.row
                (itemAttributes it ++ borderAttributes)
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
    in
    Element.column
        (tableAttributes ++ elementAttributes)
        [ Element.row rowHeaderAttributes
            (expansionHeader
                ++ selectionHeader
                ++ List.indexedMap (\columnIndex headerColumn -> createHeader headerColumn columnIndex) data.columns
            )
        , Element.el [ scrollbarY, height fill, width fill ]
            (Element.column [ height fill, width fill ]
                (List.indexedMap
                    itemDisplay
                    items
                )
            )
        ]
