module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict
import Html exposing (button, div, input, label, span, text)
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (onCheck, onClick)
import Json.Decode as Decode
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Set exposing (Set)
import Sudoku exposing (Cell(..), Coord, Definition, Grid, ValidationStatus(..), affectedCells, define, validate)


type alias Settings =
    { showAffectedCells : Bool
    , highlightSameNumbers : Bool
    }


type Validation
    = NotSelected
    | Incomplete
    | Complete


type EntryMode
    = Fill
    | Corner
    | Center


cycleEntryMode : EntryMode -> EntryMode
cycleEntryMode em =
    case em of
        Fill ->
            Corner

        Corner ->
            Center

        Center ->
            Fill


type alias Model =
    { initialDefs : List Definition
    , data : Grid
    , selected : Set Coord
    , selecting : Bool
    , modifierPressed : Bool
    , settings : Settings
    , validation : Validation
    , entryMode : EntryMode
    }


type Msg
    = ToggleCell Coord
    | KeyPressed String
    | KeyUp String
    | SetHighlightSameNumbers Bool
    | SetShowAffectedCells Bool
    | Validate
    | Reset
    | ChangeEntryMode EntryMode


main : Program (List Definition) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : List Definition -> ( Model, Cmd Msg )
init defns =
    ( { initialDefs = defns
      , data = define defns
      , selected = Set.fromList []
      , selecting = False
      , modifierPressed = False
      , settings =
            { showAffectedCells = False
            , highlightSameNumbers = False
            }
      , validation = NotSelected
      , entryMode = Fill
      }
    , Cmd.none
    )


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map KeyPressed keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Validate ->
            ( { model
                | validation =
                    if validate model.data == Valid then
                        Complete

                    else
                        Incomplete
              }
            , Cmd.none
            )

        SetHighlightSameNumbers v ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings | highlightSameNumbers = v }
            in
            ( { model | settings = newSettings }, Cmd.none )

        SetShowAffectedCells v ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings | showAffectedCells = v }
            in
            ( { model | settings = newSettings }, Cmd.none )

        ToggleCell coord ->
            let
                newSelected =
                    if Set.member coord model.selected then
                        Set.remove coord model.selected

                    else if model.modifierPressed then
                        Set.insert coord model.selected

                    else
                        Set.singleton coord
            in
            ( { model | selected = newSelected }, Cmd.none )

        Reset ->
            init model.initialDefs

        ChangeEntryMode em ->
            ( { model | entryMode = em }, Cmd.none )

        KeyUp "Meta" ->
            ( { model | modifierPressed = False }, Cmd.none )

        KeyUp _ ->
            ( model, Cmd.none )

        KeyPressed key ->
            let
                updateSelected fst snd =
                    model.selected
                        |> Set.toList
                        |> List.head
                        |> Maybe.map (Set.singleton << Tuple.mapBoth fst snd)
                        |> Maybe.withDefault (Set.singleton ( 5, 5 ))

                updateSelectedItemInGrid mNum =
                    let
                        g =
                            model.data

                        newG =
                            model.selected
                                |> Set.filter
                                    (\coord ->
                                        case Dict.get coord model.data.grid of
                                            Nothing ->
                                                False

                                            Just (GivenCell _) ->
                                                False

                                            _ ->
                                                True
                                    )
                                |> Set.foldl
                                    (\a b ->
                                        Dict.update a
                                            (\mVal ->
                                                case ( model.entryMode, mVal ) of
                                                    ( Fill, _ ) ->
                                                        mNum
                                                            |> Maybe.map FilledCell
                                                            |> Maybe.withDefault (EmptyCell Set.empty Set.empty)
                                                            |> Just

                                                    ( Center, Just (EmptyCell corner center) ) ->
                                                        mNum
                                                            |> Maybe.map
                                                                (\num ->
                                                                    if Set.member num center then
                                                                        EmptyCell corner (Set.remove num center)

                                                                    else
                                                                        EmptyCell corner (Set.insert num center)
                                                                )
                                                            |> Maybe.withDefault (EmptyCell Set.empty Set.empty)
                                                            |> Just

                                                    ( Corner, Just (EmptyCell corner center) ) ->
                                                        mNum
                                                            |> Maybe.map
                                                                (\num ->
                                                                    if Set.member num corner then
                                                                        EmptyCell (Set.remove num corner) center

                                                                    else
                                                                        EmptyCell (Set.insert num corner) center
                                                                )
                                                            |> Maybe.withDefault (EmptyCell Set.empty Set.empty)
                                                            |> Just

                                                    _ ->
                                                        Just (EmptyCell Set.empty Set.empty)
                                            )
                                            b
                                    )
                                    g.grid
                    in
                    { g | grid = newG }
            in
            case key of
                "Backspace" ->
                    ( { model | data = updateSelectedItemInGrid Nothing }, Cmd.none )

                "Delete" ->
                    ( { model | data = updateSelectedItemInGrid Nothing }, Cmd.none )

                "Meta" ->
                    ( { model | modifierPressed = True }, Cmd.none )

                "ArrowUp" ->
                    ( { model | selected = updateSelected (\r -> max 0 (r - 1)) identity }, Cmd.none )

                "ArrowDown" ->
                    ( { model | selected = updateSelected (\r -> min 8 (r + 1)) identity }, Cmd.none )

                "ArrowLeft" ->
                    ( { model | selected = updateSelected identity (\c -> max 0 (c - 1)) }, Cmd.none )

                "ArrowRight" ->
                    ( { model | selected = updateSelected identity (\c -> min 8 (c + 1)) }, Cmd.none )

                " " ->
                    ( { model | entryMode = cycleEntryMode model.entryMode }, Cmd.none )

                _ ->
                    case String.toInt key of
                        Just 0 ->
                            ( model, Cmd.none )

                        Just num ->
                            ( { model | data = updateSelectedItemInGrid (Just num) }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        isSelected coord =
            Set.member coord model.selected

        isHighlighted coord =
            model.selected
                |> Set.toList
                |> ListE.andThen (Set.toList << affectedCells model.data.regions)
                |> List.member coord

        isSameNumber coord =
            let
                cellNum =
                    model.data.grid
                        |> Dict.get coord
                        |> Maybe.andThen
                            (\cell ->
                                case cell of
                                    GivenCell num ->
                                        Just num

                                    FilledCell num ->
                                        Just num

                                    _ ->
                                        Nothing
                            )
            in
            model.selected
                |> Set.toList
                |> List.map (\c -> Dict.get c model.data.grid)
                |> MaybeE.values
                |> List.any
                    (\cell ->
                        case cell of
                            GivenCell num ->
                                Just num == cellNum

                            FilledCell num ->
                                Just num == cellNum

                            _ ->
                                False
                    )

        cornerViews corners =
            div [ class "corner-pencil-marks" ]
                (List.range 1 9
                    |> List.map
                        (\n ->
                            div
                                [ classList
                                    [ ( "corner", True )
                                    , ( "corner-" ++ String.fromInt n, True )
                                    ]
                                ]
                                [ if Set.member n corners then
                                    text <| String.fromInt n

                                  else
                                    text ""
                                ]
                        )
                )

        centerView centers =
            centers
                |> Set.toList
                |> List.map (\num -> div [ class "center" ] [ text (String.fromInt num) ])
                |> div [ class "center-pencil-marks" ]

        cellContents c =
            div [ class "selection-border" ]
                (case c of
                    EmptyCell corners centers ->
                        [ corners
                            |> cornerViews
                        , centers
                            |> centerView
                        ]

                    GivenCell i ->
                        [ span [ class "given" ] [ text <| String.fromInt i ] ]

                    FilledCell i ->
                        [ span [] [ text <| String.fromInt i ] ]
                )

        borderClass row col =
            let
                rightBorderClass =
                    if modBy 3 (col + 1) == 0 && col < 8 then
                        "double-right-border"

                    else if col == 8 then
                        "no-border-right"

                    else
                        ""

                bottomBorderClass =
                    if modBy 3 (row + 1) == 0 && row < 8 then
                        "double-bottom-border"

                    else if row == 8 then
                        "no-border-bottom"

                    else
                        ""
            in
            [ rightBorderClass, bottomBorderClass ]
                |> List.filter (\s -> s /= "")
                |> String.join " "

        cells =
            model.data.grid
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map
                    (\( ( row, col ) as coord, c ) ->
                        div
                            [ classList <|
                                [ ( "row-" ++ String.fromInt row, True )
                                , ( "col-" ++ String.fromInt col, True )
                                , ( borderClass row col, borderClass row col /= "" )
                                , ( "selected", isSelected coord )
                                , ( "highlighted", model.settings.showAffectedCells && isHighlighted coord )
                                , ( "same-number", model.settings.highlightSameNumbers && isSameNumber coord )
                                ]
                            , onClick <| ToggleCell coord
                            ]
                            [ cellContents c ]
                    )
    in
    div [ class "container" ]
        [ div [ class "grid" ] cells
        , div [ class "controls" ]
            [ button [ onClick Validate ]
                [ text "Validate"
                ]
            , button [ onClick Reset ]
                [ text "Reset"
                ]
            , div []
                [ text <|
                    case model.validation of
                        NotSelected ->
                            ""

                        Incomplete ->
                            "Not done yet!"

                        Complete ->
                            "You did it!"
                ]
            , div []
                [ label [] [ input [ type_ "checkbox", onCheck SetHighlightSameNumbers, checked model.settings.highlightSameNumbers ] [], text "Highlight same number" ]
                ]
            , div []
                [ label [] [ input [ type_ "checkbox", onCheck SetShowAffectedCells, checked model.settings.showAffectedCells ] [], text "View affected cells" ] ]
            , div []
                [ label [] [ input [ type_ "radio", name "fillMode", checked <| model.entryMode == Fill, onCheck <| always (ChangeEntryMode Fill) ] [], text "Normal" ]
                , label [] [ input [ type_ "radio", name "fillMode", checked <| model.entryMode == Corner, onCheck <| always (ChangeEntryMode Corner) ] [], text "Corner" ]
                , label [] [ input [ type_ "radio", name "fillMode", checked <| model.entryMode == Center, onCheck <| always (ChangeEntryMode Center) ] [], text "Center" ]
                ]
            ]
        ]
