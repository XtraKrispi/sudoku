module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict
import Html exposing (div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Set exposing (Set)
import Sudoku exposing (Cell(..), Coord, Definition, Grid, allCoords, define, initialGrid)


type alias Model =
    { grid : Grid
    , selected : Set Coord
    , selecting : Bool
    , modifierPressed : Bool
    }


type Msg
    = ToggleCell Coord
    | KeyPressed String
    | KeyUp String


main : Program (List Definition) Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : List Definition -> ( Model, Cmd Msg )
init defns =
    ( { grid = define defns
      , selected = Set.fromList []
      , selecting = False
      , modifierPressed = False
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
                    model.selected
                        |> Set.filter
                            (\coord ->
                                case Dict.get coord model.grid of
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
                                    (mNum
                                        |> Maybe.map FilledCell
                                        |> Maybe.withDefault EmptyCell
                                        |> Just
                                        |> always
                                    )
                                    b
                            )
                            model.grid
            in
            case key of
                "Backspace" ->
                    ( { model | grid = updateSelectedItemInGrid Nothing }, Cmd.none )

                "Delete" ->
                    ( { model | grid = updateSelectedItemInGrid Nothing }, Cmd.none )

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

                _ ->
                    case String.toInt key of
                        Just 0 ->
                            ( model, Cmd.none )

                        Just num ->
                            ( { model | grid = updateSelectedItemInGrid (Just num) }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        isSelected coord =
            Set.member coord model.selected

        cellContents c =
            div [ class "selection-border" ]
                (case c of
                    EmptyCell ->
                        []

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
            model.grid
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
                                ]
                            , onClick <| ToggleCell coord
                            ]
                            [ cellContents c ]
                    )
    in
    div []
        [ div [ class "grid" ] cells
        ]
