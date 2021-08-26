module Sudoku exposing (..)

import Dict exposing (Dict)
import List.Extra as ListE
import Maybe.Extra as Maybe
import Set exposing (Set)


type alias CornerPencilMark =
    Int


type alias CenterPencilMark =
    Int


type alias Coord =
    ( Int, Int )


type alias Definition =
    { row : Int
    , col : Int
    , value : Int
    }


type Cell
    = EmptyCell (Set CornerPencilMark) (Set CenterPencilMark)
    | FilledCell Int
    | GivenCell Int


type alias Grid =
    Dict Coord Cell


type ValidationStatus
    = Valid
    | Invalid


initialGrid : Grid
initialGrid =
    allCoords
        |> List.map (\coord -> ( coord, EmptyCell Set.empty Set.empty ))
        |> Dict.fromList


define : List Definition -> Grid
define defns =
    let
        defnGrid =
            defns
                |> List.map (\{ row, col, value } -> ( ( row, col ), GivenCell value ))
                |> Dict.fromList
    in
    Dict.union defnGrid initialGrid


allCoords : List Coord
allCoords =
    List.range 0 8
        |> ListE.andThen
            (\x ->
                List.range 0 8
                    |> List.map (\y -> ( x, y ))
            )


getCellsInRegion : Coord -> List Coord
getCellsInRegion coord =
    List.range 0 8
        |> List.map coordsInRegion
        |> List.filter (List.member coord)
        |> List.concat


getCellsInColumn : Int -> List Coord
getCellsInColumn col =
    allCoords |> List.filter (\( _, col_ ) -> col_ == col)


getCellsInRow : Int -> List Coord
getCellsInRow row =
    allCoords |> List.filter (\( row_, _ ) -> row_ == row)


affectedCells : Coord -> Set Coord
affectedCells (( row, col ) as coord) =
    [ [ coord ], getCellsInColumn col, getCellsInRow row, getCellsInRegion coord ]
        |> List.concat
        |> Set.fromList


coordsInRegion : Int -> List Coord
coordsInRegion i =
    let
        cols =
            let
                modded =
                    modBy 9 (i * 3)
            in
            [ modded, modded + 1, modded + 2 ]

        rows =
            let
                modded =
                    modBy 3 i
            in
            [ i - modded, i - modded + 1, i - modded + 2 ]
    in
    cols
        |> ListE.andThen (\c -> rows |> List.map (\r -> ( r, c )))


validate_ : (Coord -> Bool) -> Grid -> ValidationStatus
validate_ filter g =
    let
        validateValues : List Int -> ValidationStatus
        validateValues ns =
            ns
                |> Set.fromList
                |> Set.toList
                |> (\ns_ ->
                        if List.length ns_ /= List.length ns then
                            Invalid

                        else
                            Valid
                   )

        getDigit c =
            case c of
                EmptyCell _ _ ->
                    Nothing

                GivenCell i ->
                    Just i

                FilledCell i ->
                    Just i
    in
    allCoords
        |> List.filter filter
        |> List.map (\coord -> Dict.get coord g)
        |> List.map (Maybe.andThen getDigit)
        |> Maybe.values
        |> validateValues


validate : Grid -> ValidationStatus
validate g =
    let
        rowResults =
            List.range 0 8
                |> List.map (\r -> validate_ (\( row, _ ) -> row == r) g)

        colResults =
            List.range 0 8
                |> List.map (\c -> validate_ (\( _, col ) -> col == c) g)

        regionResults =
            List.range 0 8
                |> List.map (\r -> validate_ (\coord -> List.member coord (coordsInRegion r)) g)
    in
    [ rowResults, colResults, regionResults ]
        |> List.concat
        |> (\ls ->
                if List.any (\r -> r == Invalid) ls then
                    Invalid

                else
                    Valid
           )
