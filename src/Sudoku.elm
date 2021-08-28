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
    { grid : Dict Coord Cell
    , regions : List (List Coord)
    }


mapGrid : (Dict Coord Cell -> Dict Coord Cell) -> Grid -> Grid
mapGrid fn g =
    { g | grid = fn g.grid }


type ValidationStatus
    = Valid
    | Invalid


initialGrid : Grid
initialGrid =
    { grid =
        allCoords
            |> List.map (\coord -> ( coord, EmptyCell Set.empty Set.empty ))
            |> Dict.fromList
    , regions = defaultRegions
    }


defaultRegions : List (List Coord)
defaultRegions =
    [ [ ( 0, 0 )
      , ( 0, 1 )
      , ( 0, 2 )
      , ( 1, 0 )
      , ( 1, 1 )
      , ( 1, 2 )
      , ( 2, 0 )
      , ( 2, 1 )
      , ( 2, 2 )
      ]
    , [ ( 0, 3 )
      , ( 0, 4 )
      , ( 0, 5 )
      , ( 1, 3 )
      , ( 1, 4 )
      , ( 1, 5 )
      , ( 2, 3 )
      , ( 2, 4 )
      , ( 2, 5 )
      ]
    , [ ( 0, 6 )
      , ( 0, 7 )
      , ( 0, 8 )
      , ( 1, 6 )
      , ( 1, 7 )
      , ( 1, 8 )
      , ( 2, 6 )
      , ( 2, 7 )
      , ( 2, 8 )
      ]
    , [ ( 3, 0 )
      , ( 3, 1 )
      , ( 3, 2 )
      , ( 4, 0 )
      , ( 4, 1 )
      , ( 4, 2 )
      , ( 5, 0 )
      , ( 5, 1 )
      , ( 5, 2 )
      ]
    , [ ( 3, 3 )
      , ( 3, 4 )
      , ( 3, 5 )
      , ( 4, 3 )
      , ( 4, 4 )
      , ( 4, 5 )
      , ( 5, 3 )
      , ( 5, 4 )
      , ( 5, 5 )
      ]
    , [ ( 3, 6 )
      , ( 3, 7 )
      , ( 3, 8 )
      , ( 4, 6 )
      , ( 4, 7 )
      , ( 4, 8 )
      , ( 5, 6 )
      , ( 5, 7 )
      , ( 5, 8 )
      ]
    , [ ( 6, 0 )
      , ( 6, 1 )
      , ( 6, 2 )
      , ( 7, 0 )
      , ( 7, 1 )
      , ( 7, 2 )
      , ( 8, 0 )
      , ( 8, 1 )
      , ( 8, 2 )
      ]
    , [ ( 6, 3 )
      , ( 6, 4 )
      , ( 6, 5 )
      , ( 7, 3 )
      , ( 7, 4 )
      , ( 7, 5 )
      , ( 8, 3 )
      , ( 8, 4 )
      , ( 8, 5 )
      ]
    , [ ( 6, 6 )
      , ( 6, 7 )
      , ( 6, 8 )
      , ( 7, 6 )
      , ( 7, 7 )
      , ( 7, 8 )
      , ( 8, 6 )
      , ( 8, 7 )
      , ( 8, 8 )
      ]
    ]


define : List Definition -> Grid
define defns =
    let
        defnGrid =
            defns
                |> List.map (\{ row, col, value } -> ( ( row, col ), GivenCell value ))
                |> Dict.fromList
    in
    mapGrid (Dict.union defnGrid) initialGrid


allCoords : List Coord
allCoords =
    List.range 0 8
        |> ListE.andThen
            (\x ->
                List.range 0 8
                    |> List.map (\y -> ( x, y ))
            )


getCellsInRegion : List (List Coord) -> Coord -> List Coord
getCellsInRegion regions coord =
    regions
        |> List.filter (List.member coord)
        |> List.concat


getCellsInColumn : Int -> List Coord
getCellsInColumn col =
    allCoords |> List.filter (\( _, col_ ) -> col_ == col)


getCellsInRow : Int -> List Coord
getCellsInRow row =
    allCoords |> List.filter (\( row_, _ ) -> row_ == row)


affectedCells : List (List Coord) -> Coord -> Set Coord
affectedCells regions (( row, col ) as coord) =
    [ [ coord ], getCellsInColumn col, getCellsInRow row, getCellsInRegion regions coord ]
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
                        if List.length ns_ /= 9 then
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
        |> List.map (\coord -> Dict.get coord g.grid)
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
            g.regions
                |> List.map (\r -> validate_ (\coord -> List.member coord r) g)
    in
    [ rowResults, colResults, regionResults ]
        |> List.concat
        |> (\ls ->
                if List.any (\r -> r == Invalid) ls then
                    Invalid

                else
                    Valid
           )
