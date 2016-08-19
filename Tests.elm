module Main exposing (..)

import ElmTest exposing (Test, suite, test, assertEqual)
import Update exposing (..)
import Matrix exposing (Matrix)


mergeTilesSuite : Test
mergeTilesSuite =
    suite "mergeTiles"
        [ test "empty list returns empty list"
            (Update.mergeTiles []
                |> assertEqual []
            )
        , test "with list of two different tiles return those tiles"
            (Update.mergeTiles [ 1, 2 ]
                |> assertEqual [ 1, 2 ]
            )
        , test "with list of every tile different return those tiles"
            (Update.mergeTiles [ 1, 2, 4, 8 ]
                |> assertEqual [ 1, 2, 4, 8 ]
            )
        , test "with list of two same tiles on left merge those tiles to one"
            (Update.mergeTiles [ 2, 2 ]
                |> assertEqual [ 4 ]
            )
        , test "with list of two same tiles followed by another different tile, the different one should stay after merged one"
            (Update.mergeTiles [ 2, 2, 8 ]
                |> assertEqual [ 4, 8 ]
            )
        , test "with list of two same tiles followed by same tile, the last one should stay unmerged after merged one"
            (Update.mergeTiles [ 2, 2, 2 ]
                |> assertEqual [ 4, 2 ]
            )
        , test "with list of first tile different followed by two same tiles the latter tiles should merge"
            (Update.mergeTiles [ 8, 2, 2 ]
                |> assertEqual [ 8, 4 ]
            )
        , test "with list of last two tiles same the last tiles are merged"
            (Update.mergeTiles [ 8, 4, 2, 2 ]
                |> assertEqual [ 8, 4, 4 ]
            )
        , test "with all same tiles, merge each pair"
            (Update.mergeTiles [ 2, 2, 2, 2 ]
                |> assertEqual [ 4, 4 ]
            )
        ]


spawnTileSuite : Test
spawnTileSuite =
    suite "spawn tile"
        [ test "Filter gets correct coordinates of only empty space"
            (let
                initMatrix =
                    (Matrix.repeat 4 4 (Just 2))
                        |> Matrix.set 2 2 Nothing
             in
                Update.getCoordinatesOfEmptySpace initMatrix
                    |> assertEqual [ ( 2, 2 ) ]
            )
        , test "Tile spawns on only one empty space left"
            (let
                initMatrix =
                    (Matrix.repeat 4 4 (Just 2))
                        |> Matrix.set 2 2 Nothing
                listOfCoords =
                    Update.getCoordinatesOfEmptySpace initMatrix
                randPosition = 1
             in
                Update.spawnTile listOfCoords randPosition initMatrix
                    |> assertEqual (Matrix.repeat 4 4 (Just 2))
            )
        ]


all : Test
all =
    suite "Game 2048"
        [ mergeTilesSuite
        , spawnTileSuite
        ]


main : Program Never
main =
    ElmTest.runSuiteHtml all
