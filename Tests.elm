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


moveSuite : Test
moveSuite =
    suite "Move test"
        [ test "Move left"
            (let
                initMatrix =
                    (Matrix.repeat 4 4 (Nothing))
                        |> Matrix.set 1 1 (Just 2)
                        |> Matrix.set 2 1 (Just 2)

                finalMatrix =
                    (Matrix.repeat 4 4 (Nothing))
                        |> Matrix.set 0 1 (Just 4)
             in
                Update.moveLeft initMatrix
                    |> assertEqual finalMatrix
            )
        , test "Move down"
            (let
                initMatrix =
                    (Matrix.repeat 4 4 (Nothing))
                        |> Matrix.set 1 1 (Just 2)
                        |> Matrix.set 1 2 (Just 2)

                finalMatrix =
                    (Matrix.repeat 4 4 (Nothing))
                        |> Matrix.set 1 3 (Just 4)
             in
                Update.moveDown initMatrix
                    |> assertEqual finalMatrix
            )
        ]


rotationTests : Test
rotationTests =
    suite "rotation"
        [ test "rotate to left"
            (let
                initialMatrix =
                    (Matrix.fromList
                        [ [1..3]
                        , [4..6]
                        , [7..9]
                        ]
                    )
                    |> Maybe.withDefault Matrix.empty

                finalMatrix =
                    (Matrix.fromList
                        [ [ 7, 4, 1 ]
                        , [ 8, 5, 2 ]
                        , [ 9, 6, 3 ]
                        ]
                    )
                    |> Maybe.withDefault Matrix.empty
             in
                initialMatrix
                    |> Update.rotateMatrixRight
                    |> assertEqual finalMatrix
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

                randPosition =
                    1
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
        , moveSuite
        , rotationTests
        ]


main : Program Never
main =
    ElmTest.runSuiteHtml all
