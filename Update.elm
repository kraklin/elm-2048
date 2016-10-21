module Update exposing (..)

import Model exposing (GameState(..), Model, Tile, init)
import Matrix exposing (..)
import Array
import Keyboard
import Random
import Debug


type Msg
    = KeyMsg Keyboard.KeyCode
    | SpawnTile
    | NewTile Int
    | Reset
    | WinGame
    | LoseGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            case code of
                37 ->
                    testAndMove model moveLeft

                38 ->
                    testAndMove model moveUp

                39 ->
                    testAndMove model moveRight

                40 ->
                    testAndMove model moveDown

                _ ->
                    ( model, Cmd.none )

        SpawnTile ->
            let
                coordsCount =
                    model.tiles
                        |> getCoordinatesOfEmptySpace
                        |> List.length
            in
                ( model, Random.generate NewTile (Random.int 1 coordsCount) )

        NewTile position ->
            let
                coordList =
                    getCoordinatesOfEmptySpace model.tiles
            in
                ( { model | tiles = spawnTile coordList position model.tiles }
                , Cmd.none
                )

        WinGame ->
            ( { model | gameState = Won }, Cmd.none )

        LoseGame ->
            ( { model | gameState = NoMoreMoves }, Cmd.none )

        Reset ->
            Model.init


matrixToList : Matrix Tile -> List Tile
matrixToList matrix =
    matrix
        |> Matrix.toIndexedArray
        |> Array.map (\( ( x, y ), value ) -> value)
        |> Array.toList


getRowList : List Tile -> Int -> List Tile
getRowList list row =
    list
        |> List.drop (row * 4)
        |> List.take 4


listToMatrix : List Tile -> Matrix Tile
listToMatrix list =
    let
        makeRow y =
            getRowList list y
    in
        [0..3]
            |> List.map makeRow
            |> Matrix.fromList
            |> Maybe.withDefault Matrix.empty


testAndMove : Model -> (List Tile -> List Tile) -> ( Model, Cmd Msg )
testAndMove model moveFunc =
    let
        tilesList =
            model.tiles
                |> matrixToList

        newMatrix =
            moveFunc tilesList
                |> listToMatrix

        newTilesList =
            newMatrix |> matrixToList
    in
        case model.gameState of
            Model.Playing ->
                if (checkWin newTilesList) then
                    update WinGame { model | tiles = newMatrix }
                else if (checkLose newTilesList) then
                    update LoseGame { model | tiles = newMatrix }
                else if (newMatrix /= model.tiles) then
                    update SpawnTile { model | tiles = newMatrix }
                else
                    ( model, Cmd.none )

            _ ->
                ( model, Cmd.none )


checkWin : List Tile -> Bool
checkWin tiles =
    let
        filteredList =
            tiles
                |> List.filter (\val -> val == Just 2048)
    in
        if ((List.length filteredList) >= 1) then
            True
        else
            False


checkLose : List Tile -> Bool
checkLose tiles =
    [ moveLeft, moveRight, moveDown, moveUp ]
        |> List.map (\fnc -> fnc tiles)
        |> List.all (\movedMatrix -> movedMatrix == tiles)


moveLeft : List Tile -> List Tile
moveLeft tiles =
    let
        makeRow n =
            getRowList tiles n
                |> convertFromMaybes
                |> mergeTiles
                |> castToFilledMaybeList
    in
        [0..3]
            |> List.map makeRow
            |> List.concat


moveRight : List Tile -> List Tile
moveRight tiles =
    tiles
        |> rotateMatrixRight
        |> rotateMatrixRight
        |> moveLeft
        |> rotateMatrixLeft
        |> rotateMatrixLeft


moveDown : List Tile -> List Tile
moveDown tiles =
    tiles
        |> rotateMatrixRight
        |> moveLeft
        |> rotateMatrixLeft


moveUp : List Tile -> List Tile
moveUp tiles =
    tiles
        |> rotateMatrixLeft
        |> moveLeft
        |> rotateMatrixRight


mergeTiles : List Int -> List Int
mergeTiles listToMerge =
    let
        filteredList =
            listToMerge |> List.filter (\i -> i /= 0)
    in
        case filteredList of
            firstItem :: secondItem :: rest ->
                if firstItem == secondItem then
                    (firstItem * 2) :: (mergeTiles rest)
                else
                    firstItem :: mergeTiles (secondItem :: rest)

            _ ->
                listToMerge


convertFromMaybes : List Tile -> List Int
convertFromMaybes list =
    list
        |> List.filterMap identity


castToFilledMaybeList : List Int -> List Tile
castToFilledMaybeList list =
    let
        listArray =
            Array.fromList list
    in
        [0..3]
            |> List.map (\p -> Array.get p listArray)


getCoordinatesOfEmptySpace : Matrix (Maybe a) -> List ( Int, Int )
getCoordinatesOfEmptySpace tiles =
    tiles
        |> Matrix.toIndexedArray
        |> Array.filter (\( coord, val ) -> val == Nothing)
        |> Array.map (\( coord, val ) -> coord)
        |> Array.toList


spawnTile : List ( Int, Int ) -> Int -> Matrix Tile -> Matrix Tile
spawnTile listOfPlaces position tiles =
    let
        selectedCoords =
            listOfPlaces
                |> List.take position
                |> List.reverse
                |> List.head
    in
        case selectedCoords of
            Nothing ->
                tiles

            Just ( x, y ) ->
                tiles
                    |> Matrix.set x y (Just 2)


transposeM : List Tile -> List Tile
transposeM tiles =
    let
        getSwapedCoords p =
            ( snd (positionToCoords p), fst (positionToCoords p) )
                |> coordsToPosition
    in
        tiles
            |> Array.fromList
            |> Array.indexedMap
                (\position v ->
                    (Array.fromList tiles)
                        |> Array.get (getSwapedCoords position)
                        |> Maybe.withDefault Nothing
                )
            |> Array.toList
            |> reverseRows


positionToCoords : Int -> ( Int, Int )
positionToCoords position =
    ( position % 4, position // 4 )


coordsToPosition : ( Int, Int ) -> Int
coordsToPosition ( x, y ) =
    y * 4 + x


reverseRows : List Tile -> List Tile
reverseRows tiles =
    let
        reverseRow n =
            getRowList tiles n
                |> List.reverse
    in
        [0..3]
            |> List.map reverseRow
            |> List.concat


rotateMatrixRight : List Tile -> List Tile
rotateMatrixRight tiles =
    transposeM tiles


rotateMatrixLeft : List Tile -> List Tile
rotateMatrixLeft tiles =
    tiles |> transposeM |> transposeM |> transposeM
