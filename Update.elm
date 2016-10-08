module Update exposing (..)

import Model exposing (GameState(..), Model)
import Matrix exposing (..)
import Array
import Keyboard
import Random
import Debug


type Msg
    = KeyMsg Keyboard.KeyCode
    | SpawnTile
    | NewTile Int
    | WinGame


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
                    model.matrix
                        |> getCoordinatesOfEmptySpace
                        |> List.length
            in
                ( model, Random.generate NewTile (Random.int 1 coordsCount) )

        NewTile position ->
            let
                coordList =
                    getCoordinatesOfEmptySpace model.matrix
            in
                ( { model | matrix = spawnTile coordList position model.matrix }
                , Cmd.none
                )

        WinGame ->
            ( { model | gameState = Won }, Cmd.none )


testAndMove : Model -> (Matrix (Maybe Int) -> Matrix (Maybe Int)) -> ( Model, Cmd Msg )
testAndMove model moveFunc =
    let
        newMatrix =
            moveFunc model.matrix
    in
        case model.gameState of
            Model.Playing ->
                if (checkWin newMatrix) then
                    update WinGame { model | matrix = newMatrix }
                else if (newMatrix /= model.matrix) then
                    update SpawnTile { model | matrix = newMatrix }
                else
                    ( model, Cmd.none )

            _ ->
                ( model, Cmd.none )


checkWin : Matrix (Maybe Int) -> Bool
checkWin matrix =
    let
        filteredArray =
            matrix
                |> Matrix.toIndexedArray
                |> Array.filter (\( coord, val ) -> val == Just 2048)
    in
        if ((Array.length filteredArray) >= 1) then
            True
        else
            False


getRowFromMatrix : Int -> Matrix a -> List a
getRowFromMatrix rownum matrix =
    matrix
        |> Matrix.getRow rownum
        |> Maybe.withDefault Array.empty
        |> Array.toList


moveLeft : Matrix (Maybe Int) -> Matrix (Maybe Int)
moveLeft matrix =
    let
        makeRow n =
            matrix
                |> getRowFromMatrix n
                |> convertFromMaybes
                |> mergeTiles
                |> castToFilledMaybeList
    in
        [0..3]
            |> List.map makeRow
            |> Matrix.fromList
            |> Maybe.withDefault Matrix.empty


moveRight : Matrix (Maybe Int) -> Matrix (Maybe Int)
moveRight matrix =
    matrix
        |> rotateMatrixRight
        |> rotateMatrixRight
        |> moveLeft
        |> rotateMatrixLeft
        |> rotateMatrixLeft


moveDown : Matrix (Maybe Int) -> Matrix (Maybe Int)
moveDown matrix =
    matrix
        |> rotateMatrixRight
        |> moveLeft
        |> rotateMatrixLeft


moveUp : Matrix (Maybe Int) -> Matrix (Maybe Int)
moveUp matrix =
    matrix
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


convertFromMaybes : List (Maybe Int) -> List Int
convertFromMaybes list =
    list
        |> List.filterMap identity


castToFilledMaybeList : List Int -> List (Maybe Int)
castToFilledMaybeList list =
    let
        listArray =
            Array.fromList list
    in
        [0..3]
            |> List.map (\p -> Array.get p listArray)


getCoordinatesOfEmptySpace : Matrix (Maybe a) -> List ( Int, Int )
getCoordinatesOfEmptySpace matrix =
    matrix
        |> Matrix.toIndexedArray
        |> Array.filter (\( coord, val ) -> val == Nothing)
        |> Array.map (\( coord, val ) -> coord)
        |> Array.toList


spawnTile : List ( Int, Int ) -> Int -> Matrix (Maybe Int) -> Matrix (Maybe Int)
spawnTile listOfPlaces position matrix =
    let
        selectedCoords =
            listOfPlaces
                |> List.take position
                |> List.reverse
                |> List.head
    in
        case selectedCoords of
            Nothing ->
                matrix

            Just ( x, y ) ->
                matrix
                    |> Matrix.set x y (Just 2)


transposeM : Matrix (Maybe Int) -> Matrix (Maybe Int)
transposeM matrix =
    matrix
        |> Matrix.indexedMap
            (\x y v ->
                Maybe.withDefault Nothing (Matrix.get y x matrix)
            )
        |> reverseRows


reverseRows : Matrix (Maybe Int) -> Matrix (Maybe Int)
reverseRows matrix =
    let
        reverseRow n =
            matrix
                |> getRowFromMatrix n
                |> List.reverse
                |> Debug.log "reversed"
    in
        [0..((Matrix.height matrix) - 1)]
            |> List.map reverseRow
            |> Matrix.fromList
            |> Maybe.withDefault Matrix.empty


rotateMatrixRight : Matrix (Maybe Int) -> Matrix (Maybe Int)
rotateMatrixRight matrix =
    transposeM matrix


rotateMatrixLeft : Matrix (Maybe Int) -> Matrix (Maybe Int)
rotateMatrixLeft matrix =
    matrix |> transposeM |> transposeM |> transposeM
