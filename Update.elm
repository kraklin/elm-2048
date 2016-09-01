module Update exposing (..)

import Model exposing (Model)
import Matrix exposing (..)
import Array
import Keyboard
import Random


type Msg
    = KeyMsg Keyboard.KeyCode
    | SpawnTile
    | NewTile Int


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


testAndMove : Model -> (Matrix (Maybe Int) -> Matrix (Maybe Int)) -> ( Model, Cmd Msg )
testAndMove model moveFunc =
    let
        newMatrix =
            moveFunc model.matrix
    in
        if (newMatrix /= model.matrix) then
            update SpawnTile { model | matrix = newMatrix }
        else
            ( model, Cmd.none )


getRowFromMatrix : Int -> Matrix a -> List a
getRowFromMatrix rownum matrix =
    matrix
        |> Matrix.getRow rownum
        |> Maybe.withDefault Array.empty
        |> Array.toList


getColumnFromMatrix : Int -> Matrix a -> List a
getColumnFromMatrix colnum matrix =
    matrix
        |> Matrix.getColumn colnum
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
    let
        makeRow n =
            matrix
                |> getRowFromMatrix n
                |> convertFromMaybes
                |> List.reverse
                |> mergeTiles
                |> castToFilledMaybeList
                |> List.reverse
    in
        [0..3]
            |> List.map makeRow
            |> Matrix.fromList
            |> Maybe.withDefault Matrix.empty


moveDown : Matrix (Maybe Int) -> Matrix (Maybe Int)
moveDown matrix =
    let
        makeColumn n =
            matrix
                |> getColumnFromMatrix n
                |> convertFromMaybes
                |> List.reverse
                |> mergeTiles
                |> castToFilledMaybeList
                |> List.reverse
    in
        [0..3]
            |> List.map makeColumn
            |> Matrix.fromList
            |> Maybe.withDefault Matrix.empty
            |> transpose


moveUp : Matrix (Maybe Int) -> Matrix (Maybe Int)
moveUp matrix =
    let
        makeColumn n =
            matrix
                |> getColumnFromMatrix n
                |> convertFromMaybes
                |> mergeTiles
                |> castToFilledMaybeList
    in
        [0..3]
            |> List.map makeColumn
            |> Matrix.fromList
            |> Maybe.withDefault Matrix.empty
            |> transpose


transpose : Matrix (Maybe a) -> Matrix (Maybe a)
transpose matrix =
    matrix
        |> Matrix.indexedMap
            (\x y v ->
                Maybe.withDefault Nothing (Matrix.get y x matrix)
            )


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
        |> List.map (Maybe.withDefault 0)
        |> List.filter (\n -> n /= 0)


castToFilledMaybeList : List Int -> List (Maybe Int)
castToFilledMaybeList list =
    let
        listOfMaybes =
            List.map Just list

        toFill =
            4 - List.length list
    in
        if (toFill == 0) then
            listOfMaybes
        else
            List.append listOfMaybes (List.repeat toFill Nothing)


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
