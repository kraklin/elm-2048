module Model exposing (Model, GameState(..), init, allTilesMatrix, Tile)

import Matrix exposing (..)


type alias Model =
    { tiles : Matrix (Maybe Int)
    , gameState : GameState
    }


type alias Tile =
    Maybe Int


type GameState
    = Playing
    | Won
    | NoMoreMoves


init : ( Model, Cmd a )
init =
    ( { tiles =
            --allTilesMatrix
            Matrix.repeat 4 4 (Nothing)
                |> Matrix.set 0 0 (Just 2)
      , gameState = Playing
      }
    , Cmd.none
    )


allTilesMatrix : Matrix (Maybe Int)
allTilesMatrix =
    Matrix.repeat 4 4 (Nothing)
        |> Matrix.set 0 0 (Just 2)
        |> Matrix.set 0 1 (Just 4)
        |> Matrix.set 0 2 (Just 8)
        |> Matrix.set 0 3 (Just 16)
        |> Matrix.set 1 0 (Just 32)
        |> Matrix.set 1 1 (Just 64)
        |> Matrix.set 1 2 (Just 128)
        |> Matrix.set 1 3 (Just 256)
        |> Matrix.set 2 0 (Just 512)
        |> Matrix.set 2 1 (Just 1024)
        |> Matrix.set 2 2 (Just 2048)
