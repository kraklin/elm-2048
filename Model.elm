module Model exposing (Model, init)

import Matrix exposing (..)


type alias Model =
    { matrix : Matrix (Maybe Int)
    }


init : ( Model, Cmd a )
init =
    ( { matrix =
            Matrix.repeat 4 4 (Nothing)
                |> Matrix.set 0 0 (Just 2)
      }
    , Cmd.none
    )
