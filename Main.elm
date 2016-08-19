module Main exposing (main)

import Html.App as Html
import Model exposing (Model, init)
import Update exposing (update, Msg)
import View exposing (view)
import Keyboard


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.ups Update.KeyMsg ]