module View exposing (view)

import Update exposing (Msg)
import Model exposing (Model)
import Html exposing (Html, div, text, h1, h2)
import Html.Attributes exposing (style)
import Matrix exposing (..)
import Array exposing (..)
import Dict exposing (..)


-- View


baseTileStyle : List ( String, String )
baseTileStyle =
    [ ( "width", "100px" )
    , ( "height", "100px" )
    , ( "margin", "5px" )
    , ( "display", "inline-block" )
    , ( "line-height", "100px" )
    , ( "font-size", "60px" )
    , ( "font-weight", "bold" )
    , ( "text-align", "center" )
    ]


valueColorMap : Dict Int ( String, String )
valueColorMap =
    Dict.fromList
        [ ( 2, ( "#eee4da", "black" ) )
        , ( 4, ( "#ede0c8", "black" ) )
        , ( 8, ( "#f2b179", "#f9f6f2" ) )
        , ( 16, ( "#f59563", "#f9f6f2" ) )
        , ( 32, ( "#f67c5f", "#f9f6f2" ) )
        , ( 64, ( "#f65e3b", "#f9f6f2" ) )
        , ( 128, ( "#edcf72", "#f9f6f2" ) )
        , ( 256, ( "#edcc61", "#f9f6f2" ) )
        , ( 512, ( "#edc850", "#f9f6f2" ) )
        , ( 1024, ( "#edc53f", "#f9f6f2" ) )
        , ( 2048, ( "#edc53f", "#f9f6f2" ) )
        ]


prettyPrint : Matrix (Maybe Int) -> Html Msg
prettyPrint matrix =
    matrix
        |> Matrix.indexedMap drawTile
        |> matrixToDivs


drawTile : Int -> Int -> Maybe Int -> Html Msg
drawTile x y value =
    case value of
        Nothing ->
            div
                [ style
                    (baseTileStyle
                        ++ [ ( "background", "rgba(238, 228, 218, 0.35)" ) ]
                    )
                ]
                [ text " " ]

        Just value ->
            let
                ( bgColor, fgColor ) =
                    Dict.get value valueColorMap
                        |> Maybe.withDefault ( "#ffb380", "white" )
            in
                div
                    [ style
                        (baseTileStyle
                            ++ [ ( "background-color", bgColor )
                               , ( "color", fgColor )
                               ]
                        )
                    ]
                    [ text (toString value) ]


matrixToDivs : Matrix (Html.Html Msg) -> Html.Html Msg
matrixToDivs matrix =
    let
        makeRow y =
            Matrix.getRow y matrix
                |> Maybe.map (Array.toList)
                |> Maybe.withDefault []
                |> Html.div []

        height =
            Matrix.height matrix
    in
        [0..height]
            |> List.map makeRow
            |> Html.div []


showGameWon : Model.GameState -> Html msg
showGameWon state =
    case state of
        Model.Won ->
            h2 [] [ text "You have Won" ]

        Model.Playing ->
            div [] []


view : Model -> Html.Html Msg
view model =
    div
        [ style
            [ ( "background", "#faf8ef" )
            , ( "color", "#776e65" )
            , ( "margin", "0 auto" )
            ]
        ]
        [ h1 [ style [ ( "text-align", "center" ) ] ] [ text "ELM 2048" ]
        , showGameWon model.gameState
        , div
            [ style
                [ ( "width", "450px" )
                , ( "height", "100%" )
                , ( "margin", "0 auto" )
                , ( "font-family", "\"Clear Sans\", \"Helvetica Neue\", Arial, sans-serif;" )
                , ( "font-size", "18px" )
                ]
            ]
            [ div
                [ style
                    [ ( "background-color", "#bbada0" )
                    , ( "padding", "5px" )
                    ]
                ]
                [ prettyPrint model.matrix
                ]
            ]
        ]
