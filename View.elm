module View exposing (view)

import Update exposing (Msg)
import Model exposing (Model)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Matrix exposing (..)
import Array exposing (..)
import Dict exposing (..)


-- View


baseTileStyle : List ( String, String )
baseTileStyle =
    [ ( "width", "100px" )
    , ( "height", "100px" )
    , ( "border-radius", "10px" )
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
        [ ( 2, ( "#ffb380", "white" ) )
        , ( 4, ( "#ff7f2a", "white" ) )
        , ( 8, ( "#ff3e2a", "white" ) )
        , ( 16, ( "#ffad2a", "black" ) )
        , ( 32, ( "#ffc42a", "black" ) )
        , ( 64, ( "#ff834f", "black" ) )
        , ( 128, ( "#ff834f", "black" ) )
        , ( 256, ( "#ff834f", "black" ) )
        , ( 512, ( "#ff834f", "black" ) )
        , ( 1024, ( "#ff834f", "black" ) )
        , ( 2048, ( "#ff834f", "black" ) )
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
                        ++ [ ( "background-color", "lightgray" ) ]
                    )
                ]
                [ text " " ]

        Just value ->
            let (bgColor, fgColor) = 
                Dict.get value valueColorMap 
                |> Maybe.withDefault ( "#ffb380", "white" ) 
            in
            div
                [ style
                    (baseTileStyle
                        ++ [ ( "background-color", bgColor )
                        , ("color", fgColor) ]
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


view : Model -> Html.Html Msg
view model =
    div
        [ style
            [ ( "background", "#faf8ef" )
            , ( "color", "#776e65" )
            , ( "margin", "0 auto" )
            ]
        ]
        [ div
            [ style
                [ ( "width", "500px" )
                , ( "height", "100%" )
                , ( "margin", "0 auto" )
                , ( "font-family", "\"Clear Sans\", \"Helvetica Neue\", Arial, sans-serif;" )
                , ( "font-size", "18px" )
                ]
            ]
            [ div []
                [ prettyPrint model.matrix
                ]
            ]
        ]
