module View exposing (view)

import Update exposing (Msg)
import Model exposing (Model)
import Html exposing (Html, div, text)
import Html.Attributes
import Matrix exposing (..)
import Array exposing (..)


-- View


prettyPrint : Matrix (Maybe Int) -> Html.Html Msg
prettyPrint matrix =
    matrix
        |> Matrix.indexedMap drawTile
        |> matrixToDivs


drawTile : Int -> Int -> Maybe Int -> Html.Html Msg
drawTile x y value =
    case value of
        Nothing ->
            Html.div
                [ Html.Attributes.style
                    [ ( "width", "40px" )
                    , ( "height", "40px" )
                    , ( "border-radius", "4px" )
                    , ( "margin", "2px" )
                    , ( "display", "inline-block" )
                    , ( "line-height", "40px" )
                    , ( "font-size", "12px" )
                    , ( "background-color", "lightgrey" )
                    ]
                ]
                []

        Just value ->
            Html.div
                [ Html.Attributes.style
                    [ ( "width", "40px" )
                    , ( "height", "40px" )
                    , ( "border-radius", "4px" )
                    , ( "margin", "2px" )
                    , ( "display", "inline-block" )
                    , ( "line-height", "40px" )
                    , ( "font-size", "12px" )
                    , ( "text-align", "center" )
                    , ( "background-color", "green" )
                    ]
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
    div []
        [ prettyPrint model.matrix
        ]
