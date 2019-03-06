module Playground exposing (playground)
import Html exposing (Html, Attribute)

import Point exposing (..)

import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (..)

-- Module imports
import Model exposing (..)


playground : Model -> Html Msg
playground model =
     let
         viewBoxSize = "0 0 50 50"
         range = List.range 0 100
     in
        svg [viewBox viewBoxSize, Svg.Attributes.class "snake__playground"]
            (List.map (\row -> gameRow model row) range)

gameRow : Model -> Int -> Svg Msg
gameRow model row =
    let
        range = List.range 0 100
    in
        g [] (List.filterMap (\cell -> gameCell model row cell) range)

gameCell : Model -> Int -> Int -> Maybe (Svg Msg)
gameCell model cell row =
    let
        cellWidth = 2
        cellHeight = 2
        pixelX = (row * cellWidth)
        pixelY = (cell * cellHeight)
    in
        case getPointType model (Point row cell) of
            Snake ->
                Just ( rect
                    [ x (String.fromInt pixelX)
                    , y (String.fromInt pixelY)
                    , Svg.Attributes.width (String.fromInt cellWidth)
                    , Svg.Attributes.height (String.fromInt cellHeight)
                    , fill (fieldColor Snake)
                    ] []
                )
            Food ->
                Just ( rect
                    [ x (String.fromInt pixelX)
                    , y (String.fromInt pixelY)
                    , Svg.Attributes.width (String.fromInt cellWidth)
                    , Svg.Attributes.height (String.fromInt cellHeight)
                    , fill (fieldColor Food)
                    ] []
                )
            Empty -> Nothing

fieldColor: PointType -> String
fieldColor cell = case cell of
    Snake -> "#343434"
    Food -> "#ff4488"
    _ -> ""
