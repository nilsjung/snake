module Playground exposing (playground)
import Html exposing (Html, Attribute)

import Point exposing (..)

import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (..)

-- Module imports
import Model exposing (..)

--- default playground size settings
size : Int
size = 70

playgroundSize : Point
playgroundSize = Point 500 500

columnHeight :  Int -> Float
columnHeight board = (toFloat playgroundSize.y) / (toFloat board)

columnWidth : Int -> Float
columnWidth board = (toFloat playgroundSize.x) / (toFloat board)


fieldColor: PointType -> String
fieldColor cell = case cell of
    Snake -> "#668466"
    Food -> "#846666"
    _ -> ""


--- Main View ---
playground : Model -> Html Msg
playground model =
     let
         range = List.range 0 size
         sizeX = round ((toFloat playgroundSize.x) / (toFloat size)) * size
         sizeY = round ((toFloat playgroundSize.y) / (toFloat size)) * size
         viewBoxSize = "0 0 " ++ String.fromInt sizeX ++ " " ++ String.fromInt sizeY
     in
        svg [viewBox viewBoxSize, Svg.Attributes.class "snake__playground"]
            (List.map (\row -> gameRow model row) range)

gameRow : Model -> Int -> Svg Msg
gameRow model row =
    let
        range = List.range 0 size
    in
        g [] (List.filterMap (\cell -> gameCell model row cell) range)

gameCell : Model -> Int -> Int -> Maybe (Svg Msg)
gameCell model cell row =
    let
        cellWidth = columnWidth size
        cellHeight = columnHeight size
        pixelX = (toFloat row * cellWidth)
        pixelY = (toFloat cell * cellHeight)
    in
        case getPointType model (Point row cell) of
            Snake ->
                Just ( rect
                    [ x (String.fromFloat pixelX)
                    , y (String.fromFloat pixelY)
                    , Svg.Attributes.width (String.fromFloat cellWidth)
                    , Svg.Attributes.height (String.fromFloat cellHeight)
                    , fill (fieldColor Snake)
                    ] []
                )
            Food ->
                Just ( rect
                    [ x (String.fromFloat pixelX)
                    , y (String.fromFloat pixelY)
                    , Svg.Attributes.width (String.fromFloat cellWidth)
                    , Svg.Attributes.height (String.fromFloat cellHeight)
                    , fill (fieldColor Food)
                    ] []
                )
            Empty -> Nothing
