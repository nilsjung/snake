module Playground exposing (playground)
import Html exposing (Html, Attribute)

import Point exposing (..)

import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (..)

-- Module imports
import Model exposing (..)

import Constants

--- default playground size settings
columnHeight :  Int -> Float
columnHeight board = (toFloat Constants.boardDimension.y) / (toFloat board)

columnWidth : Int -> Float
columnWidth board = (toFloat Constants.boardDimension.x) / (toFloat board)


fieldColor: PointType -> String
fieldColor cell = case cell of
    Snake -> "#668466"
    Food -> "#846666"
    _ -> ""


--- Main View ---
playground : Model -> Html Msg
playground model =
     let
         range = List.range 0 Constants.playgroundSize
         sizeX = round ((toFloat Constants.boardDimension.x) / (toFloat Constants.playgroundSize)) * Constants.playgroundSize
         sizeY = round ((toFloat Constants.boardDimension.y) / (toFloat Constants.playgroundSize)) * Constants.playgroundSize
         viewBoxSize = "0 0 " ++ String.fromInt sizeX ++ " " ++ String.fromInt sizeY
     in
        svg [viewBox viewBoxSize, Svg.Attributes.class "snake__playground"]
            (List.map (\row -> gameRow model row) range)

gameRow : Model -> Int -> Svg Msg
gameRow model row =
    let
        range = List.range 0 Constants.playgroundSize
    in
        g [] (List.filterMap (\cell -> gameCell model row cell) range)

gameCell : Model -> Int -> Int -> Maybe (Svg Msg)
gameCell model cell row =
    let
        cellWidth = columnWidth Constants.playgroundSize
        cellHeight = columnHeight Constants.playgroundSize
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
