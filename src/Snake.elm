module Snake exposing (..)

import Point exposing (..)

type alias Snake = List Point

addElementToSnake : Snake -> Snake
addElementToSnake oldSnake =
    let
        lastElement = List.head (List.reverse oldSnake)
    in
        case lastElement of
            Just a -> a::oldSnake
            Nothing -> oldSnake


moveSnakeElements : Point -> Snake -> Snake -> Point -> Snake
moveSnakeElements nextMove oldSnake newSnake snakeElement =
        if List.length newSnake ==  0 then
            [snakeElement, Point.add snakeElement nextMove]
        else if List.length newSnake == List.length oldSnake then
            newSnake
        else snakeElement::newSnake