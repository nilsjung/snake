-- some functions effecting the game play
module GamePlay exposing (..)

import Keyboard exposing (Key(..))
import Keyboard.Arrows exposing (..)
import Random
import Constants

import Model exposing (Model)
import Point exposing (Point)
import Snake exposing (Snake)


isOppositeDirection : Direction -> Direction -> Bool
isOppositeDirection key1 key2 =
    if (key1 == East || key2 == East) && (key1 == West || key2 == West) then
        True
    else if (key1 == North || key2 == North) && (key1 == South || key2 == South) then
        True
    else
        False

disableOppositeDirections : Direction -> Direction -> Direction
disableOppositeDirections nextDirection actualDirection =
    if isOppositeDirection nextDirection actualDirection then
         actualDirection
    else
        nextDirection

-- reverse the key up and down functionality
getNextMoveFromKey : Model -> Model
getNextMoveFromKey model = case List.head model.pressedKeys of
    Just a -> case a of
        ArrowUp -> {model | nextMove = (disableOppositeDirections North model.nextMove)}
        ArrowDown -> {model | nextMove = (disableOppositeDirections South model.nextMove)}
        ArrowLeft -> {model | nextMove = (disableOppositeDirections West model.nextMove)}
        ArrowRight -> {model | nextMove = (disableOppositeDirections East model.nextMove)}
        _ -> model
    Nothing -> model


generateRandomFood :  Random.Generator Point
generateRandomFood =
    let randomInt = Random.int 2 (Constants.playgroundSize - 3)
    in Random.map2 (\x y -> Point x y) randomInt randomInt


outOfBounds : Point -> Int -> Bool
outOfBounds element boundary = (element.x >= boundary || element.x < 0) || (element.y >= boundary || element.y < 0)

reversePoint: Point -> Int -> Point
reversePoint point max =
    let isXOut = point.x >= max || point.x < 0
        isYOut = point.y >= max || point.y < 0
        newX = abs ((abs point.x) - max)
        newY = abs ((abs point.y) - max)
    in
        if isXOut && isYOut then
            Point newX newY
        else if isYOut then
            Point point.x newY
        else if isXOut then
            Point newX point.y
        else
            point

isGameOver : Maybe Point -> Maybe Snake-> Bool
isGameOver head tail =
    case head of
        Just h-> case tail of
            Just t -> List.member h t
            Nothing -> False
        Nothing -> False

isCollusion : Snake -> Point -> Bool
isCollusion snake point = case List.head snake of
    Just a -> Point.equal a point
    Nothing -> False
