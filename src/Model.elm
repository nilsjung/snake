module Model exposing (..)

import Keyboard exposing (Key(..))
import Time

import Point exposing (..)

type Msg
    = KeyMsg Keyboard.Msg
    | Move
    | Clock Time.Posix

type PointType = Snake | Food | Empty
type GameState = Running | GameOver

type alias Point =
    { x : Int
    , y: Int
    }

type alias Model =
    { snake: List Point
    , food: Point
    , pressedKeys: List Key
    , nextMoves: List Point
    , gameState: GameState
    }

getPointType : Model -> Point -> PointType
getPointType model point =
  if List.any (\s -> s == point) model.snake then
    Snake
  else if model.food == point then
    Food
  else
    Empty


--- Some math on Point

-- Add Two Points
add : Point -> Point -> Point
add p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y}

-- Add a point to the list of Points
addPointToList : Point -> List Point -> List Point
addPointToList point list = List.map (add point) list