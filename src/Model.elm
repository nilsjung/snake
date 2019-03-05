module Model exposing (..)

import Keyboard exposing (Key(..))

type Msg
    = KeyMsg Keyboard.Msg
    | Tick

type alias Point =
    { x : Int
    , y: Int
    }

type alias Model =
    { snake: List Point
    , food: Point
    , pressedKeys: List Key
    , nextMove: Point
    , gameOver: Bool
    }


type PointType = Snake | Food | Empty
getPointType : Model -> Point -> PointType
getPointType model point =
  if List.any (\s -> s == point) model.snake then
    Snake
  else if model.food == point then
    Food
  else
    Empty
