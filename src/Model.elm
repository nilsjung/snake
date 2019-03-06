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

type alias Model =
    { snake: List Point
    , food: Point
    , pressedKeys: List Key
    , nextMove: Maybe Key
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
