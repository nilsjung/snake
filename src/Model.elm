module Model exposing (..)

import Keyboard exposing (Key(..))
import Time

import Point exposing (..)

type Msg
    = KeyMsg Keyboard.Msg
    | Move
    | GenerateFood Point
    | Clock Time.Posix
    | Restart

type PointType = Snake | Food | Empty
type GameState = Running | GameOver

type alias Model =
    { snake: List Point
    , food: Point
    , pressedKeys: List Key
    , nextMove: Maybe Key
    , gameState: GameState
    , speed: Int
    , actualScore: Int
    , highScore: Int
    }

getPointType : Model -> Point -> PointType
getPointType model point =
  if List.any (\s -> s == point) model.snake then
    Snake
  else if model.food == point then
    Food
  else
    Empty
