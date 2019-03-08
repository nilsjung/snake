module Main exposing (..)

import Random exposing (..)
import Browser

import List

import Time

import Html exposing (Html, Attribute)


import Keyboard exposing (Key(..))
import Keyboard.Arrows

import Constants
import Model exposing (..)
import Point exposing(..)
import View exposing (..)
import Snake exposing (..)
import GamePlay exposing (..)
------- Main ------

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

------- Model -------

defaultSnake : Snake
defaultSnake = [Point 13 13]

initialModel =
    { snake = defaultSnake
    , pressedKeys = []
    , food = Point 15 15
    , nextMove = Nothing
    , gameState = Running
    , speed = 100
    , actualScore = 0
    , highScore = 0
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel
    , Random.generate GenerateFood generateRandomFood
    )

------- Update -------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyMsg keyMsg -> ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys } |> update Move)
        Move -> (getNextMoveFromKey model, Cmd.none)
        GenerateFood food -> ({model | food = food}, Cmd.none)
        Clock _ -> nextGameCycle model
        Restart -> ({initialModel | highScore = model.highScore}, Cmd.none)




nextGameCycle : Model -> (Model, Cmd Msg)
nextGameCycle model =
    let
        keyToPoint = case model.nextMove of
            Just key -> Keyboard.Arrows.arrows [key]
            Nothing -> Point 0 0
        movingSnake =
            List.foldr
              (\element newSnake
                ->  if outOfBounds element Constants.playgroundSize then
                       moveSnakeElements keyToPoint model.snake newSnake (reversePoint element Constants.playgroundSize)
                    else moveSnakeElements keyToPoint model.snake newSnake element)
              []
              model.snake
        snakeHead = List.head movingSnake
        snakeTail = List.tail movingSnake
        eatenFood = isCollusion (List.reverse model.snake) model.food
        isNewHighScore = model.highScore < model.actualScore
        calculatedScore = case List.maximum (model.actualScore::[model.highScore]) of
            Just max -> max
            Nothing -> model.highScore
    in
        if (isGameOver snakeHead snakeTail) then
            case model.nextMove of -- the game has not start yet
                Just _ ->
                    if isNewHighScore
                    then (
                        { model
                            | gameState = GameOverWithHighScore
                            , highScore = calculatedScore
                        }
                        , Cmd.none)
                    else
                        ({ model
                            | gameState = GameOver
                            , highScore = calculatedScore
                        }, Cmd.none )
                Nothing -> (model, Cmd.none)
        else if eatenFood then
            ({ model
                | snake = addElementToSnake movingSnake
                , food = Point 0 0
                , speed = model.speed - 1
                , actualScore = model.actualScore + (100 - model.speed)
            }, Random.generate GenerateFood generateRandomFood )
        else
        ({ model
            | snake = movingSnake
        }
        , Cmd.none)


------- View -------

view : Model -> Html Msg
view model = mainGameBoard model


------- Subscriptions -------
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Sub.map KeyMsg Keyboard.subscriptions
    , Time.every (toFloat model.speed) Clock
    ]

