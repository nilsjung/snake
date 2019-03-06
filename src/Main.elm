module Main exposing (..)

import Browser
import Browser.Events

import Html exposing (Html, Attribute, div, span, h1, text)
import Html.Attributes exposing (..)

import Keyboard exposing (Key(..))
import Keyboard.Arrows

import Model exposing (..)
import Playground exposing (playground)

------- Main ------

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


isCollusion : List Point -> Point -> Bool
isCollusion snake point = List.member point snake

------- Model -------

defaultSnake : List Point
defaultSnake = [{x=9, y=30}, {x=10, y=30}, {x=10, y=31}]

initialModel =
    { snake = defaultSnake
    , pressedKeys = []
    , food = {x = 0, y = 0}
    , nextMoves = []
    , gameState = Running
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel
    , Cmd.none
    )

------- Update -------

-- reverse the key up and down functionality
getNextMoveFromKey : Model -> Model
getNextMoveFromKey model = case List.head model.pressedKeys of
    Just a -> case a of
        ArrowUp -> {model | nextMoves = (Keyboard.Arrows.arrows (List.singleton ArrowDown))::model.nextMoves}
        ArrowDown -> {model | nextMoves = (Keyboard.Arrows.arrows (List.singleton ArrowUp))::model.nextMoves}
        ArrowLeft -> {model | nextMoves = (Keyboard.Arrows.arrows (List.singleton ArrowLeft))::model.nextMoves}
        ArrowRight -> {model | nextMoves = (Keyboard.Arrows.arrows (List.singleton ArrowRight))::model.nextMoves}
        arrow -> model
    Nothing -> model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyMsg keyMsg -> ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys } |> update Move)
        Move -> (getNextMoveFromKey model, Cmd.none)
        Clock _ -> nextGameCycle model


nextGameCycle : Model -> (Model, Cmd Msg)
nextGameCycle model = case List.head model.nextMoves of
    Just nextMove -> ({model | snake = addPointToList nextMove model.snake}, Cmd.none)
    Nothing -> (model, Cmd.none)

------- View -------

view : Model -> Html Msg
view model =
    div [Html.Attributes.class "snake"]
    [ h1 [class "snake__headline"] [text "Snake Game"]
    , div [Html.Attributes.class "snake__container"]
      [ playground model
      ]
    ]


------- Subscriptions -------
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Sub.map KeyMsg Keyboard.subscriptions
    , Browser.Events.onAnimationFrame Clock
    ]

