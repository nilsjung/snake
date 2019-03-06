module Main exposing (..)

import Browser
import Browser.Events

import Constants
import Html exposing (Html, Attribute, div, h1, text)
import Html.Attributes exposing (..)

import Keyboard exposing (Key(..))
import Keyboard.Arrows

import Model exposing (..)
import Playground exposing (playground)
import Point exposing(Point)
------- Main ------

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


isCollusion : List Point -> Point -> Bool
isCollusion snake point = case List.head snake of
    Just a -> Point.equal a point
    Nothing -> False

------- Model -------

defaultSnake : List Point
defaultSnake = [Point 13 13, Point 12 13, Point 11 13, Point 10 13]

initialModel =
    { snake = defaultSnake
    , pressedKeys = []
    , food = Point 15 15
    , nextMove = Nothing
    , gameState = Running
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel
    , Cmd.none
    )

------- Update -------

isOppositeDirection : Key -> Key -> Bool
isOppositeDirection key1 key2 =
    if (key1 == ArrowRight || key2 == ArrowRight) && (key1 == ArrowLeft || key2 == ArrowLeft) then
        True
    else if (key1 == ArrowUp || key2 == ArrowUp) && (key1 == ArrowDown || key2 == ArrowDown) then
        True
    else
        False


disableOppositeDirections : Key -> Maybe Key -> Maybe Key
disableOppositeDirections nextDirection actualDirection =
    case actualDirection of
        Just key ->
            if isOppositeDirection nextDirection key then
                Just key
            else
                Just nextDirection
        Nothing -> Just nextDirection

-- reverse the key up and down functionality
getNextMoveFromKey : Model -> Model
getNextMoveFromKey model = case List.head model.pressedKeys of
    Just a -> case a of
        ArrowUp -> {model | nextMove = (disableOppositeDirections ArrowDown model.nextMove)}
        ArrowDown -> {model | nextMove = (disableOppositeDirections ArrowUp model.nextMove)}
        ArrowLeft -> {model | nextMove = (disableOppositeDirections ArrowLeft model.nextMove)}
        ArrowRight -> {model | nextMove = (disableOppositeDirections ArrowRight model.nextMove)}
        _ -> model
    Nothing -> model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyMsg keyMsg -> ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys } |> update Move)
        Move -> (getNextMoveFromKey model, Cmd.none)
        Clock _ -> nextGameCycle model



outOfBounds : Point -> Int -> Bool
outOfBounds element boundary = (element.x > boundary || element.x < 0) || (element.y > boundary || element.y < 0)

reversePoint: Point -> Int -> Point
reversePoint point max =
    let isXOut = point.x > max || point.x < 0
        isYOut = point.y > max || point.y < 0
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
        eatenFood = isCollusion model.snake model.food
        newModel =
            if eatenFood then
                {model | snake = addElementToSnake movingSnake}
            else { model | snake = movingSnake }
    in
    (newModel
    , Cmd.none)

addElementToSnake : List Point -> List Point
addElementToSnake oldSnake =
    let
        lastElement = List.head (List.reverse oldSnake)
    in
        case lastElement of
            Just a -> a::oldSnake
            Nothing -> oldSnake


moveSnakeElements : Point -> List Point -> List Point -> Point -> List Point
moveSnakeElements nextMove oldSnake newSnake snakeElement =
        if List.length newSnake ==  0 then
            [snakeElement, Point.add snakeElement nextMove]
        else if List.length newSnake == List.length oldSnake then
            newSnake
        else snakeElement::newSnake
------- View -------

view : Model -> Html Msg
view model = case model.gameState of
    Running -> div [Html.Attributes.class "snake"]
                [ h1 [class "snake__headline"] [text "Snake Game"]
                , div [Html.Attributes.class "snake__container"]
                  [ playground model
                  ]
                ]
    GameOver -> div [Html.Attributes.class "snake"]
                [ h1 [class "snake__headline"] [text "Snake Game"]
                , div [class "snake__container snake__container--game-over"]
                  [ div [class "snake__container__game-over-message"] [text "Game Over"]
                  ]
                ]


------- Subscriptions -------
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
    [ Sub.map KeyMsg Keyboard.subscriptions
    , Browser.Events.onAnimationFrame Clock
    ]

