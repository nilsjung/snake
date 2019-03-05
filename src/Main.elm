module Main exposing (..)

import Browser

import Html exposing (Html, Attribute, div, span, h1, text)
import Html.Attributes exposing (..)

import Keyboard exposing (Key(..))
import Keyboard.Arrows

import Svg exposing (Svg, svg, rect, g, circle)
import Svg.Attributes exposing (..)


main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


------- Model -------

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

isCollusion : List Point -> Point -> Bool
isCollusion snake point = List.member point snake

defaultSnake : List Point
defaultSnake = [{x=9, y=30}, {x=10, y=30}, {x=10, y=31}, {x=11, y=31}]

initialModel =
    { snake = defaultSnake
    , pressedKeys = []
    , food = {x = 0, y = 0}
    , nextMove = {x=1, y=0}
    , gameOver = False
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel
    , Cmd.none
    )

type PointType = Snake | Food | Empty
getPointType : Model -> Point -> PointType
getPointType model point =
  if List.any (\s -> s == point) model.snake then
    Snake
  else if model.food == point then
    Food
  else
    Empty


------- Update -------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyMsg keyMsg -> ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none)
        _ -> (model, Cmd.none)

------- View -------
gameContainer : Model -> Html Msg
gameContainer model =
     let
         viewBoxSize = "0 0 300 300"
         range = List.range 0 100
     in
        svg [viewBox viewBoxSize, Svg.Attributes.class "snake__playground"]
            (List.map (\row -> gameRow model row) range)

gameRow : Model -> Int -> Svg Msg
gameRow model row =
    let
        range = List.range 0 100
    in
        g [] (List.filterMap (\cell -> gameCell model row cell) range)

gameCell : Model -> Int -> Int -> Maybe (Svg Msg)
gameCell model cell row =
    let
        cellWidth = 3
        cellHeight = 3
        pixelX = (row * cellWidth)
        pixelY = (cell * cellHeight)
    in
        case getPointType model (Point row cell) of
            Snake ->
                Just ( rect
                    [ x (String.fromInt pixelX)
                    , y (String.fromInt pixelY)
                    , Svg.Attributes.width (String.fromInt cellWidth)
                    , Svg.Attributes.height (String.fromInt cellHeight)
                    , fill (fieldColor Snake)
                    ] []
                )
            Food ->
                Just ( rect
                    [ x (String.fromInt pixelX)
                    , y (String.fromInt pixelY)
                    , Svg.Attributes.width (String.fromInt cellWidth)
                    , Svg.Attributes.height (String.fromInt cellHeight)
                    , fill (fieldColor Food)
                    ] []
                )
            Empty -> Nothing

fieldColor: PointType -> String
fieldColor cell = case cell of
    Snake -> "#343434"
    Food -> "#ff4488"
    _ -> ""


view : Model -> Html Msg
view model =
    div [Html.Attributes.class "snake"]
    [ h1 [] [text "Snake Game"]
    , div [Html.Attributes.class "snake__container"]
      [ div []
          [ span [] [text "Snake"]]
      , gameContainer model
      ]
    ]

printPoint : Point -> String -> Html Msg
printPoint point color =
    div [] [text (String.fromInt point.x), text (String.fromInt point.y)]

--- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

