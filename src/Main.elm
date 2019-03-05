module Main exposing (..)

import Browser

import Html exposing (Html, Attribute, div, span, h1, text)
import Html.Attributes exposing (..)

import Keyboard exposing (Key(..))

import Model exposing (..)
import Playground exposing (playground)


main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
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

------- Update -------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyMsg keyMsg -> ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none)
        _ -> (model, Cmd.none)

------- View -------


view : Model -> Html Msg
view model =
    div [Html.Attributes.class "snake"]
    [ h1 [class "snake__headline"] [text "Snake Game"]
    , div [Html.Attributes.class "snake__container"]
      [ playground model
      ]
    ]

--- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

