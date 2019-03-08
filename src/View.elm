module View exposing (..)

import Html exposing (Html, Attribute, button, a, span, div, h1, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

import Model exposing (Model, GameState(..), Msg(..))
import Playground exposing (playground)

mainGameBoard : Model -> Html Msg
mainGameBoard model =
    let
        speedLevel = 100 - model.speed
        repository = div [class "snake__github-repository"]
            [ a [class "snake__github-repository__link", href "https://github.com/nilsjung/snake"] [text "github.com/nilsjung/snake"]
            ]
        renderPerStatus = case model.gameState of
            Running ->
                div [Html.Attributes.class "snake__container"]
                    [ playground model
                    ]
            GameOver ->
                div [class "snake__container snake__container--game-over"]
                    [ div [class "snake__game-over-information"]
                        [ div [class "snake__container__game-over-message"] [text "Game Over"]
                        , div [class "snake__retry-button__container"]
                            [ button [class "snake__retry-button", onClick Restart] [text "Retry"]
                            ]
                        ]
                    ]

            GameOverWithHighScore ->
                div [class "snake__container snake__container--game-over"]
                    [ div [class "snake__game-over-information"]
                        [ div [class "snake__container__game-over-message"] [text "Game Over"]
                        , div [class "snake__container__game-over-with-highscore"]
                            [ div [class "snake__container__hightscore-message"] [text "New Highscore"]
                            , div [class "snake__container__hightscore"] [text (String.fromInt model.highScore)]
                            ]
                        , div [class "snake__retry-button__container"]
                            [ button [class "snake__retry-button", onClick Restart] [text "Retry"]]
                        ]
                    ]
    in
    div [Html.Attributes.class "snake"]
        [ div [class "snake__headline__container"]
            [ h1 [class "snake__headline"] [text "Snake Game"]
            , div [class "snake__information__container"]
                    [ span [class "snake__information"] [span [class "snake__information__label"] [text "Speed"], span [class "snake__information__value snake__information__speed-level"] [text (String.fromInt speedLevel)]]
                    , span [class "snake__information"] [span [class "snake__information__label"] [text "Score"], span [class "snake__information__value snake__information__score"] [text (String.fromInt model.actualScore)]]
                    , span [class "snake__information"] [span [class "snake__information__label"] [text "High Score"], span [class "snake__information__value snake__information__high-score"] [text (String.fromInt model.highScore)]]
                    ]
            ]
        , renderPerStatus
        , repository
        ]