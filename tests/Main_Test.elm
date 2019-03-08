module Main_Test exposing (..)

import Expect exposing (Expectation)
import GamePlay
import Test exposing (..)

import Point exposing (Point)


suite : Test
suite =
    describe "Snake Tests"
        [ describe "Main isCollusion"
            [ test "returns true if the point collides with the first element of the list" <|
                \_ ->
                    let
                        list = [Point 100 200, Point 101 200]
                        point = Point 100 200
                    in
                        Expect.true "Expect the point to be in the list" (GamePlay.isCollusion list point)
            , test "returns false of another element of the list is equal" <|
                \_ ->
                    let
                        list = [Point 100 200, Point 101 200]
                        point = Point 101 200
                    in
                        Expect.false "Expect the head not to be equal" (GamePlay.isCollusion list point)
            , test "returns false if point is not part of the list" <|
                \_ ->
                    let
                        list = [Point 100 200, Point 101 200]
                        point = Point 101 201
                    in
                        Expect.false "Expect the point to be not in the list" (GamePlay.isCollusion list point)
            ]
        , describe "outOfBounds"
            [ test "returns true if element is out of bounds" <|
                \_ ->
                    let
                        element = Point 13 -3
                        size = 12
                    in
                        Expect.true "Expect the element to be out of bounds" (GamePlay.outOfBounds element size)
            , test "returns false if the element in inside the boundaries" <|
                \_ ->
                    let
                        element = Point 1 1
                        size = 12
                    in
                        Expect.false "Expect the element to be inside the boundaries" (GamePlay.outOfBounds element size)
            ]
        , describe "reverseElement"
            [ test "returns the reverse element y is too small" <|
                \_ ->
                let element = Point 13 -1
                    size = 20
                in
                    Expect.equal (Point 13 19) (GamePlay.reversePoint element size)
            , test "returns the reverse element if both are too small" <|
                \_ ->
                let
                    element = Point -1 -1
                    size = 20
                in
                  Expect.equal (Point 19 19) (GamePlay.reversePoint element size)
            , test "returns the reverse element if x is too large" <|
                \_ ->
                let
                    element = Point 21 0
                    size = 20
                in
                  Expect.equal (Point 1 0) (GamePlay.reversePoint element size)
            ]
        , describe "isGameOver"
            [ test "returns false if snake has 1 element" <|
                \_ ->
                    let
                        head = Just (Point 1 1)
                        tail = Just []
                    in
                        Expect.false "no game over" (GamePlay.isGameOver head tail)
            , test "returns false if head is not in tail" <|
                \_ ->
                    let
                        head = Just (Point 1 1 )
                        tail = Just ([Point 1 2])
                    in
                        Expect.false "no game over" (GamePlay.isGameOver head tail)
            , test "returns true if snake head is in tail" <|
                \_ ->
                    let
                        head = Just (Point 1 1)
                        tail =  Just ([Point 1 2, Point 1 3, Point 1 1])
                    in
                        Expect.true "game over" (GamePlay.isGameOver head tail)
            ]
        ]