module Main_Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main

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
                        Expect.true "Expect the point to be in the list" (Main.isCollusion list point)
            , test "returns false of another element of the list is equal" <|
                \_ ->
                    let
                        list = [Point 100 200, Point 101 200]
                        point = Point 101 200
                    in
                        Expect.false "Expect the head not to be equal" (Main.isCollusion list point)
            , test "returns false if point is not part of the list" <|
                \_ ->
                    let
                        list = [Point 100 200, Point 101 200]
                        point = Point 101 201
                    in
                        Expect.false "Expect the point to be not in the list" (Main.isCollusion list point)
            ]
        ]