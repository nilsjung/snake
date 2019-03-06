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
        , describe "outOfBounds"
            [ test "returns true if element is out of bounds" <|
                \_ ->
                    let
                        element = Point 13 -3
                        size = 12
                    in
                        Expect.true "Expect the element to be out of bounds" (Main.outOfBounds element size)
            , test "returns false if the element in inside the boundaries" <|
                \_ ->
                    let
                        element = Point 1 1
                        size = 12
                    in
                        Expect.false "Expect the element to be inside the boundaries" (Main.outOfBounds element size)
            ]
        , describe "reverseElement"
            [ test "returns the reverse element y is too small" <|
                \_ ->
                let element = Point 13 -1
                    size = 20
                in
                    Expect.equal (Point 13 19) (Main.reversePoint element size)
            , test "returns the reverse element if both are too small" <|
                \_ ->
                let
                    element = Point -1 -1
                    size = 20
                in
                  Expect.equal (Point 19 19) (Main.reversePoint element size)
            , test "returns the reverse element if x is too large" <|
                \_ ->
                let
                    element = Point 21 0
                    size = 20
                in
                  Expect.equal (Point 1 0) (Main.reversePoint element size)
            ]
        ]