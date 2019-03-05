module Main_Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main


suite : Test
suite =
    describe "Snake Tests"
        [ describe "Main isCollusion"
            [ test "returns true if point collates with list" <|
                \_ ->
                    let
                        list = [{x = 100, y = 200}, {x = 101, y = 200}]
                        point = {x = 101, y = 200}
                    in
                        Expect.true "Expect the point to be in the list" (Main.isCollusion list point)
            ,
             test "returns false if point is not part of the list" <|
                \_ ->
                    let
                        list = [{x = 100, y = 200}, {x = 101, y = 200}]
                        point = {x = 103, y = 200}
                    in
                        Expect.false "Expect the point to be not in the list" (Main.isCollusion list point)
            ]
        ]