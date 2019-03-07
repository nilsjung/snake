module Point exposing (..)

type alias Point =
    { x : Int
    , y: Int
    }

-- Add Two Points
add : Point -> Point -> Point
add p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y}

equal : Point -> Point -> Bool
equal p1 p2 = p1.x == p2.x && p1.y == p2.y