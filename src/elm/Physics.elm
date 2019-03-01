module Physics exposing (..)

type alias Box =
    { x1 : Int
    , x2 : Int
    , y1 : Int
    , y2 : Int
    }

boxesCollide : Box -> Box -> Bool
boxesCollide box1 box2 =
    let
        xcollide = box1.x1 < box2.x2 && box2.x1 < box1.x2
        ycollide = box1.y1 < box2.y2 && box2.y1 < box1.y2
    in
        xcollide && ycollide