module AllBlobs exposing (..)

import Canvas exposing (circle, lineTo, path, Shape)

type alias Radius = Float

-- Blobs 
-- (We made these by hand, just by playing around with these to produce
-- satisfying - looking shapes)

blob1 : Float -> Float -> Radius -> List Shape
blob1 x y r = 
  [ circle (x, y) r
  , circle (x - (r * 2/3), y) (r/2)
  , circle (x - (r * 1/12), y - (13*r/16)) (1*r/3)
  , circle (x + (r * 3/4), y + (5*r/8)) (1*r/5)
  , circle (x + (r * 11/12), y + (3*r/4)) (1*r/3)
  ]


blob2 : Float -> Float -> Radius -> List Shape
blob2 x y r = 
  [ circle (x, y) r
  , circle (x + (r * 2/3), y) (r/2)
  , circle (x + (r * 1/12), y - (5*r/8)) (2*r/3)
  , circle (x - (r * 1/12), y - (7*r/8)) (2*r/3)
  , circle (x - (r * 3/4), y - (7*r/8)) (2*r/3)
  ]

blob3 : Float -> Float -> Radius -> List Shape
blob3 x y r = 
  [ circle (x, y) r
  , circle (x - (r * 1/12), y - (1*r/2)) (2*r/3)
  , circle (x - (r * 3/4), y + (7*r/8)) (2*r/3)
  ]

blob4 : Float -> Float -> Radius -> List Shape
blob4 x y r = 
  [ circle (x, y) r
  , circle (x + (r/2), y - (r/3)) r
  , circle (x + (4*r/7), y + (r/3)) (6*r/7)
  ]

blob5 : Float -> Float -> Radius -> List Shape
blob5 x y r = 
  [ circle (x, y) r
  , circle (x + (3*r/4), y) (7*r/8)
  , circle (x - (5*r/7), y) (r/2)
  , circle (x - (5*r/7), y + (4*r/5)) (r/3)
  , circle (x - (9*r/7), y) (r/2)
  , circle (x - (8*r/7), y + (r/3)) (r/3)
  ]


-- special effects:

-- might not use this but i made it look like rays coming out of the corner
rays : Bool -> Float -> Float -> List Shape -> List Shape
rays bool x y blob =
  if bool then
    path ( x, y )
        [ lineTo ( 10, 30 )
        , lineTo ( 30, 30 )
        , lineTo ( 20, 10 )
        ] 
        :: blob
    else blob


-- make it drip paint
dripPaint : Bool -> Float -> Float -> Radius -> List Shape -> List Shape
dripPaint bool x y r blob =
  if bool then
    List.append blob 
    [ path ( x, y )
        [ lineTo ( x , y + (4*r) )
        , lineTo ( x + 5, y + (4*r))
        , lineTo ( x + 5 , y )
        ]
    ]
    else blob
