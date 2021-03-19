module AllBlobs exposing (..)

import Canvas exposing (circle, lineTo, path, Shape)

type alias Radius = Float

{-- This file contains helper functions that relate to rendering
  the shapes of the splatters. 
  - special effects (rays and dripping paint)
  - blob types
--}

{-- special effects:

    rays: Instead of blobs, have the paint splatter show up as a ray coming out of
    the top left-hand corner of the page and ending where the user clicks.
--}
rays : Bool -> Float -> Float -> List Shape
rays bool x y =
  if bool then
    [path ( x, y )
        [ lineTo ( 10, 30 )
        , lineTo ( 30, 30 )
        , lineTo ( 20, 10 )
        ]
    ]
    else []


{-- To make the simulation more realistic, make it drip paint --}
dripPaint : Bool -> Float -> Float -> Radius -> Shape
dripPaint bool x y r =
  if bool then
    path ( x, y )
      [ lineTo ( x , y + (4*r) )
      , lineTo ( x + 5, y + (4*r))
      , lineTo ( x + 5 , y )
      ]
    else circle (x, y) 0 -- no drip effect


{-- Blobs 

  We played around with randomly generating the blobs. However, this resulted 
  in some rather ugly shapes. We decided it was nicer to pre-set the 
  allowable blobs. This allowed us to create 5 blob shapes that were 
  asymetrical enough to be interesting, but still nice enough to be 
  aesthetically pleasing. We made these by hand, by playing around with 
  these to produce satisfying - looking shapes.
--}

{-- A blob --}
blob1 : Float -> Float -> Radius -> List Shape
blob1 x y r = 
  [ circle (x, y) r
  , circle (x - (r * 2/3), y) (2*r/3)
  , circle (x + (r * 4/5), y + (3*r/8)) (1*r/2)
  , circle (x + (8*r/9), y - (1*r/8)) (1*r/2)
  ]


{-- A different blob --}
blob2 : Float -> Float -> Radius -> List Shape
blob2 x y r = 
  
  [ circle (x, y) r
  , circle (x + (r * 3/5), y) (r/2)
  , circle (x + (r * 1/12), y - (5*r/8)) (2*r/3)
  , circle (x - (r * 1/12), y - (7*r/8)) (2*r/3)
  , circle (x - (r * 1/2), y - (7*r/8)) (2*r/3)
  ]

{-- Another blob --}
blob3 : Float -> Float -> Radius -> List Shape
blob3 x y r = 
  [ circle (x, y) r
  , circle (x - (r * 1/12), y - (1*r/2)) (2*r/3)
  , circle (x - (r * 3/4), y + (7*r/8)) (2*r/3)
  ]

{-- A blob --}
blob4 : Float -> Float -> Radius -> List Shape
blob4 x y r = 
  [ circle (x, y) r
  , circle (x + (r/2), y - (r/3)) r
  , circle (x + (4*r/7), y + (r/3)) (6*r/7)
  ]

{-- A blob --}
blob5 : Float -> Float -> Radius -> List Shape
blob5 x y r = 
  [ circle (x, y) r
  , circle (x + (3*r/4), y) (7*r/8)
  , circle (x - (5*r/7), y + r) (r/2)
  , circle (x - (4*r/7), y + r) (r/2)
  , circle (x - (2*r/7), y + r) (r/2)
  , circle (x, y + r) (r/2)
  ]
