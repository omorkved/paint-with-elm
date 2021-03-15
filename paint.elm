module Paint exposing (main)

----------------------------------------------------
-- Our notes:
-- elm make Paint.elm --output=elm.js
-- click on the "app.html" one
----------------------------------------------------
-- Notes for turning it in:

{-- Canvas started code: https://ellie-app.com/62Dy7vxsBHZa1,
    which was linked from the Canvas documentation --}

----------------------------------------------------
-- Imports

-- added by us:
import Platform
import Json.Decode as Decode
import Random
-- from original:
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onClick)
import Canvas exposing (rect, shapes, circle, Renderable, Point, Shape)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)



{-- MODEL
-- Define a rectangle to be the canvas? 
Or maybe the canvas shouldn't be in the model cuz it doesnt ever update.

-- List of paint strokes? 
Whenever they add a paint stroke, it gets added to the list. 
The entire List of paint strokes is rendered.

--}


{-- idea:
    We can have multiple settings:
 -- setting one "paint mode", onMouseDown paint wherever the cursor goes, stop on next Mouse Down.
 -- setting two "splatter mode": onMouseDown, splatter a bit of paint as like a one-time thing

--}

-- jk don't do this cuz Canvas has a built-in point type.
--type alias Point = { x : Float, y : Float }

type alias Splatter = {loc : Point, size : Float } --maybe rename size to radius

type alias Model =
    { count : Float
    , clickList : List Point -- Points of where they clicked. Head is most recent
    , splatterList : List Splatter -- like clicklist but also contains size of the splatter for each click. Head is most recent
     }


type Msg
    = Frame Float 
    | ClickedPoint Point
    | SizeSplatter Point Float

-- Add more flags here:
type alias Flags =
    Int --passing in date for now
    -- jk { windowWidth : Int }

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : Flags -> ( Model, Cmd Msg )
init flags =
    --right now, not doing anything with flags.

    -- If you add new fields to the model, put initial values here:
    ({ count = 0
    , clickList = []
    , splatterList = []}
    -- no initial commands
    , Cmd.none)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- a Frame msg makes things spin
    Frame _ ->
      ( { model | count = model.count + 1 }, Cmd.none )

    -- a ClickedPoint message gives us a new coordinate for a paint splatter
    ClickedPoint newClick ->
        -- add this Point to the clickList
      ( { model | clickList = newClick :: model.clickList }
        -- generate a size for the splatter
      , (Random.generate (SizeSplatter newClick) (Random.float 3 50)) )

    SizeSplatter loc size ->
      ({model 
      | splatterList = ({loc = loc, size = size} :: model.splatterList)
      }
      , Cmd.none
      )



{-- Helper function for subscriptions:
    This maps the coordinates of the click, which are given
    in JSON, to ints so we can bundle them into a Point
 --}
-- credit: clickDecoder taken from Quiz 1' code (presumably written by Prof Chugh)
clickDecoder : Decode.Decoder Point
clickDecoder =
  Decode.map2
    --(\x y -> { x = toFloat x, y = toFloat y })
    (\x y -> (toFloat x, toFloat y))
    (Decode.field "clientX" Decode.int)
    (Decode.field "clientY" Decode.int)


{-- point to Msg:
    Helper function for subscriptions. 
    Literally just turns a Point into a Msg so that 
    subscriptions can have the right type.
--}
pointToMsg : Point -> Msg
pointToMsg point =
  ClickedPoint point

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ onAnimationFrameDelta Frame -- render animation. This is used for the spinning box
    , onClick (Decode.map pointToMsg clickDecoder) -- React to clicks
    ]


-- constants: (from the spinning box:)
width = 400
height = 400
centerX = width / 2
centerY = height / 2


-- helper function for placeOnSplatter: literally just picks a random number


-- helper function for view:
-- the goal of this function is to place a dot (for now) where we click
-- eventually this will put shapes that looks like a paint splatter instead of a dot
placeOneSplatter : Splatter -> Shape
placeOneSplatter splat =
    circle splat.loc splat.size

-- initially call this on model.clickList
placeSplatters : List Splatter -> Renderable
placeSplatters pts =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    Canvas.shapes [] (List.map placeOneSplatter pts)



-- View:
view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ {-- uncomment this to make the spinning box appear
        Canvas.toHtml
            ( width, height )
            [ style "border" "1000px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , renderSpin model.count
            ]
        ,--} Html.div [] [Html.text ("Num clicks: " ++ String.fromInt (List.length model.clickList))]
        , Canvas.toHtml
            (width, height)
            [ style "border" "1000px solid rgba(0,0,0,0.1)" ]
            [placeSplatters model.splatterList ]
        ]



-- For the spinning box:
clearScreen =
    shapes [ fill Color.green ] [ rect ( 0, 0 ) width height ]

-- For the spinning box:
renderSpin count =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        rotation =
            degrees (count * 3)

        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
    shapes
        [ transform
            [ translate centerX centerY
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( x, y ) size size ]

