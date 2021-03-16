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
import Array



{-- idea:
    We can have multiple settings:
 -- setting one "paint mode", onMouseDown paint wherever the cursor goes, stop on next Mouse Down.
 -- setting two "splatter mode": onMouseDown, splatter a bit of paint as like a one-time thing
--}

-- jk don't do this cuz Canvas has a built-in point type.
--type alias Point = { x : Float, y : Float }

type alias Splatter = {loc : Point, size : Float, index : Int } --added in an index field
--maybe rename size to radius
--you can put more stuff in here


type alias Model =
    { count : Int -- Count of how many splatters
    , clickList : List Point -- Points of where they clicked. Head is most recent
    , splatterList : List Splatter -- like clicklist but also contains size of the splatter for each click. Head is most recent
    , colorList : List Color.Color -- List of colors in order of when they were generated
    --, colorList : List Color
    --, colorList or something 
    }


type Msg
    = Frame Float --from spinny box, we're not using this
    | ClickedPoint Point --handles clicks
    | SizeSplatter Point Float --handles the generator that generates the size for that splatte
    --you should add stuff here.

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
    , splatterList = []
    , colorList = [Color.red, Color.orange, Color.yellow, Color.green, Color.blue, Color.purple, Color.brown]}
    -- no initial commands
    , Cmd.none)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- a Frame msg makes things spin
    --once again, this is for rendering animation, we dont currently use it
    Frame _ ->
      ( { model | count = model.count + 1 }, Cmd.none )

    -- a ClickedPoint message gives us a new coordinate for a paint splatter
    ClickedPoint newClick ->
        -- add this Point to the clickList
      ( { model | clickList = newClick :: model.clickList, count = model.count + 1 }
        -- generate a size for the splatter
      , (Random.generate (SizeSplatter newClick) (Random.float 3 50)) )

    SizeSplatter loc size ->
      ({model 
      | splatterList = ({loc = loc, size = size, index = model.count} :: model.splatterList)
      }
      , Cmd.none
      )

    --add in stuff here based on like a color msg.
    --like when they pick a color, add to colorList or something


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

    -- add stuff here
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
placeSplatters : List Splatter -> Int -> Renderable
placeSplatters pts count =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    --let c = colors
    Canvas.shapes [fill (Color.rgba 1 2 0 1)] (List.map placeOneSplatter pts)

placeSplatter : Splatter -> Int -> Color.Color -> Renderable
placeSplatter pt i colors =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    Canvas.shapes [fill colors] [placeOneSplatter pt]



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
        ,--} {-- this wasnt appearing- im trying something else
                Html.div [] [Html.text ("Num clicks: " ++ String.fromInt (List.length model.clickList))]
            --}
            Html.div [] [Html.text ("Num clicks: " ++ String.fromInt (model.count))]
        , Canvas.toHtml
            (800, 700) --Olivia will change to window.height / 2.
            [ style "border" "1000px solid rgba(0,0,0,0.1)" ] --i haven't messed around with this line, feel free to!
            (List.map3 placeSplatter model.splatterList (List.range 1 model.count) model.colorList)
        ]



{-- Everything below was only for the spinning box example. Not actually used --}
-- For the spinning box:
clearScreen =
    shapes [ fill Color.green ] [ rect ( 0, 0 ) width height ]

-- For the spinning box:
renderSpin count =
    let
        size =  width / 3
        x = -(size / 2)
        y = -(size / 2)
        rotation = degrees (count * 3)
        hue = toFloat (count / 4 |> floor |> modBy 100) / 100
    in
    shapes
        [ transform
            [ translate centerX centerY
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( x, y ) size size ]

