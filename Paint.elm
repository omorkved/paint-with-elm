module Paint exposing (main)

----------------------------------------------------
-- Our notes: (delete before turning in)
-- elm make Paint.elm --output=Paint.js
----------------------------------------------------
-- Citations: (Include in project write-up and in code)

{-- Loosely referenced elm-canvas started code: https://ellie-app.com/62Dy7vxsBHZa1,
    which was linked from the Canvas documentation --}

{-- Learned how to update viewport from 
    https://discourse.elm-lang.org/t/browser-dom-getviewport-return-value/1990/2 --}

----------------------------------------------------
-- Imports

-- our file
import AllBlobs exposing (..)

-- added by us:
import Platform
import Task
import Json.Decode as Decode
import Random
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown)
import Canvas exposing (rect, shapes, circle, Renderable, Point, Shape)
import Canvas.Settings exposing (fill, Setting)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div, button, text)
import Html.Events
import Html.Attributes exposing (style)
import Array
---------------------------------------------------------
-- Types

-- jk don't do this cuz Canvas has a built-in point type.
--type alias Point = { x : Float, y : Float }

type alias BlobID = Int
type alias Radius = Float

type alias Splatter = 
    { loc : Point
    , finalRadius : Float -- Once animation stops
    , currRadius : Float -- For animation
    , blobID : BlobID
    , dripLength : Float -- For animation
    , index : Int }
--you can put more stuff in here

type alias Model =
    { count : Int -- Count of how many splatters
    , viewport : Maybe Viewport -- for storing width and height of screen
    , clickList : List Point -- Where they clicked. Head is most recent
    , splatterList : List Splatter -- like clickList but also contains size of the splatter for each click. Head is most recent
    , colorList : List Color.Color -- List of colors in order of when they were generated
    , isDripping : Bool
    }

type Msg
    = ClickedPoint Point --handles clicks.
    | PickRadiusSplatter Point Radius --handles the generator for the final radius for the splatter
    | PickWhichShape Point Radius BlobID --handles the generator for the ID of the blob
    | GetWindowSize Viewport
    | Frame Float -- For animation
    | ToggleDrip
    --you should add stuff here.

---------------------------------------------------------
-- Flags

-- Add more flags here:
type alias Flags =
    Int --passing in date for now
    -- jk { windowWidth : Int }
---------------------------------------------------------
-- Code

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

    ({ count = 0
    , viewport = Nothing
    , clickList = []
    , splatterList = []
    , colorList = [Color.red, Color.orange, Color.yellow, Color.green, Color.blue, Color.purple, Color.brown]
    , isDripping = True
    }
    -- Fetch the window size
    , Task.perform GetWindowSize Browser.Dom.getViewport)


-- For the animation
growSplat : Model -> Splatter -> Splatter
growSplat model splat =
    if splat.currRadius < splat.finalRadius then
      { splat | currRadius = splat.currRadius + 2 }
    else
      if model.isDripping then
        { splat | dripLength = splat.dripLength + (0.005 * splat.finalRadius) }
      else 
        splat

---------------------------------------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Animation: Makes radius grow in size or make paint drip lengthen
    Frame _ ->
      ( {model | splatterList = (List.map (growSplat model) model.splatterList)}
      , Cmd.none )

    -- ToggleDrip turns "paint drip" effect on or off
    ToggleDrip ->
      ( {model | isDripping = not model.isDripping}
      , Cmd.none)


    -- a ClickedPoint message gives us a potential coordinate for a new paint splatter
    ClickedPoint newClick ->
      -- Did they click into the actual canvas, or on another part of the screen:
      let
        x = Tuple.first newClick |> round
        y = Tuple.second newClick |> round
        (xScreen, yScreen) = 
          case model.viewport of
            Just viewport -> (round viewport.viewport.width, round viewport.viewport.height)
            Nothing -> (800, 800)
      in
      if (x > canvasWidth xScreen) || (y > canvasHeight yScreen) then
        -- They clicked outside of the paint canvas. Ignore
        (model, Cmd.none)
      else
        -- Add this Point to the clickList
        ( { model | clickList = newClick :: model.clickList, count = model.count + 1 }
        -- Generate a random size for the splatter
        , (Random.generate (PickRadiusSplatter newClick) (Random.float 3 50)) )


    -- Pick a random size for the splatter
    PickRadiusSplatter loc finalRadius ->
      -- Dont update the model just yet! We want another cmd to generate a random blobID first.
      ( model
      , Random.generate (PickWhichShape loc finalRadius) (Random.int 1 5)
      )


    -- Just picked a random blobID. Pack it all into a new Splatter in our model
    PickWhichShape loc finalRadius blobID ->
      ({model 
      | splatterList = 
        ({loc = loc
        , finalRadius = finalRadius
        -- Radius begins small (for animation)
        , currRadius = 5
        , blobID = blobID
        , dripLength = 0
        , index = model.count} 
        :: model.splatterList)
      }
      , Cmd.none
      )


    -- Credit: Learned how to update viewport from 
    -- https://discourse.elm-lang.org/t/browser-dom-getviewport-return-value/1990/2
    GetWindowSize viewport ->
        ({ model | viewport = Just viewport }, Cmd.none)

-- End update function
---------------------------------------------------------


{-- Helper function for subscriptions:
    This maps the coordinates of the click, which are given
    in JSON, to ints so we can bundle them into a Point
    
    credit: clickDecoder taken from Quiz 1' code (presumably written by Prof Chugh) 
--}
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
    [ Browser.Events.onClick (Decode.map pointToMsg clickDecoder) -- React to clicks
    , onAnimationFrameDelta Frame -- Animate splatters
    -- add stuff here
    ]

-- End subscription and subscription helper fcns

---------------------------------------------------------
-- helper functions for view:

placeOneSplatter : Model -> Splatter -> List Shape
placeOneSplatter model splat =
    let
      x = Tuple.first splat.loc
      y = Tuple.second splat.loc
      -- Note we use the current radius (not the final radius)
      -- so that the animation works
      r = splat.currRadius
    in
    -- Turn Paint dripping off here by toggling model.isDripping
    dripPaint model.isDripping x y splat.dripLength <|

    rays False x y <|
    case splat.blobID of
    -- Probably can refactor this matching to make it nicer
    -- I placed these functions in a new file (AllBlobs.elm)
      1 -> blob1 x y r
      2 -> blob2 x y r
      3 -> blob3 x y r
      4 -> blob4 x y r
      _ -> blob5 x y r
      

placeSplatter : Model -> Splatter -> Int -> Color.Color -> Renderable
placeSplatter model pt i colors =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    Canvas.shapes [fill colors] (placeOneSplatter model pt)


-- Feel free to play around with these wrappers:
canvasWidth width = 3 * width // 4
canvasHeight height = 7 * height // 8

-- View:
view : Model -> Html Msg
view model =
    let
      (width, height) = 
        case model.viewport of
            Nothing ->
                (800, 800)
            Just viewport ->
                (round viewport.viewport.width
                , round viewport.viewport.height)
    in
    div
        [ style "display" "flex"
        , style "justify-content" "flex-start" -- Keep this as flex-start (left-aligned fixes the click offset issue)
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            (canvasWidth width, canvasHeight height)
            [ style "border" "10px solid rgba(0,0,0,0.1)"] --i haven't messed around with this line, feel free to!
            (List.map3 (placeSplatter model) model.splatterList (List.range 1 model.count) model.colorList)
        , button [Html.Events.onClick ToggleDrip] [ text "Toggle drip" ]
        ]
