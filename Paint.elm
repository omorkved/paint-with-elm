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
import Browser.Dom exposing (Viewport)
-- from original:
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onClick)
import Canvas exposing (rect, shapes, circle, Renderable, Point, Shape)
import Canvas.Settings exposing (fill, Setting)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div, button, text)
import Html.Events
import Html.Attributes exposing (style)
import Array
--import Element exposing (text)
import Element
import Element.Background as Background
import Element.Input as Input
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
    }

type Msg
    = ClickedPoint Point --handles clicks
    | PickRadiusSplatter Point Radius --handles the generator for the final radius for the splatter
    | PickWhichShape Point Radius BlobID --handles the generator for the ID of the blob
    | GetWindowSize Viewport
    | Frame Float -- For animation
    | PickColor Color.Color
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
    , colorList = [Color.blue]}
    
    -- Fetch the window size
    , Task.perform GetWindowSize Browser.Dom.getViewport)


-- For the animation
growSplat : Splatter -> Splatter
growSplat splat =
    if splat.currRadius < splat.finalRadius then
      { splat | currRadius = splat.currRadius + 2 }
    else
      { splat | dripLength = splat.dripLength + (0.005 * splat.finalRadius) }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Animation: Makes radius grow in size
    Frame _ ->
      ( {model | splatterList = (List.map growSplat model.splatterList)}
      , Cmd.none )

    -- a ClickedPoint message gives us a new coordinate for a paint splatter
    ClickedPoint newClick ->
      -- Add this Point to the clickList
      let
        (width, height) = 
          case model.viewport of
              Nothing ->
                  (800, 800)
              Just viewport ->
                  (round viewport.viewport.width
                  , round viewport.viewport.height)
      in 
        if (Tuple.first newClick > toFloat (width-100)) || (Tuple.second newClick > toFloat (height-100))
        then (model, Cmd.none)
        --- sorry i really need to fix this plz ignore the fugly code
        {--let 
          color = case List.head model.colorList of
            Just c -> c
            _ -> Color.white
        in
          ( { model | clickList = newClick :: model.clickList, count = model.count + 1, colorList = addColor color model.colorList}
            -- Generate a size for the splatter
          , (Random.generate (PickRadiusSplatter newClick) (Random.float 3 50)) ) --}
        else
          ( { model | clickList = newClick :: model.clickList, count = model.count + 1}
            -- Generate a size for the splatter
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

    -- Chose a color and add to list of colors
    PickColor color -> 
      ({ model | colorList = addColor color model.colorList }, Cmd.none)

    -- Credit: Learned how to update viewport from 
    -- https://discourse.elm-lang.org/t/browser-dom-getviewport-return-value/1990/2
    GetWindowSize viewport ->
        ({ model | viewport = Just viewport }, Cmd.none)


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
    [ onClick (Decode.map pointToMsg clickDecoder) -- React to clicks
    , onAnimationFrameDelta Frame -- Animate splatters
    -- add stuff here
    ]


-- helper function for view:
-- the goal of this function is to place a dot (for now) where we click
-- eventually this will put shapes that looks like a paint splatter instead of a dot
placeOneSplatter : Splatter -> List Shape
placeOneSplatter splat =
    let
      x = Tuple.first splat.loc
      y = Tuple.second splat.loc
      -- Note we use the current radius (not the final radius)
      -- so that the animation works
      r = splat.currRadius
    in
    -- Turn Paint dripping off Here
    dripPaint False x y splat.dripLength <|

    rays False x y <|
    case splat.blobID of
    -- Probably can refactor this matching to make it nicer
    -- I placed these functions in a new file (AllBlobs.elm)
      1 -> blob1 x y r
      2 -> blob2 x y r
      3 -> blob3 x y r
      4 -> blob4 x y r
      _ -> blob5 x y r
      
    
        
-- initially call this on model.clickList
--placeSplatters : List Splatter -> Int -> Renderable
--placeSplatters pts count =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    --let c = colors
    --Canvas.shapes [fill (Color.rgba 1 2 0 1)] (List.map placeOneSplatter pts)

placeSplatter : Splatter -> Int -> Color.Color -> Renderable
placeSplatter pt i colors =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    Canvas.shapes [fill colors] (placeOneSplatter pt)

-- add a new color to colorList and modify the rest of the colorlist
addColor : Color.Color -> List Color.Color -> List Color.Color
addColor c colors = 
  case colors of
    [] -> c :: colors
    _ -> c :: List.map2 mixColors (List.indexedMap Tuple.pair colors) (List.repeat (List.length colors) c)

-- mix two colors with a given factor
mixColors : (Int, Color.Color) -> Color.Color -> Color.Color
mixColors (index, oColor) mixColor =
  let c1 = Color.toRgba oColor
      c2 = Color.toRgba mixColor
      i = (toFloat index) + 1
      j = i - 1
  in 
    let r = ((j * c1.red) + c2.red)/i
        g = ((j*c1.green) + c2.green)/i
        b = ((j*c1.blue) + c2.blue)/i
    in 
      Color.fromRgba {red = r, green = g, blue = b, alpha = 1}

  




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
        --, style "justify-content" "center"
        , style "align-items" "center"
        ]
        [  {-- this wasnt appearing- im trying something else
                Html.div [] [Html.text ("Num clicks: " ++ String.fromInt (List.length model.clickList))]
            --}
            Html.div [] [Html.text ("Num clicks: " ++ String.fromInt (model.count))]
        , Canvas.toHtml
            (width, height)
            [ style "border" (String.fromInt 50 ++ "px solid rgba(0,0,0,0.1)")] --i haven't messed around with this line, feel free to!
            (List.map3 placeSplatter model.splatterList (List.range 1 model.count) model.colorList)
          , div 
              [style "border" "1px solid rgba(1,1,0,0.1)"] 
          [ button [Html.Events.onClick (PickColor Color.red)] [ text "Red" ]
          , button [Html.Events.onClick (PickColor Color.orange)] [ text "Orange" ]
          , button [Html.Events.onClick (PickColor Color.yellow)] [ text "Yellow" ]
          , button [Html.Events.onClick (PickColor Color.green)] [ text "Green" ]
          , button [Html.Events.onClick (PickColor Color.blue)] [ text "Blue" ]
          , button [Html.Events.onClick (PickColor Color.purple)] [ text "Purple" ]
          ]]
