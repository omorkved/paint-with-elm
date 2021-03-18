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
import Canvas.Settings.Advanced exposing (rotate, transform
  , translate, GlobalCompositeOperationMode(..), compositeOperationMode)
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

-- -- Set page title? --- haha nope
-- port title : String
-- port title = "The page title"

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
    , isRotating : Bool
    , degreesRotate : Float
    , plainCircles : Bool
    , raysInsteadOfBlobs : Bool
    }

type Msg
    = ClickedPoint Point --handles clicks.
    | PickRadiusSplatter Point Radius --handles the generator for the final radius for the splatter
    | PickWhichShape Point Radius BlobID --handles the generator for the ID of the blob
    | GetWindowSize Viewport
    | Frame Float -- For animation
    | PickColor Color.Color
    | ToggleDrip
    | ToggleRotate
    | TogglePlainCircles
    | ToggleRays
    | ClearScreen
    --you should add stuff here.

---------------------------------------------------------
-- Code

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg )
init () =
    --right now, not doing anything with flags.

    ({ count = 0
    , viewport = Nothing
    , clickList = []
    , splatterList = []
    , colorList = []
    , isDripping = False
    , isRotating = False
    , degreesRotate = 0
    , plainCircles = False
    , raysInsteadOfBlobs = False
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

-- Call this function when the user wants to clear the scren
clearShapes : Model -> Model
clearShapes model =
    { count = 0
    , viewport = model.viewport
    -- dump the shapes
    , clickList = []
    , splatterList = []
    -- preserve the most recently picked color
    , colorList = 
      case List.head model.colorList of
        Just color -> [color]
        Nothing -> []

    -- preserve the current settings
    , isDripping = model.isDripping
    , isRotating = model.isRotating
    , degreesRotate = model.degreesRotate
    , plainCircles = model.plainCircles
    , raysInsteadOfBlobs = model.raysInsteadOfBlobs
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Animation: Makes radius grow in size or make paint drip lengthen
    Frame _ ->
      ( { count = model.count, viewport = model.viewport
      , clickList = model.clickList, raysInsteadOfBlobs = model.raysInsteadOfBlobs
      , colorList = model.colorList, isDripping = model.isDripping
      , isRotating = model.isRotating, plainCircles = model.plainCircles

        -- These two things change:
      , degreesRotate = model.degreesRotate + 1
      ,  splatterList = (List.map (growSplat model) model.splatterList)}
      , Cmd.none )

    -- ToggleDrip turns "paint drip" effect on or off
    ToggleDrip ->
      ( {model | isDripping = not model.isDripping}
      , Cmd.none)

    ToggleRotate ->
      let 
        newModel = clearShapes model
      in
      ( {newModel | isRotating = not model.isRotating}
      , Cmd.none)

    
    TogglePlainCircles ->
      ( { count = model.count, viewport = model.viewport
      , clickList = model.clickList, splatterList = model.splatterList
      , colorList = model.colorList, isDripping = model.isDripping
      , isRotating = model.isRotating, degreesRotate = model.degreesRotate
      -- These two change:
      , plainCircles = not model.plainCircles
      , raysInsteadOfBlobs = False
      }, Cmd.none)

    -- ToggleRays switches whether we are painting with rays or not.
    -- Always clear the screen when we switch between rays and blobs, to make it look nicer
    ToggleRays ->
      let
        newModel = clearShapes model
      in
      ({newModel | raysInsteadOfBlobs = not model.raysInsteadOfBlobs}, Cmd.none)

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
        , (Random.generate (PickRadiusSplatter newClick) (Random.float 30 50)) )


    -- Pick a random size for the splatter
    PickRadiusSplatter loc finalRadius ->
      -- Dont update the model just yet! We want another cmd to generate a random blobID first.
      ( model
      , Random.generate (PickWhichShape loc finalRadius) (Random.int 1 1)
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
      
    ClearScreen ->
        (clearShapes model, Cmd.none)
        --init ()

    -- Credit: Learned how to update viewport from 
    -- https://discourse.elm-lang.org/t/browser-dom-getviewport-return-value/1990/2
    GetWindowSize viewport ->
        ({ model | viewport = Just viewport }, Cmd.none)

-- End update function
---------------------------------------------------------
-- Subscriptions (and relevant helper functions)

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
    -- React to clicks into the canvas
    [ Browser.Events.onClick (Decode.map pointToMsg clickDecoder) 

    -- Allows for animation (of splatter size and of drips)
    , onAnimationFrameDelta Frame
    ]

-- End subscription and subscription helper fcns

---------------------------------------------------------
-- helper functions for view:

maybeRenderSpin : Model -> Setting
maybeRenderSpin model =
    let
        rotation = 
          if model.isRotating then degrees (model.degreesRotate * 3)
          else degrees 0
        
    in
    transform [rotate rotation]


--place one splatter produces the visualization that corresponds
-- to one specific click.
-- This vis may be: a circle, a blob, a ray, and with or without paint dripping
placeOneSplatter : Model -> Splatter -> List Shape
placeOneSplatter model splat =
    let
      x = Tuple.first splat.loc
      y = Tuple.second splat.loc
      -- Note we use the current radius (not the final radius)
      -- so that the animation works
      r = splat.currRadius
      drip = (dripPaint model.isDripping x y splat.dripLength)

    in
    if model.raysInsteadOfBlobs then
      -- Plots rays instead of blobs
      rays True x y
    else
      -- Optional "paint dripping" effect
      drip ::
      if model.plainCircles then
        -- Plot boring plain circles instead of blobs
        [circle (x, y) r]
      else
        --Plot fun blobs

        case splat.blobID of
          1 -> blob1 x y r
          2 -> blob2 x y r
          3 -> blob3 x y r
          4 -> blob4 x y r
          _ -> blob5 x y r
      

placeSplatter : Model -> Splatter -> Int -> Color.Color -> Renderable
placeSplatter model pt i colors =
    -- The (map placeOneSplatter pts) call is: List Point -> List Shape
    -- so this allows us to use the built-in shapes function
    Canvas.shapes [fill colors, (maybeRenderSpin model)--, compositeOperationMode Saturation
    ] (placeOneSplatter model pt)

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
      i = (toFloat index) + 2
      j = i - 1
  in 
    let r = ((j * c1.red) + c2.red)/i
        g = ((j*c1.green) + c2.green)/i
        b = ((j*c1.blue) + c2.blue)/i
    in 
      Color.fromRgba {red = r, green = g, blue = b, alpha = 1}

  



-- Wrappers for width and height
canvasWidth width = 3 * width // 4
canvasHeight height = 7 * height // 8


-- Code taken from https://elm-lang.org/examples/image-previews
viewPreview : String -> Html msg
viewPreview url =
  div
    [ style "display" "flex"
    , style "justify-content" "flex-start" 
    , style "width" "700px"
    , style "height" "60px"
    , style "background-image" ("url('" ++ url ++ "')")
    , style "background-position" "left"
    , style "background-repeat" "no-repeat"
    , style "background-size" "contain"
    ]
    []

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
      basecolor = 
        case List.head model.colorList of 
          Just color -> Color.toRgba color
          _ -> Color.toRgba Color.white
      basecolor2 = if basecolor == (Color.toRgba Color.white)
                    then Color.toRgba Color.black
                    else basecolor
      r = style "backgroundColor" "rgb(204, 0, 0)"
      o = style "backgroundColor" "rgb(245, 121, 0)"
      y = style "backgroundColor" "rgb(237, 212, 0)"
      g = style "backgroundColor" "rgb(115, 210,  22)"
      b = style "backgroundColor" "rgb(52, 101, 164)"
      p = style "backgroundColor" "rgb(117,  80, 123)"
      textcolor = style "color" "white"
      noborder = style "border" "0px solid rgba(0,250,200,1)"
      w = style "width" "200px"
      h = style "height" "50px"--"80px"
      h2 = style "height" "38px"
      fontsize = style "font" "Comic sans MS"
      otherbackground = style "backgroundColor" "rgba(0, 0, 0, 0)"

      -- Produce our shapes. Or, if the list is emptied, clear the canvas.
      ourShapes = 
        if (List.isEmpty model.splatterList) then 
          [Canvas.clear (0, 0) (toFloat (canvasWidth width)) (toFloat (canvasHeight height))]
        else 
          (List.map3 (placeSplatter model) model.splatterList (List.range 1 model.count) model.colorList)

    in
    let othercolor = style "color" ("rgba(" ++ (String.fromFloat (basecolor2.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor2.green)) ++ ", "++ (String.fromFloat (250 * basecolor2.blue)) ++ ", " ++ (String.fromFloat 1) ++ ")")
    in
    div
        [ style "display" "flex"
        , style "justify-content" "flex-start" -- Keep this as flex-start (left-aligned fixes the click offset issue)
        , style "align-items" "center"
        ]
        [  {-- this wasnt appearing- im trying something else
                Html.div [] [Html.text ("Num clicks: " ++ String.fromInt (List.length model.clickList))]
            --}
        Canvas.toHtml
            (canvasWidth width, canvasHeight height)
            [ style "border" ("15px solid rgba(" ++ (String.fromFloat (basecolor.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor.green)) ++ ", "++ (String.fromFloat (250 * basecolor.blue)) ++ ", " ++ (String.fromFloat 0.5) ++ ")")] --i haven't messed around with this line, feel free to!
            ourShapes
        , div 
              [style "border" ("15px solid rgba(" ++ (String.fromFloat (basecolor.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor.green)) ++ ", "++ (String.fromFloat (250 * basecolor.blue)) ++ ", " ++ (String.fromFloat 0.5) ++ ")")
              , style "backgroundColor" ("rgba(" ++ (String.fromFloat (basecolor.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor.green)) ++ ", "++ (String.fromFloat (250 * basecolor.blue)) ++ ", " ++ (String.fromFloat 0.2) ++ ")")
              , style "width" "200px"
              , style "height" (String.fromInt (height - 95) ++ "px")
              ]
          [ viewPreview "https://davinstudios.com/sitebuilder/images/Original_Splash_With_Drips_10-31-16-642x209.png" 
          , button [fontsize, noborder, h, w, textcolor, r, Html.Events.onClick (PickColor Color.red)] [ text "Red" ]
          , button [noborder, h, w, textcolor,o, Html.Events.onClick (PickColor Color.orange)] [ text "Orange" ]
          , button [noborder, h, w, textcolor,y, Html.Events.onClick (PickColor Color.yellow)] [ text "Yellow" ]
          , button [noborder, h, w, textcolor,g, Html.Events.onClick (PickColor Color.green)] [ text "Green" ]
          , button [noborder, h, w, textcolor,b, Html.Events.onClick (PickColor Color.blue)] [ text "Blue" ]
          , button [noborder, h, w, textcolor,p, Html.Events.onClick (PickColor Color.purple)] [ text "Purple" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick ToggleDrip] [ text "Toggle drip" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick ToggleRays] [ text "Rays" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick TogglePlainCircles] [ text "Boring circles" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick ToggleRotate] [ text "Arcs" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick ClearScreen] [ text "Clear screen" ]
          ]
        ]
