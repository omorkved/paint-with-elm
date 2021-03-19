module Paint exposing (main)

----------------------------------------------------
{-- Citations: (Include in project write-up and in code)

- Loosely referenced elm-canvas started code: https://ellie-app.com/62Dy7vxsBHZa1,
  which was linked from the Canvas documentation
- Learned how to update viewport from 
  https://discourse.elm-lang.org/t/browser-dom-getviewport-return-value/1990/2 --}
-- referenced small amount of image preview code from https://elm-lang.org/examples/image-previews
----------------------------------------------------
-- Imports

-- Our files
{-- Contains low level details for generate blob shapes --}
import AllBlobs exposing (..)

{-- Based off of hw7. We also added many new functions to the Deque module --}
import Deque exposing (..)

-- External
import Platform
import Task
import Json.Decode as Decode
import Random
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown)
import Canvas exposing (rect, shapes, circle, Renderable, Point, Shape)
import Canvas.Settings exposing (fill, stroke, Setting)
import Color
import Html exposing (Html, div, button, text)
import Html.Events
import Html.Attributes exposing (style)
---------------------------------------------------------
-- Types

{-- We pre-set the blob shapes (reference our report for more info)
 -- so, use BlobID as an identifier for which blob shape to use. --}
type alias BlobID = Int

type alias Radius = Float

{-- Splatter contains all info needed to render an individual click, except for color
    Color is determined by mixing the relevant color entries in the colorList --}
type alias Splatter = 
    { loc : Point
    , finalRadius : Float -- Once animation stops
    , currRadius : Float -- For animation
    , blobID : BlobID
    , dripLength : Float -- For animation
    , index : Int }

type alias Model =
    { count : Int -- Count of how many splatters
    , viewport : Maybe Viewport -- For storing width and height of screen
    , clickList : Deque Point -- Where they clicked. Back is most recent
    , splatterList : Deque Splatter -- Back is most recent
    -- List of color button presses. Back is most recent
    , colorList : Deque Color.Color

    -- Booleans to keep track of which shape to draw
    , plainCircles : Bool
    , raysInsteadOfBlobs : Bool
    , explode : Bool
    , toRerender : Maybe Splatter

    -- Booleans used for rendering purposes
    , isDripping : Bool
    , isActuallyDripping : Bool

    }

type Msg
    = ClickedPoint Point 

    -- PickRadiusSplatter handles the generator for the final radius for the splatter
    | PickRadiusSplatter Point Radius

    --PickWhichShapes handles the generator for the ID of the blob
    | PickWhichShape Point Radius BlobID

    -- Handles rendering details:
    | GetWindowSize Viewport
    | Frame Float -- For animation

    -- Handle the color buttons:
    | PickColor Color.Color

    -- Handle the feature buttons:
    | TogglePlainCircles
    | ToggleRays
    | Explode Int Int
    | EraseNewest
    | EraseOldest
    | ClearScreen

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
    ({ count = 0
    , viewport = Nothing
    , clickList = Deque.empty
    , splatterList = Deque.empty
    , colorList = Deque.empty
    , isDripping = True
    , isActuallyDripping = False
    , plainCircles = False
    , raysInsteadOfBlobs = False
    , explode = False
    , toRerender = Nothing
    }
    -- Fetch the window size
    , Task.perform GetWindowSize Browser.Dom.getViewport)


---------------------------------------------------------

{-- growSplat:
    For animation details
    If Splatter is new, grows the splatter to finalRadius size
      to create a smoother feel.

    If splatter has been around for awhile, create a "paint drip"
      effect to simulate real paint.
--}
growSplat : Model -> Splatter -> Splatter
growSplat model splat =
    if splat.currRadius < splat.finalRadius then
      { splat | currRadius = splat.currRadius + 2 }
    else
      if model.isDripping then
        { splat | dripLength = splat.dripLength + (0.005 * splat.finalRadius) }
      else 
        splat


-- Call this function when the user wants to clear the scren
clearShapes : Model -> Model
clearShapes model =
    { count = 0
    , viewport = model.viewport

    -- Dump the shapes
    , clickList = Deque.empty
    , splatterList = Deque.empty
    , explode = False
    , toRerender = Nothing

    -- Preserve the most recently picked color
    , colorList = 
      case Deque.peekBack model.colorList of
        Just color -> addBack color Deque.empty --single item in Deque
        Nothing -> Deque.empty

    -- Preserve the current settings
    , isDripping = model.isDripping
    , isActuallyDripping = model.isActuallyDripping
    , plainCircles = model.plainCircles
    , raysInsteadOfBlobs = model.raysInsteadOfBlobs
    }

-- update:
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    -- Grab screen details for easier reference
    (xScreen, yScreen) = 
      case model.viewport of
        Just viewport -> (round viewport.viewport.width, round viewport.viewport.height)
        Nothing -> (800, 800)
  in
  case msg of

    -- Frame: For animation. Makes radius grow in size or make paint drip lengthen
    Frame _ ->
      ( { model |
      splatterList = (Deque.mapToDeque (growSplat model) model.splatterList)
      , toRerender = Nothing
      , isDripping = (not model.isDripping)
      , isActuallyDripping = model.isDripping && model.isActuallyDripping
      }

      -- Check if we are in Explode mode
      , if model.explode then
        -- If so, generate a phantom click 
       Random.generate 
       ClickedPoint 
       (Random.pair (Random.float 0 (canvasWidth xScreen |> toFloat))
       (Random.float 0 (canvasHeight yScreen |> toFloat)))
      else Cmd.none 
      )

    -- TogglePlainCircles: switches whether we are painting with circles or blobs
    TogglePlainCircles ->
      let
        -- Clear the screen:
        newModel = clearShapes model
      in
      ( { model | plainCircles = not newModel.plainCircles
      , raysInsteadOfBlobs = False
      }, Cmd.none)

    -- ToggleRays: switches whether we are painting with rays shapes or not.
    ToggleRays ->
      let
         -- Always clears the screen when we switch between rays and blobs,
         -- to make it look nicer.
        newModel = clearShapes model
      in
      ({newModel | raysInsteadOfBlobs = not model.raysInsteadOfBlobs}, Cmd.none)

    -- Explode: a fun feature
    Explode width height ->
      ({model | explode = True}
      , Cmd.none)
      
    -- ClickedPoint: gives us a coordinate for a new paint splatter
    ClickedPoint newClick ->
      -- Did they click into the actual canvas, or on another part of the screen:
      let
        x = Tuple.first newClick |> round
        y = Tuple.second newClick |> round
      in
      if (x > canvasWidth xScreen) || (y > canvasHeight yScreen) then
        -- They clicked outside of the paint canvas. Ignore
        (model, Cmd.none)
      else
        -- Add this Point to the clickList
        ( { model | clickList = (Deque.addBack newClick model.clickList), count = model.count + 1 }
        -- Generate a random size for the splatter
        , (Random.generate (PickRadiusSplatter newClick) (Random.float 30 50)) )

    -- PickRadiusSplatter: Handles the newly-picked random size for a new splatter
    PickRadiusSplatter loc finalRadius ->
      -- Dont update the model just yet! We want another cmd to generate a random blobID first.
      ( model
      , Random.generate (PickWhichShape loc finalRadius) (Random.int 1 5)
      )

    {-- PickWhichShape: Handles a newly-picked blobID. 
        Packs it all into a new Splatter in our model --}
    PickWhichShape loc finalRadius blobID ->
      ({model 
      | splatterList = 
        Deque.addBack
        {loc = loc, finalRadius = finalRadius
        -- Radius begins small (for animation)
        , currRadius = 5
        , blobID = blobID, dripLength = 0
        , index = model.count} 
        model.splatterList
      }
      , Cmd.none
      )

    -- PickColor: When user choses a color, add to list of colors
    PickColor color -> 
      ({ model | colorList = addColor color model.colorList}, Cmd.none)
      
    -- ClearScreen: clears the screen
    ClearScreen ->
        (clearShapes model, Cmd.none)
        --init ()

    {-- Erase commands: 
      Take advantage of the power of deques. 
      User can erase one splatter, either the
      oldest or the newest created one. --}
    EraseNewest ->
      ({ model | toRerender = Deque.peekBack model.splatterList 
      , splatterList = 
        case Deque.removeBack model.splatterList of
          Just deq -> deq
          Nothing -> Deque.empty
      }
      , Cmd.none)

    EraseOldest ->
      ({ model | toRerender = Deque.peekFront model.splatterList
      , splatterList = 
        case Deque.removeFront model.splatterList of
          Just deq -> deq
          Nothing -> Deque.empty
      }    
      , Cmd.none)

    -- GetWindowSize: Credit: Learned how to update viewport from 
    -- https://discourse.elm-lang.org/t/browser-dom-getviewport-return-value/1990/2
    GetWindowSize viewport ->
        ({ model | viewport = Just viewport }, Cmd.none)


-- End update function
---------------------------------------------------------
-- Subscriptions (and relevant helper functions)

{-- Helper function for subscriptions:
    This maps the coordinates of the click, which are given
    in JSON, to ints so we can bundle them into a Point
    
    credit: clickDecoder was based on Quiz 1' code (presumably written by Prof Chugh) 
--}
clickDecoder : Decode.Decoder Point
clickDecoder =
  Decode.map2
    (\x y -> (toFloat x, toFloat y))
    (Decode.field "clientX" Decode.int)
    (Decode.field "clientY" Decode.int)


{-- point to Msg:
    Helper function for subscriptions. 
    Turns a Point into a Msg so that 
    subscriptions can have the right type.
--}
pointToMsg : Point -> Msg
pointToMsg point = ClickedPoint point


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    -- React to clicks into the canvas
    [ Browser.Events.onClick (Decode.map pointToMsg clickDecoder) 

    -- Allows for animation (of splatter size and of drips)
    , onAnimationFrameDelta Frame
    ]

---------------------------------------------------------
-- Helper functions for view:


{-- placeOneSplatter: 
  Produces the visualization that corresponds to a specific click.

  Depending on settings in the model, this visualization may be a 
  circle, a blob, a ray, and with or without paint dripping
--}
placeOneSplatter : Model -> Splatter -> List Shape
placeOneSplatter model splat =
    let
      x = Tuple.first splat.loc
      y = Tuple.second splat.loc
      r = splat.currRadius
      drip = (dripPaint model.isDripping x y splat.dripLength)
    in
    if model.raysInsteadOfBlobs then
      -- Plots rays instead of blobs
      rays True x y
    else
      -- Include the "paint dripping" effect
      drip ::
      if model.plainCircles then
        -- Plot plain circles instead of blobs
        [circle (x, y) r]
      else
        --Plot fun blobs. These are pre-set in AllBlobs to guarantee that they look pretty :)
        case splat.blobID of
          1 -> blob1 x y r
          2 -> blob2 x y r
          3 -> blob3 x y r
          4 -> blob4 x y r
          _ -> blob5 x y r
      

{--placeSplatter: 
  Wraps the shape produces by placeOneSplatter into a Canvas.Renderable type 
--}
placeSplatter : Model -> Splatter -> Color.Color -> Renderable
placeSplatter model pt colors =
    Canvas.shapes [fill colors] (placeOneSplatter model pt)


{-- Functions for mixing paint --}
{-- addColor: add a new color to colorList and 
    modify the rest of the colorlist
--}
addColor : Color.Color -> Deque Color.Color -> Deque Color.Color
addColor c colors = 
  if Deque.isEmpty colors then 
    Deque.addBack c colors
  else 
    Deque.addBack c (Deque.mapToDeque (mixColors c) (Deque.indexedMap colors))

{-- mixColors: mix two colors with a given factor --}
mixColors : Color.Color -> (Int, Color.Color) -> Color.Color
mixColors mixColor (index, oColor) =
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

{-- Functions for view --}
-- Wrappers for width and height for the actual "draw area"
canvasWidth width = 3 * width // 4
canvasHeight height = 7 * height // 8


-- Citation: referenced small amount of code from https://elm-lang.org/examples/image-previews
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
            Nothing -> (800, 800)
            Just viewport ->
                (round viewport.viewport.width
                , round viewport.viewport.height)
      basecolor = 
        case peekBack model.colorList of  -- Most recently added
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
      h = style "height" "50px"
      h2 = style "height" "28px"
      fontsize = style "font" "Comic sans MS"
      otherbackground = style "background-color" "rgba(0, 0, 0, 0)"


      -- ourShapes: Produce most recent shapes. Or, if the list is emptied, clear the canvas.
      ourShapes = 
        if (Deque.isEmpty model.splatterList) then
          -- The list is empty, so clear the previously-rendered shapes
          [Canvas.clear (0, 0) (toFloat (canvasWidth width)) (toFloat (canvasHeight height))]
        else 
          case model.toRerender of
            Just removeMe ->
              -- If this is set, there is something new to erase.
              let
                -- Increment border, so that it erases cleanly without leaving a ghost border behind
                removeMeSlightlyLargerBorder = {removeMe | currRadius = removeMe.currRadius + 5}
              in
              [Canvas.shapes [fill Color.white, stroke Color.white] 
                (placeOneSplatter model removeMeSlightlyLargerBorder)]

            Nothing ->
              -- Nothing to erase. So, we'll render the shapes affected by the most recent color change

              {--If there is only one color so far, but multiple splatters, we need to repeat
                the single color to get the "map" call to render every splatter --}
              if ((Deque.length model.colorList) == 1) && ((Deque.length model.splatterList) > 1) then
                case Deque.peekFront model.colorList of
                  Just onlyCol ->
                    Deque.squishToList 
                      (Deque.map2ToDeque (placeSplatter model) 
                        model.splatterList (Deque.repeat (Deque.length model.splatterList) onlyCol))
                  Nothing ->
                    Deque.squishToList 
                      (Deque.map2ToDeque (placeSplatter model) 
                        model.splatterList model.colorList)
                else
              -- There are multiple colors and multiple splatters. Rerenders
              -- the splatters that were affected by the most recent color change.
              Deque.squishToList 
                (Deque.map2ToDeque (placeSplatter model) model.splatterList model.colorList)

      -- End of ourShapes

      -- Custom button text
      rays = 
        if model.raysInsteadOfBlobs then 
          if Deque.isEmpty model.splatterList then "Click onto the canvas. Or, click 'Explode'"
          else "Turn Rays Off" 
        else "Turn Rays On"

      boringCircles = if model.plainCircles then "Go back to blobs" else "Normal circles please"
      clearScreenText = if model.explode then "Stop the explosion" else "Clear the screen"
      explodeText = if model.explode then "(Click different colors while the explosion runs)" else "Explode"
      
      -- Report the color of the current mix:
      printRGBNicely rgb =
        String.slice 0 4 (Debug.toString rgb)

      currentColorMix = 
        case Deque.peekFront model.colorList of
          Just currColor -> 
            let colorDict = Color.toRgba currColor
            in
            "R: " ++ printRGBNicely colorDict.red
            ++ " G: " ++ printRGBNicely colorDict.green
            ++ " B : " ++ printRGBNicely colorDict.blue

          Nothing -> "Select a color to begin"
      newCurrColor = 
        case Deque.peekFront model.colorList of
          Just currColor -> Color.toRgba currColor
          _ -> Color.toRgba Color.white

    in
    let othercolor = style "color" ("rgba(" ++ (String.fromFloat (basecolor2.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor2.green)) ++ ", "++ (String.fromFloat (250 * basecolor2.blue)) ++ ", " ++ (String.fromFloat 1) ++ ")")
        othercolor2 = style "color" ("rgba(" ++ (String.fromFloat (newCurrColor.red * 250)) ++ ", " ++ (String.fromFloat (250*newCurrColor.green)) ++ ", "++ (String.fromFloat (250 * newCurrColor.blue)) ++ ", " ++ (String.fromFloat 1) ++ ")")
    in
    div
        [ style "display" "flex"
        , style "justify-content" "flex-start" -- Keep this as flex-start (left-aligned fixes the click offset issue)
        , style "align-items" "center"
        ]
        [
        Canvas.toHtml
            (canvasWidth width, canvasHeight height)
            [ style "border" ("15px solid rgba(" ++ (String.fromFloat (basecolor.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor.green)) ++ ", "++ (String.fromFloat (250 * basecolor.blue)) ++ ", " ++ (String.fromFloat 0.5) ++ ")")] --i haven't messed around with this line, feel free to!
            ourShapes
        , div 
              [style "border" ("15px solid rgba(" ++ (String.fromFloat (basecolor.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor.green)) ++ ", "++ (String.fromFloat (250 * basecolor.blue)) ++ ", " ++ (String.fromFloat 0.5) ++ ")")
              , style "backgroundColor" ("rgba(" ++ (String.fromFloat (basecolor.red * 250)) ++ ", " ++ (String.fromFloat (250*basecolor.green)) ++ ", "++ (String.fromFloat (250 * basecolor.blue)) ++ ", " ++ (String.fromFloat 0.2) ++ ")")
              , style "width" "200px"
              , style "height" (String.fromInt (canvasHeight height + 3) ++ "px")
              ]
          [ viewPreview "https://davinstudios.com/sitebuilder/images/Original_Splash_With_Drips_10-31-16-642x209.png" 
          , Html.p [fontsize, othercolor2] [text "Your final mix:"]
          , Html.p [fontsize, othercolor2] [text currentColorMix]
          --, text ("len splatterList: " ++ Debug.toString (List.length (Deque.squishToList model.splatterList)))
          , button [fontsize, noborder, h, w, textcolor, r, Html.Events.onClick (PickColor Color.red)] [ text "Red" ]
          , button [noborder, h, w, textcolor,o, Html.Events.onClick (PickColor Color.orange)] [ text "Orange" ]
          , button [noborder, h, w, textcolor,y, Html.Events.onClick (PickColor Color.yellow)] [ text "Yellow" ]
          , button [noborder, h, w, textcolor,g, Html.Events.onClick (PickColor Color.green)] [ text "Green" ]
          , button [noborder, h, w, textcolor,b, Html.Events.onClick (PickColor Color.blue)] [ text "Blue" ]
          , button [noborder, h, w, textcolor,p, Html.Events.onClick (PickColor Color.purple)] [ text "Purple" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick ToggleRays] [ text rays ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick TogglePlainCircles] [ text boringCircles ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick EraseOldest] [ text "Erase oldest" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick EraseNewest] [ text "Erase newest" ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick (Explode width height)] [ text explodeText ]
          , button [noborder, h2, w, othercolor, otherbackground, Html.Events.onClick ClearScreen] [ text clearScreenText ]
          ]
        ]
