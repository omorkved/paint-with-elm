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
-- from original:
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onClick)
import Canvas exposing (rect, shapes)
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

type alias Point = { x : Int, y : Int }

type alias Model =
    { count : Float
    , clickList : List Point
     }


type Msg
    = Frame Float | ClickedPoint Point


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( { count = 0, clickList = [] }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- a Frame msg makes things spin
    Frame _ ->
      ( { model | count = model.count + 1 }, Cmd.none )

    -- a ClickedPoint message gives us a new coordinate for a paint splatter
    ClickedPoint newClick ->
      ( { model | clickList = newClick :: model.clickList }, Cmd.none )



{-- Helper function for subscriptions:
    This maps the coordinates of the click, which are given
    in JSON, to ints so we can bundle them into a Point
 --}
-- credit: clickDecoder taken from Quiz 1' code (presumably written by Prof Chugh)
clickDecoder : Decode.Decoder Point
clickDecoder =
  Decode.map2
    (\x y -> { x = x, y = y })
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
width =
    400


height =
    400


centerX =
    width / 2


centerY =
    height / 2


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
        ]


clearScreen =
    shapes [ fill Color.green ] [ rect ( 0, 0 ) width height ]

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

