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
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
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

type alias Point = { x : Float, y : Float }

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
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- a Frame msg makes things spin
    Frame _ ->
      ( { model | count = model.count + 1 }, Cmd.none )

    -- a ClickedPoint message means a new click for a paint splatter
    ClickedPoint newClick ->
      ( { model | clickList = newClick :: model.clickList }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.todo "time to batch..."

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
        ,--} Html.div [] [Html.text ("num clicks: " ++ String.fromInt (List.length model.clickList))]
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

