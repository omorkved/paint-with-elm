
import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Time
import Debug

{-- If we want to use collage:
import Collage exposing (circle, filled, uniform, Shape, Collage, shift)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg, svgBox)
import Collage.Text as Text exposing (Text, fromString, size, large, shape)
import Color exposing (..)
--}

-- taken from Canvas docs
import Canvas exposing (..)
import Canvas.Settings exposing (..)
--import Color -- elm install avh4/elm-color
import Html exposing (Html)
import Html.Attributes exposing (style)

----------------------------------------------------------------------



-- For this commit: Starter code, taken from prof. Ravi Chugh

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = { count: Int }

initModel = { count = 0 }


-- UPDATE

type Msg = Noop | Reset | Increment

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
    Reset ->
      (initModel, Cmd.none)
    Increment ->
      ({ count = 1 + model.count }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    -- https://css-tricks.com/quick-css-trick-how-to-center-an-object-exactly-in-the-center/
    let
      styles =
        [ ("position", "fixed")
        , ("top", "50%")
        , ("left", "50%")
        , ("transform", "translate(-50%, -50%)")
        ]
      display =
        Html.text ("Count: " ++ Debug.toString model.count)
    in
      Html.div (List.map (\(k, v) -> Attr.style k v) styles) [display]


-- SUBSCRIPTIONS

-- https://github.com/elm/browser/blob/1.0.0/notes/keyboard.md
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onMouseDown (Decode.succeed Increment)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "Escape" then Reset else Noop) keyDecoder)
    ]