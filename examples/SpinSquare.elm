module SpinSquare (Model, Action(..), init, update, view) where

import Easing exposing (ease, easeOutBounce, float)
import Html exposing (Html)
import Svg exposing (svg, rect, g, text, text')
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (Time, second)
import StartApp exposing (HandledTask)
import Ticker exposing (tock)


-- MODEL

type alias Model =
    { angle : Float
    , animationState : AnimationState
    }


type alias AnimationState =
    Maybe { prevClockTime : Time,  elapsedTime: Time }


init : (Model, List Action)
init =
  ( { angle = 0, animationState = Nothing }
  , []
  )


rotateStep = 90
duration = second


-- UPDATE

type Action
    = Spin
    | Tick Time


update : Signal.Address Action -> Action -> Model -> (Model, Maybe HandledTask)
update address msg model =
  case msg of
    Spin ->
      case model.animationState of
        Nothing ->
          ( model, Just <| tock address Tick )

        Just _ ->
          ( model, Nothing )

    Tick clockTime ->
      let
        newElapsedTime =
          case model.animationState of
            Nothing ->
              0

            Just {elapsedTime, prevClockTime} ->
              elapsedTime + (clockTime - prevClockTime)
      in
        if newElapsedTime > duration then
          ( { angle = model.angle + rotateStep
            , animationState = Nothing
            }
          , Nothing
          )
        else
          ( { angle = model.angle
            , animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
            }
          , Just <| tock address Tick
          )


-- VIEW

toOffset : AnimationState -> Float
toOffset animationState =
  case animationState of
    Nothing ->
      0

    Just {elapsedTime} ->
      ease easeOutBounce float 0 rotateStep duration elapsedTime


view : Signal.Address Action -> Model -> Html
view address model =
  let
    angle =
      model.angle + toOffset model.animationState
  in
    svg
      [ width "200", height "200", viewBox "0 0 200 200" ]
      [ g [ transform ("translate(100, 100) rotate(" ++ toString angle ++ ")")
          , onClick (Signal.message address Spin)
          ]
          [ rect
              [ x "-50"
              , y "-50"
              , width "100"
              , height "100"
              , rx "15"
              , ry "15"
              , style "fill: #60B5CC;"
              ]
              []
          , text' [ fill "white", textAnchor "middle" ] [ text "Click me!" ]
          ]
      ]
