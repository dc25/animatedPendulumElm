import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

dt = 0.01
scale = 100

type alias Model =
  { angle : Float
  , angVel : Float
  , length : Float
  , gravity : Float
  }

init =
  { angle = pi / 2
  , angVel = 0.0
  , length = 2
  , gravity = -9.81
  }

update : Model -> Model
update model =
  let
    angAcc = -1.0 * (model.gravity / model.length) * sin (model.angle)
    angVel' = model.angVel + angAcc * dt
    angle' = model.angle + angVel' * dt
  in
    { model
      | angle = angle'
      , angVel = angVel'
    }

view model =
  let
    endPoint = ( 0, scale * model.length )
    pendulum =
      group
        [ segment ( 0, 0 ) endPoint
            |> traced { defaultLine | width = 2, color = red }
        , circle 8
            |> filled blue
        , circle 5
            |> filled green
            |> move endPoint
        ]
  in
    collage 700 500
      [ pendulum |> rotate model.angle ]

main =
  Signal.foldp (\_ model -> update model) init (every (dt * second))
    |> Signal.map view
