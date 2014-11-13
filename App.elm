module App where

import Color
import Mouse
import Graphics.Input (..)

hoveredItemInput = input Nothing

e (col,text) = plainText text |> color col
  |> width 400
  |> hoverable hoveredItemInput.handle (\b -> if b then Just (col,text) else Nothing)

type Item = (Color, String)
type Model = [Item]

model : Model
model =
  [ (Color.red, "short")
  , (Color.blue, "\"long\"")
  , (Color.green, "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing ")
  , (Color.yellow, "yellow")
  , (Color.purple, "hose interested. Sections 1.10.32 and 1.10.33 from \"de Finibus Bonorum et Malorum")
  , (Color.orange, ".")
  ]

data Command
  = Move Int Int -- from, to

step : Command -> Model -> Model
step c (a::b::[]) = (b::a::[])

scene : Model -> Maybe Item -> Element
scene m x = flow down
  [ flow down (map e m)
  , asText <| show x
  ]

modelSignal = constant model

hoveredItem : Signal (Maybe Item)
hoveredItem = hoveredItemInput.signal

main = lift2 scene modelSignal hoveredItem