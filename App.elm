module App where

import Color
import Mouse

e (col,text) = plainText text |> color col |> width 400

type Model = [(Color, String)]

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

scene : Model -> Maybe (Color, String) -> Element
scene m x = flow down
  [ flow down (map e m)
  , asText <| show x
  ]

findRow : (Int,Int) -> Model -> Maybe (Color, String)
findRow (x,y) m =
  case m of
  (next::rest) -> 
    let h = (heightOf (e next))
    in if 
      | y < h -> Just next
      | otherwise -> findRow (x,y-h) rest
  _ -> Nothing

modelSignal = constant model

main = lift2 scene modelSignal (lift2 findRow Mouse.position modelSignal)