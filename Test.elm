module Test where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import App
import App (Command(..))
import Color

i1 = (Color.red, "1")
i2 = (Color.blue, "2")
i3 = (Color.green, "3")
i4 = (Color.black, "4")

test1 = Suite "move command"
  [ test "can move adjacent items" <|
    App.step (Move 1 0) [i1, i2]
    `assertEqual`
    [i2,i1]
  , test "can move adjacent items in a list" <|
    App.step (Move 2 1) [i1, i2, i3, i4]
    `assertEqual`
    [i1, i3, i2, i4]
  , test "can move adjacent items in a list the other way" <|
    App.step (Move 1 3) [i1, i2, i3, i4]
    `assertEqual`
    [i1, i3, i4, i2]
  ]

suite = Suite "elm-drag"
  [ test1
  ]
