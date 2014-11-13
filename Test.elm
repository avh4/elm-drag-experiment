module Test where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import App
import App (Command(..))
import Color

i1 = (Color.red, "a")
i2 = (Color.blue, "b")

test1 = Suite "move command"
  [ test "can move adjacent items" <|
    App.step (Move 1 0) [i1, i2]
    `assertEqual`
    [i2,i1]
  ]

suite = Suite "elm-drag"
  [ test1
  ]
