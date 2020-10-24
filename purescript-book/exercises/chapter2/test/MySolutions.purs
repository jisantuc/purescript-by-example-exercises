module Test.MySolutions where

import Global (readFloat)
import Prelude

import Math (e, pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea radius = pi * radius * radius

addE :: String -> Number
addE = (_ + e) <<< readFloat