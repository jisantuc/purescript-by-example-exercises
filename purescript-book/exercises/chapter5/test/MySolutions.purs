module Test.MySolutions where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Person (Address)
import Data.Picture (Point(..), Shape(..))
import Test.NoPeeking.Solutions (doubleScaleAndCenter)

factorial :: Int -> Int
factorial 0 = 1

factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1

binomial 0 _ = 0

binomial n k
  | n < k = 0
  | otherwise = (factorial n) / ((factorial k) * (factorial $ n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1

pascal 0 _ = 0

pascal n k
  | n < k = 0
  | otherwise = (pascal (n - 1) k) + pascal (n - 1) (k - 1)

sameCity :: forall r. { address :: Address | r } -> { address :: Address | r } -> Boolean
sameCity { address: { city: x } } { address: { city: y } } = x == y

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x

fromSingleton default _ = default

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ radius) = Circle origin (radius * 2.0)

doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)

doubleScaleAndCenter (Line (Point { x: startX, y: startY }) (Point { x: endX, y: endY })) =
  let
    xDelta = abs $ (endX - startX) / 2.0 + startX

    yDelta = abs $ (endY - startY) / 2.0 + startY

    newStartX = (startX - xDelta) * 2.0

    newStartY = (startY - yDelta) * 2.0

    newEndX = (endX - xDelta) * 2.0

    newEndY = (endY - yDelta) * 2.0
  in
    Line (Point { x: newStartX, y: newStartY }) (Point { x: newEndX, y: newEndY })

doubleScaleAndCenter (Text _ msg) = Text origin msg

shapeText :: Shape -> Maybe String
shapeText (Text _ msg) = Just msg

shapeText _ = Nothing
