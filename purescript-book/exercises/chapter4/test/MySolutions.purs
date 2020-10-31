module Test.MySolutions where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concat, filter, head, length, nub, null, range, sort, tail)
import Data.Foldable (foldl)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Math (floor, sqrt)
import Test.Examples (factors)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n = if n == 0 then true else if n == 1 then false else isEven (n - 2)

countEven :: Array Int -> Int
countEven ns =
  if null ns then
    0
  else
    if (fromMaybe false $ (isEven <$> head ns)) then
      (1 + _) $ countEven $ fromMaybe [] $ tail ns
    else
      countEven $ (fromMaybe [] $ tail ns)

squared :: Array Number -> Array Number
squared = ((\x -> x * x) <$> _)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = ((_ >= 0.0) <$?> _)

isPrime :: Int -> Boolean
isPrime n = n > 1 && (length $ factors n) == 1

triples :: Int -> Array (Array Int)
triples n = do
  a <- range 1 n
  b <- range a n
  c <- range b n
  guard $ a * a + b * b == c * c
  pure $ [ a, b, c ]

factorize :: Int -> Array Int
factorize n =
  if (n < 2) then
    []
  else
    if (isPrime n) then
      [ n ]
    else
      (reverse <<< sort <<< nub)
        $ do
            i <- range 2 (fromMaybe 2 $ fromNumber (floor <<< sqrt <<< toNumber $ n))
            guard $ (n `mod` i == 0 && isPrime i)
            concat [ [ i ], if (isPrime $ n / i) then [ n / i ] else [], (factorize $ n / i) ]

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\x y -> x && y) true

reverse :: forall a. Array a -> Array a
reverse arr = if (null arr) then arr else foldl (\xs x -> [ x ] <> xs) [] arr
