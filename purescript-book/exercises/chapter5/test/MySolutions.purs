module Test.MySolutions where

import Prelude
import Data.Person (Address)

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
