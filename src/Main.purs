module Main where

import Prelude

import Effect (Effect)
import Euler (answer)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ "The answer is " <> show answer