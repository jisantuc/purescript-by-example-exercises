module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (message)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import MySolutions (concatenateFiles, concatenateMany, concatenateManyParallel, countCharacters, getWithTimeout, writeGet)

main :: Effect Unit
main =
  Aff.launchAff_ do
    concatenateFiles "foo.txt" "bar.txt" "baz.txt"
    concatenateMany [ "foo.txt", "bar.txt", "baz.txt" ] "many.txt"
    concatenateManyParallel [ "foo.txt", "bar.txt", "baz.txt" ] "manypar.txt"
    count <- countCharacters "foo.txt"
    case count of
      Right charCount -> Console.log ("Char count was: " <> show charCount)
      Left err -> Console.log ("Something went wrong while counting characters: " <> message err)
    writeGet "https://backsplash.rasterfoundry.com/healthcheck" "healthcheck.json"
    zeroTimeout <- getWithTimeout 0.0 "http://reqres.in/api/users/1"
    longTimeout <- getWithTimeout 9000.0 "http://reqres.in/api/users/1"
    Console.log $ "Result with no time: " <> show zeroTimeout
    Console.log $ "Result with time: " <> show longTimeout
