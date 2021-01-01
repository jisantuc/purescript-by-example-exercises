module MySolutions where

import Prelude
import Affjax (defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alt ((<|>))
import Control.Parallel (parTraverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Traversable (foldl, traverse)
import Effect.Aff (Aff, Error, attempt)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles inf1 inf2 outPath = do
  f1 <- readTextFile UTF8 inf1
  f2 <- readTextFile UTF8 inf2
  writeTextFile UTF8 outPath (f1 <> f2)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany infs out = do
  inData <- traverse (readTextFile UTF8) infs
  let
    concatenated = foldl (\acc x -> acc <> x) "" inData
  writeTextFile UTF8 out concatenated

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel infs out = do
  inData <- parTraverse (readTextFile UTF8) infs
  let
    concatenated = foldl (\acc x -> acc <> x) "" inData
  writeTextFile UTF8 out concatenated

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters inf = attempt $ length <$> readTextFile UTF8 inf

writeGet :: String -> FilePath -> Aff Unit
writeGet url outPath = do
  result <- AX.get ResponseFormat.string url
  case result of
    Right resp -> writeTextFile UTF8 outPath resp.body
    Left err ->
      Console.log
        ( "Failed request to " <> url <> ": " <> AX.printError err
        )

race :: forall a. Aff a -> Aff a -> Aff a
race x y = x <|> y

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout timeout url =
  ( \x -> case x of
      Right rec -> Just rec.body
      Left err -> Just (AX.printError err)
  )
    <$> ( AX.request
          $ defaultRequest { url = url, responseFormat = ResponseFormat.string }
      )
