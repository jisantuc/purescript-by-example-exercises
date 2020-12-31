module Test.MySolutions where

import Prelude

import Data.Array (head, nub, sort, tail)
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file
third :: forall a. Array a -> Maybe a
third arr = do
  dropped1 <- tail arr
  dropped2 <- tail dropped1
  head dropped2

possibleSums :: Array Int -> Array Int
possibleSums [] = [0]
possibleSums xs =
  (nub <<< sort) $ foldM (\acc x -> [acc, acc + x]) 0 xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM pred Nil = pure Nil
filterM pred (x : xs) = do
  result <- pred x
  filteredT <- filterM pred xs
  pure (if (result) then (x:filteredT) else filteredT)
