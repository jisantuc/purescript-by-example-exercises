module Test.MySolutions where

import Prelude
import Data.Array (dropEnd, foldMap, foldl, foldr, last, reverse, singleton)
import Data.Foldable (class Foldable)

-- Note to reader: Add your solutions to this file
newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> signPart <> show imaginary <> "i"
    where
    signPart
      | imaginary > 0.0 = "+"
      | otherwise = ""

instance eqComplex :: Eq Complex where
  eq (Complex { real: real1, imaginary: imag1 }) (Complex { real: real2, imaginary: imag2 }) = real1 == real2 && imag1 == imag2

data NonEmpty a
  = NonEmpty a (Array a)

instance eqNonEmptyA :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty h1 t1) (NonEmpty h2 t2) = h1 == h2 && t1 == t2

instance semigroupNonEmptyA :: Semigroup (NonEmpty a) where
  append (NonEmpty h1 t1) (NonEmpty h2 t2) = NonEmpty h1 (t1 <> singleton h2 <> t2)

instance functorNonEmpty :: Functor (NonEmpty) where
  map f (NonEmpty h t) = NonEmpty (f h) (f <$> t)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty h t) = show (singleton h <> t)

instance foldNonEmpty :: Foldable (NonEmpty) where
  foldl f default (NonEmpty h t) = foldl f (f default h) t
  foldr f default (NonEmpty h t) = (foldr f default (singleton h <> t))
  foldMap f (NonEmpty h t) = foldMap f (singleton h <> t)

data Extended a
  = Finite a
  | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite x) (Finite y) = x == y

instance ordExtended :: (Ord a, Eq a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

data OneMore f a
  = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl f default (OneMore one others) = foldl f first others
    where
    first = f default one
  foldr f default (OneMore one others) = f one t
    where
    t = foldr f default others
  foldMap f (OneMore one others) = (f one) <> foldMap f others
