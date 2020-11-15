module Test.MySolutions where

import Prelude
import Data.Array (foldMap, foldl, foldr, nub, nubByEq, singleton)
import Data.Array.Partial as Partial
import Data.Foldable (class Foldable, maximum)
import Data.Hashable (hash, hashEqual, class Hashable)
import Data.Maybe (Maybe(..))

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

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just n -> n
  _ -> Partial.head xs

newtype Multiply
  = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

derive newtype instance eqMultiply :: Eq Multiply

derive newtype instance showMultiply :: Show Multiply

class
  Monoid m <= Action m a where
  act :: m -> a -> a

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) m = n * m

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = go n s
    where
    go 0 _ = ""

    go 1 s' = s'

    go n' s' = go (n' - 1) (s' <> s)

instance actionApply :: Action Multiply (Array Int) where
  act (Multiply n) arr = (_ * n) <$> arr

newtype Self m
  = Self m

derive newtype instance eqSelf :: Eq m => Eq (Self m)

derive newtype instance showSelf :: Show m => Show (Self m)

derive newtype instance semigroupSelf :: Semigroup m => Semigroup (Self m)

derive newtype instance monoidSelf :: Monoid m => Monoid (Self m)

instance actMultiplySelf :: Action Multiply (Self Multiply) where
  act n (Self m) = Self $ n <> m

instance actMultiply :: Action (Self Multiply) Int where
  act (Self (Multiply m)) n = n * m

arrayHasDuplicates :: forall a. Hashable a => Ord a => Array a -> Boolean
arrayHasDuplicates arr = nubByEq hashEqual arr /= arr && (nub arr /= arr)

newtype Hour
  = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hourHashable :: Hashable Hour where
  hash (Hour n) = hash (mod n 12)
