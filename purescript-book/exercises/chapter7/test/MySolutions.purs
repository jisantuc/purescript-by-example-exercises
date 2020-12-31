module Test.MySolutions where

import Prelude
import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, validateAddress, validatePhoneNumber)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags, unicode)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)
import Partial.Unsafe (unsafePartial)

-- Note to reader: Add your solutions to this file
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

-- thanks, psc-ide suggestions 🤷🏻‍♂️
subMaybe :: forall t1 t2. Apply t1 => Ring t2 => t1 t2 -> t1 t2 -> t1 t2
subMaybe = lift2 (-)

mulMaybe :: forall t6 t7. Apply t6 => Semiring t7 => t6 t7 -> t6 t7 -> t6 t7
mulMaybe = lift2 (*)

divMaybe :: forall t11 t12. Apply t11 => EuclideanRing t12 => t11 t12 -> t11 t12 -> t11 t12
divMaybe = lift2 (/)

addApply :: forall f. Apply f => f Int -> f Int -> f Int
addApply = lift2 (+)

subApply :: forall t1 t2. Apply t1 => Ring t2 => t1 t2 -> t1 t2 -> t1 t2
subApply = subMaybe

mulApply :: forall t6 t7. Apply t6 => Semiring t7 => t6 t7 -> t6 t7 -> t6 t7
mulApply = mulMaybe

divApply :: forall t11 t12. Apply t11 => EuclideanRing t12 => t11 t12 -> t11 t12 -> t11 t12
divApply = divMaybe

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing

combineMaybe (Just v) = Just <$> v

stateRegex :: Regex
stateRegex =
  unsafePartial case regex "^[a-zA-Z]{2}$" noFlags of
    Right r -> r

-- ugh seriously unicode is the gotcha? 👎🏻
nonEmptyRegex :: Regex
nonEmptyRegex =
  unsafePartial case regex "^.*\\p{Ll}+.*$" unicode of
    Right r -> r

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved { street, city, state } = ado
  str <- matches "Street" nonEmptyRegex street *> pure street
  c <- matches "City" nonEmptyRegex city *> pure city
  stt <- matches "State" stateRegex state *> pure state
  in address str c stt

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance eqTree :: Eq a => Eq (Tree a) where
  eq Leaf Leaf = true
  eq (Branch left1 v1 right1) (Branch left2 v2 right2) = eq v1 v2 && eq left1 left2 && eq right1 right2
  eq (Branch _ _ _) Leaf = false
  eq Leaf (Branch _ _ _) = false

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch left v right) =
    "(Branch "
      <> show left
      <> " "
      <> show v
      <> " "
      <> show right
      <> ")"

instance functorTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch left v right) = Branch (f <$> left) (f v) (f <$> right)

instance foldTree :: Foldable Tree where
  foldl _ z Leaf = z
  foldl f z (Branch left v right) = foldl f (f (foldl f z left) v) right
  foldr f z Leaf = z
  foldr f z (Branch left v right) = foldr f (f v (foldr f z right)) left
  foldMap f Leaf = mempty
  foldMap f (Branch left v right) = foldMap f left <> f v <> foldMap f right

instance traversableTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch left v right) = ado
    l' <- traverse f left
    v' <- f v
    r' <- traverse f right
    in Branch l' v' r'
  sequence Leaf = pure Leaf
  sequence (Branch left v right) = ado
    l' <- sequence left
    v' <- v
    r' <- sequence right
    in Branch l' v' r'

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf

traversePreOrder f (Branch left v right) = ado
  v' <- f v
  l' <- traversePreOrder f left
  r' <- traversePreOrder f right
  in Branch l' v' r'

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf

traversePostOrder f (Branch left v right) = ado
  l' <- traversePostOrder f left
  r' <- traversePostOrder f right
  v' <- f v
  in Branch l' v' r'

sequenceUsingTraverse :: forall g f a. Traversable g => Applicative f => g (f a) -> f (g a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall a f g b. Traversable f => Applicative g => (a -> g b) -> f a -> g (f b)
traverseUsingSequence f = sequence <<< map f

type OptionalAddrPerson
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

validatePersonOptionalAddress :: OptionalAddrPerson -> V Errors OptionalAddrPerson
validatePersonOptionalAddress { firstName, lastName, homeAddress, phones } = ado
  first' <- matches "FirstName" nonEmptyRegex firstName *> pure firstName
  last' <- matches "LastName" nonEmptyRegex lastName *> pure lastName
  home' <- traverse validateAddress homeAddress
  phones' <- traverse validatePhoneNumber phones
  in { firstName: first', lastName: last', homeAddress: home', phones: phones' }
