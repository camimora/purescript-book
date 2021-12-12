module Test.MySolutions where

import Prelude
import Data.Maybe (Maybe)
import Control.Apply (lift2)

-- Note to reader: Add your solutions to this file
addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe a b = lift2 add a b

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe a b = ado
  x <- a
  y <- b
  in x - y

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe a b = ado
  x <- a
  y <- b
  in x * y

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe a b = ado
  x <- a
  y <- b
  in x / y

addApply :: forall a f. Apply f => Semiring a => f a -> f a -> f a
-- addApply x y = (\a b -> a + b) <$> x <*> y
-- addApply a b = ado
--   x <- a
--   y <- b
--   in x + y
addApply x y = (map (\a b -> a + b) x) <*> y

subApply :: forall a f. Apply f => Ring a => f a -> f a -> f a
subApply a b = ado
  x <- a
  y <- b
  in x - y

mulApply :: forall a f. Apply f => Semiring a => f a -> f a -> f a
mulApply a b = ado
  x <- a
  y <- b
  in x * y

divApply :: forall a f. Apply f => EuclideanRing a => f a -> f a -> f a
divApply a b = ado
  x <- a
  y <- b
  in x / y

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance treeShow :: Show (Tree Int) where
  show (Leaf) = "Leaf"
  show (Branch t1 v t2) = "(Branch " <> show t1 <> " " <> show v <> " " <> show t2 <> ")"

instance treeEq :: Eq (Tree Int) where
  eq (Leaf) (Leaf) = true
  eq (Leaf) _ = false
  eq _ (Leaf) = false
  eq (Branch t1a v1 t1b) (Branch t2a v2 t2b) = (eq t1a t2a) && (v1 == v2) && (eq t1b t2b)
