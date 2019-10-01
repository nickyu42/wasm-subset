{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix(Fix,Fix',ArrowFix(..),IterationStrategy,transform,filter) where

import Prelude hiding (filter,pred)

import Control.Arrow
import Control.Arrow.Trans

import Data.Profunctor
import Data.Lens(Iso',from,Prism',getMaybe,get,set)

-- | Type family that computes the type of the fixpoint.
type family Fix (c :: * -> * -> *) x y :: * -> * -> *
type Fix' c x y = Fix c x y x y

-- | Interface for describing fixpoint computations.
class ArrowFix c where
  -- | Computes the fixpoint of an arrow computation.
  fix :: (c -> c) -> c

  default fix :: (c ~ c' x y, ArrowTrans c', Underlying c' x y ~ c'' x' y', ArrowFix (c'' x' y')) => (c -> c) -> c
  fix f = lift (fix (unlift . f . lift))
  {-# INLINE fix #-}

type instance Fix (->) x y = (->)
instance ArrowFix (x -> y) where
  fix f = f (fix f)

type IterationStrategy c a b = c a b -> c a b

transform :: Profunctor c => Iso' a a' -> IterationStrategy c a' b -> IterationStrategy c a b
transform iso strat f = lmap (get iso) (strat (lmap (get (from iso)) f))
{-# INLINE transform #-}

filter :: (Profunctor c, ArrowChoice c, ArrowApply c) => Prism' a a' -> IterationStrategy c a' b -> IterationStrategy c a b
filter pred strat f = proc a -> case getMaybe pred a of
  Just a' -> strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> f -< a
{-# INLINE filter #-}

