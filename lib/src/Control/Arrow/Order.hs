{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Order where

import           Prelude hiding ((.))

import           Control.Arrow
import           Data.Order(Complete(..))
import           Data.Profunctor

class (Arrow c, Profunctor c) => ArrowLowerBounded c where
  bottom :: c x y

class (Arrow c, Profunctor c) => ArrowComplete y c where
  (<⊔>) :: c x y -> c x y -> c x y

instance Complete y => ArrowComplete y (->) where
  (<⊔>) f g = \x -> f x ⊔ g x
  {-# INLINE (<⊔>) #-}

-- | An arrow computation @c@ is effect commutative iff for all @f, g :: c x y@,
--
-- > (f ⊔ g) ⊑ (proc x -> do y1 <- f -< x
-- >                         y2 <- g -< x
-- >                         returnA -< y1 ⊔ y2)
-- 
-- and 
--
-- > (f ⊔ g) ⊑ (proc x -> do y1 <- g -< x
-- >                         y2 <- f -< x
-- >                         returnA -< y1 ⊔ y2)
--
class (Arrow c, Profunctor c) => ArrowEffectCommutative c
instance ArrowEffectCommutative (->)

class (Arrow c, Profunctor c) => ArrowJoin c where
  joinSecond :: c x y -> c (z,x) (z,y)

instance ArrowJoin (->) where
  joinSecond g = \(z,x) -> (z,g x)
  {-# INLINE joinSecond #-}

-- | Joins a list of arguments. Use it with idiom brackets:
-- @
--   let a = ...; b = ...; xs = ...
--   (| joinList (returnA -< a) (\x -> f -< x+b) |) xs
-- @
joinList :: (ArrowChoice c, ArrowComplete y c) => c (e,s) y -> c (e,(x,s)) y -> c (e,([x],s)) y
joinList empty f = proc (e,(l,s)) -> case l of
  []     -> empty -< (e,s)
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList empty f -< (e,(xs,s)))

joinList1 :: (ArrowChoice c, ArrowLowerBounded c, ArrowComplete y c) => c (e,(x,s)) y -> c (e,([x],s)) y
joinList1 f = proc (e,(l,s)) -> case l of
  []     -> bottom -< ()
  [x]    -> f -< (e,(x,s))
  (x:xs) -> (f -< (e,(x,s))) <⊔> (joinList1 f -< (e,(xs,s)))

joinList1' :: (ArrowChoice c, ArrowLowerBounded c, ArrowComplete y c) => c (x,e) y -> c ([x],e) y
joinList1' f = proc (l,e) -> case l of
  []     -> bottom -< ()
  [x]    -> f -< (x,e)
  (x:xs) -> (f -< (x,e)) <⊔> (joinList1' f -< (xs,e))

