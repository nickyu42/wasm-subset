{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.Error(ErrorT,runErrorT) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Abstract.Error

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype ErrorT e c x y = ErrorT (KleisliT (Error e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun, ArrowJoin,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure var val env, ArrowStore a b,
            ArrowExcept e')

runErrorT :: ErrorT e c x y -> c x (Error e y)
runErrorT = coerce
{-# INLINE runErrorT #-}

instance (ArrowChoice c, Profunctor c) => ArrowFail e (ErrorT e c) where
  fail = lift $ arr Fail

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ErrorT e c) where
  app = lift (app .# first coerce)

type instance Fix (ErrorT e c) x y = ErrorT e (Fix c x (Error e y))
deriving instance (ArrowFix (Underlying (ErrorT e c) x y)) => ArrowFix (ErrorT e c x y)

instance (ArrowChoice c, ArrowLowerBounded c) => ArrowLowerBounded (ErrorT e c) where
  bottom = lift bottom

deriving instance (ArrowChoice c, ArrowComplete (Error e y) c) => ArrowComplete y (ErrorT e c)

