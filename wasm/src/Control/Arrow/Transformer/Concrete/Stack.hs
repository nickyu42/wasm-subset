{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.Stack where 

import Prelude hiding ((.))

import Control.Category
import Control.Arrow
import Control.Arrow.Stack
import Control.Arrow.Const
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Transformer.State

import Data.Profunctor

-- | Transformer that adds a stack to a computation
--   newtype StateT s c x y = StateT { runStateT :: c (s,x) (s,y) }
newtype StackT val c x y = StackT (StateT ([val]) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
              ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e)


instance (ArrowChoice c, Profunctor c, Eq val) => ArrowStack val (StackT val c) where
    type instance Join y (StackT val c) = ()

    -- f is success, g is failure
    pop (StackT f) (StackT g) = StackT $ proc expect -> do
        s <- get -< ()
        case s of
            v:_ -> if v == expect then f -< v else g -< expect
            [] -> g -< expect

    -- modify takes an arrow that modifies the state
    -- defined in Control.Arrow.State
    push = StackT $ modify $ arr $ \(v, s) -> ((), v:s)


-- | Add state in StackT 
instance ArrowState s c => ArrowState s (StackT val c) where
  get = lift' get
  put = lift' put