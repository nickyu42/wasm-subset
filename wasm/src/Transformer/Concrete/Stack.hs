{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Transformer.Concrete.Stack where 

import Prelude hiding ((.), fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Stack
import Control.Arrow.Const
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Transformer.State

import Data.Profunctor
import Data.String

-- | Transformer that adds a stack to a computation
newtype StackT val c x y = StackT (StateT ([val]) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
              ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e)


instance (ArrowChoice c, Profunctor c, IsString e, ArrowFail e c) 
    => ArrowStack val (StackT val c) where

    peek = StackT $ proc _ -> do
        s <- get -< ()
        case s of
            v:_ -> returnA -< v
            [] -> fail -< fromString "Can't peek empty stack"

    -- the concrete implementation of the stack
    -- does not check if the correct value is on the stack
    pop = StackT . modify $ proc (_, s) -> do
        case s of
            v:vs -> returnA -< (v, vs)
            [] -> fail -< fromString "Can't pop from empty stack"

    push = StackT . modify . arr $ \(v, s) -> ((), v:s)

    getStack = StackT get
    putStack = StackT put

-- | StateT is already an instance of ArrowState, so the functions are lifted
instance ArrowState s c => ArrowState s (StackT val c) where
  get = lift' get
  put = lift' put

instance ArrowFix (Underlying (StackT val c) x y) => ArrowFix (StackT val c x y)
type instance Fix (StackT val c) x y = StackT val (Fix c [val] [val]) 