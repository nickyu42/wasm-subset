{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Trace where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate)

import Control.Category
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Fix.Chaotic
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Context as Context
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Order

import Data.Profunctor.Unsafe
import Data.Coerce

import Debug.Trace as Debug
import Text.Printf

trace :: (Show a, Show b, Arrow c) => IterationStrategy c a b
trace f = proc x -> do
  y <- f -< Debug.trace (printf "CALL\n%s\n\n" (show x)) x
  returnA -< Debug.trace (printf "RETURN\neval(%s)\n\t= %s\n\n" (show x) (show y)) y
{-# INLINE trace #-}

newtype TraceT c x y = TraceT (c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowComplete z,ArrowJoin,ArrowEffectCommutative,ArrowComponent a,ArrowStack a,ArrowContext ctx,ArrowState s)

instance (Show a, ArrowIterate a c) => ArrowIterate a (TraceT c) where
  iterate = TraceT $ proc (a,b) ->
    iterate -< Debug.trace (printf "ITERATE\n\tx: %s\n\n" (show a)) (a,b)

instance (Show a, Show b, ArrowCache a b c) => ArrowCache a b (TraceT c) where
  lookup = TraceT $ proc a -> do
    b  <- lookup -< a
    returnA -< Debug.trace (printf "LOOKUP\n\tx: %s\n\ty: %s\n\n" (show a) (show b)) b
  update = TraceT $ proc (a,b) -> do
    bOld  <- lookup -< a
    (s,b') <- update -< (a,b)
    returnA -< Debug.trace (printf "UPDATE\n\tx: %s\n\ty: %s -> %s, %s\n\n" (show a) (show bOld) (show b') (show s))  (s,b')
  write = TraceT $ proc (a,b,s) ->
    write -< Debug.trace (printf "WRITE\n\tx: %s\n\ty: %s\n\t%s\n\n" (show a) (show b) (show s)) (a,b,s)
  setStable = TraceT $ proc (s,a) -> 
    setStable -< Debug.trace (printf "STABLE: %s\n\n" (show s)) (s,a)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

runTraceT :: TraceT c x y -> c x y
runTraceT (TraceT f) = f
{-# INLINE runTraceT #-}

instance ArrowRun c => ArrowRun (TraceT c) where
  type Run (TraceT c) x y = Run c x y
  run f = run (runTraceT f)

instance (Profunctor c,ArrowApply c) => ArrowApply (TraceT c) where
  app = TraceT (app .# first coerce)
  {-# INLINE app #-}

