{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}                 -- for newtype multiple args
{-# LANGUAGE TypeFamilies #-}               -- for type family Run
{-# LANGUAGE UndecidableInstances #-}       -- allow ArrowFail String instance in UseVal declaration
module ConcreteInterpreter where

import Prelude hiding (fail)

-- Sturdy modules
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.Concrete.Failure
import Data.Concrete.Error

-- Own modules
import Control.Arrow.Stack
import Transformer.Concrete.Stack
import GenericInterpreter as Generic
import Syntax

import Control.Category
import Control.Arrow
import Data.Profunctor
import Data.Int

data Val
    = I32Val Int32
    | F64Val Double
    deriving (Show, Eq)

wasmBool :: Bool -> Val
wasmBool True = I32Val 1
wasmBool False = I32Val 0

newtype ConcreteT c x y = ConcreteT (c x y) 
    deriving (Profunctor, Category, Arrow, ArrowFail e, ArrowStack val, ArrowChoice)

deriving instance ArrowFix (c x y) => ArrowFix (ConcreteT c x y)

instance ArrowRun c => ArrowRun (ConcreteT c) where
    type Run (ConcreteT c) x y = Run c x y

    -- | run applies run on the lower layer arrow
    run (ConcreteT c) = Trans.run c

instance (ArrowChoice c, ArrowFail String c) => UseVal Val (ConcreteT c) where
    int32 = arr I32Val
    float64 = arr F64Val
    eq = proc v -> case v of
        (I32Val v1, I32Val v2) -> returnA -< wasmBool $ v1 == v2
        (F64Val v1, F64Val v2) -> returnA -< wasmBool $ v1 == v2
        _ -> fail -< "Invalid value types for eq comparison"
    lt = proc v -> case v of
        (I32Val v1, I32Val v2) -> returnA -< wasmBool $ v1 < v2
        (F64Val v1, F64Val v2) -> returnA -< wasmBool $ v1 < v2
        _ -> fail -< "Invalid value types for lt comparison"
    add = proc v -> case v of
        (I32Val v1, I32Val v2) -> returnA -< I32Val $ v1 + v2
        (F64Val v1, F64Val v2) -> returnA -< F64Val $ v1 + v2
        _ -> fail -< "Invalid value type for addition"
    neg = proc v -> case v of
        (F64Val v') -> returnA -< F64Val (-v')
        _ -> fail -< "Non-float value given for negation"

type TransformerStack = ConcreteT (StackT Val (FailureT String (->))) [AdminInstr Val] ()

run :: [Instr] -> Error String [Val]
run instr =
    fst <$>
        Trans.run
            (Generic.run :: TransformerStack)
            ([], (map Plain instr))