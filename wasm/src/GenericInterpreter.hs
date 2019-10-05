{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GenericInterpreter where

import Prelude hiding (fail)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Stack
import Control.Arrow.Fix

import Data.String
import Data.Int

import Syntax

data Config v = Config { confCode :: Code v }

class Arrow c => IsVal v c | c -> v where
    int64 :: c Int64 v
    float64 :: c Double v
    eq :: c (v, v) v
    lt :: c (v, v) v
    add :: c (v, v) v
    neg :: c v v

type CanInterp v e c = (
    ArrowStack v c,
    IsVal v c,
    IsString e,
    ArrowChoice c,
    ArrowFail e c,
    ArrowFix (c [AdminInstr v] [AdminInstr v]))

step :: CanInterp v e c => c [AdminInstr v] [AdminInstr v]
step = fix $ \step' -> proc instr -> case instr of

    (Plain instr':rest) -> case instr' of
        Const wasmval -> case wasmval of
            I64Val v -> do
                val <- int64 -< v
                push -< val
                step' -< rest
            F64Val v -> do
                val <- float64 -< v
                push -< val
                step' -< rest

        Binary t op -> case op of
            Add -> do
                val1 <- pop -< t
                val2 <- pop -< t
                push <<< add -< (val1, val2)
                step' -< rest
        
        Unary t op -> case op of
            Neg -> do
                val <- pop -< t
                push <<< neg -< val
                step' -< rest
        
        Compare t op -> case op of
            Eq -> do
                val1 <- pop -< t
                val2 <- pop -< t
                push <<< eq -< (val1, val2)
                step' -< rest

            Lt -> do
                val1 <- pop -< t
                val2 <- pop -< t
                push <<< lt -< (val1, val2)
                step' -< rest
    
    _ -> fail -< fromString "Not Implemented"
        
        

