{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GenericInterpreter where

import Prelude hiding (fail, break)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Stack
import Control.Arrow.Fix

import Data.String
import Data.Int
import Text.Printf

import Syntax

class Arrow c => UseVal v c | c -> v where
    int32 :: c Int32 v
    float64 :: c Double v
    eq :: c (v, v) v
    lt :: c (v, v) v
    add :: c (v, v) v
    neg :: c v v

-- class Arrow c => UseControl frame c | c -> frame where
--     askContext :: c () ctx
--     setContext :: c ctx ()

type CanInterp v e c = (
    ArrowStack v c,
    UseVal v c,
    Show v,
    IsString e,
    ArrowChoice c,
    ArrowFail e c,
    ArrowFix (c [AdminInstr v] [AdminInstr v]),
    ArrowFix (c [AdminInstr v] ()))

step :: CanInterp v e c => c [AdminInstr v] [AdminInstr v]
step = fix $ \step' -> proc instr -> case instr of

    (Plain instr':_) -> case instr' of
        ConstI32 v -> do
            val <- int32 -< v
            push -< val
            returnA -< []

        ConstF64 v -> do
            val <- float64 -< v
            push -< val
            returnA -< []

        Binary t op -> case op of
            Add -> do
                val1 <- pop -< t
                val2 <- pop -< t
                push <<< add -< (val1, val2)
                returnA -< []
        
        Unary t op -> case op of
            Neg -> do
                val <- pop -< t
                push <<< neg -< val
                returnA -< []
        
        Compare t op -> case op of
            Eq -> do
                val1 <- pop -< t
                val2 <- pop -< t
                push <<< eq -< (val1, val2)
                returnA -< []

            Lt -> do
                val1 <- pop -< t
                val2 <- pop -< t
                push <<< lt -< (val1, val2)
                returnA -< []
        
        -- | a Block reduces to a label
        Block resultTypes blockInstr -> do
            s <- getStack -< ()
            clear -< ()
            let label = Label resultTypes [] s (map Plain blockInstr)
            returnA -< [label]

        -- | a Loop reduces to a label with itself as a saved instruction
        Loop blockInstr -> do
            s <- getStack -< ()
            clear -< ()
            let label = Label [] [instr'] s (map Plain blockInstr)
            returnA -< [label]

        -- | Br reduces to a Breaking admin instr
        Br x -> do
            let break = Breaking x
            returnA -< [break]

    -- | A Label reduces to itself but with the inner instruction executed
    ((Label returnTypes labelInstr savedStack innerInstr):rest) -> case innerInstr of

        (Breaking 0:_) -> do
            -- restore the outer stack and pop return values
            returnValues <- popN -< returnTypes
            putStack -< returnValues ++ savedStack

            returnA -< (map Plain labelInstr)

        (Breaking n:_) -> do
            returnA -< (Breaking (n - 1)):rest
            
        [] -> do
            putStack -< savedStack
            returnA -< []

        innerInstr' -> do
            let (headInner, tailInner) = splitAt 1 innerInstr'
            innerInstr'' <- step' -< headInner
            let finalInner = innerInstr'' ++ tailInner
            returnA -< [Label returnTypes labelInstr savedStack finalInner]
    
    (Breaking n:_) -> fail -< fromString $ printf "Can't break %s levels up" (show n)
    
    [] -> returnA -< []
    
    v -> fail -< fromString $ "Not Implemented: " ++ (show v)

run :: CanInterp v e c => c [AdminInstr v] ()
run = fix $ \run' -> proc instr -> case instr of
    e:es -> do
        e' <- step -< [e]
        run' -< e' ++ es
    [] -> returnA -< ()