module Syntax where

import Data.Int

data WasmType
    = I64
    | F64
    deriving (Show, Eq)

data Instr
    = ConstI64 Int64
    | ConstF64 Double
    | Binary WasmType BinOpInstr
    | Unary WasmType UnOpInstr
    | Compare WasmType RelOpInstr
    deriving (Show, Eq)
    -- | Block [WasmType] [Instr]
    -- | Br Integer
    -- | BrIf Integer
    -- | If [WasmType] [Instr] [Instr]
    -- | Loop [WasmType] [Instr]

data BinOpInstr
    = Add
    deriving (Show, Eq)
  
data UnOpInstr
    = Neg
    deriving (Show, Eq)
  
data RelOpInstr
    = Eq
    | Lt
    deriving (Show, Eq)

type Stack a = [a]
    
data Code v = Code (Stack v) [AdminInstr v] deriving (Show, Eq)
-- data ModInst = EmptyInst deriving (Show, Eq)
-- data Frame = Frame ModInst Locals deriving (Show, Eq)
  
data AdminInstr v =
    Plain Instr
    | Trapping String
    | Returning (Stack v)
    | Breaking Integer (Stack v)
    | Label Integer [Instr] (Code v)
    | Frame (Code v)
    deriving (Show, Eq)