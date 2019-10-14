module Syntax where

import Data.Int

data WasmType
    = I32
    | F64
    deriving (Show, Eq)

data Instr
    = ConstI32 Int32
    | ConstF64 Double
    | Binary WasmType BinOpInstr
    | Unary WasmType UnOpInstr
    | Compare WasmType RelOpInstr
    | Block [WasmType] [Instr]
    | Loop [Instr]
    | Br Int
    deriving (Show, Eq)
    -- | BrIf Integer
    -- | If [WasmType] [Instr] [Instr]

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

data AdminInstr v =
    Plain Instr
    | Trapping String
    | Returning (Stack v)
    | Breaking Int
    | Label [WasmType] [Instr] [v] [AdminInstr v]
    deriving (Show, Eq)