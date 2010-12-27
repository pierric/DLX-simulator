module Types(Byte, Half, Word, RegAddr, MemAddr, Opcode(..), Optype(..),
             opcode_lookup, opcode_lookupR,
             func_lookup, func_lookupR
            ) where

import Data.Int
import Data.Maybe
import qualified Data.Bimap as BM

type Byte = Int8
type Half = Int16
type Word = Int32
type RegAddr = Word
type MemAddr = Word

-- All available opcodes
data Opcode = Addi | Addiu | Subi | Subiu | Andi | Ori | Xori | Lhgi
            | Beqz | Bnez  | Jr   | Jalr
            | Lb | Lh | Lw | Lbu | Lhu | Sb | Sh | Sw

            | Add  | Addu  | Sub  | Subu  | And  | Or  | Xor  | Lhg
            
            | J    | Jal deriving (Show, Eq, Enum, Ord)

-- Load, Store, I-kind ALU, R-kind ALU, Jump
data Optype = ILoad | IStore | IALUi | IALUr | IJump

type_of opcode | opcode `elem` [Lb,Lh,Lw,Lbu,Lhu] = ILoad
               | opcode `elem` [Sb,Sh,Sw]         = IStore
               | opcode `elem` [Addi,Addiu,Subi, Subiu, Andi, Ori, Xori, Lhgi] = IALUi
               | opcode `elem` [Add, Addu, Sub, Subu, And, Or, Xor, Lhg]       = IALUr
               | opcode `elem` [Beqz, Bnez, Jr, Jalr, J, Jal] = IJump
               | otherwise = error "unknown opcode"

opcode_table =  BM.fromList $
                [(0x20, Lb), (0x21, Lh), (0x23, Lw), (0x24, Lbu), (0x25, Lhu), (0x28, Sb), (0x29, Sh), (0x2b, Sw)
                ,(0x08, Addi), (0x09, Addiu), (0x10, Subi), (0x11, Subiu), (0x12, Andi), (0x13, Ori), (0x14, Xori), (0x15, Lhgi)
                ,(0x04, Beqz), (0x05, Bnez), (0x16, Jr), (0x17, Jalr)
                ,(0x02, J), (0x03, Jal)]

func_table   = BM.fromList $
               [(0x20, Add), (0x21, Addu), (0x22, Sub), (0x23, Subu), (0x24, And), (0x25, Or), (0x26, Xor), (0x27, Lhg)]

opcode_lookup  = fromJust . flip BM.lookup  opcode_table
opcode_lookupR = fromJust . flip BM.lookupR opcode_table

func_lookup    = fromJust . flip BM.lookup  func_table
func_lookupR   = fromJust . flip BM.lookupR func_table