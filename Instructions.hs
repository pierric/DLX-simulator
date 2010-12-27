{-# LANGUAGE DoRec,PackageImports,TemplateHaskell #-}
module Instructions where

import Data.Char
import Data.List(sortBy)
import Data.Ord.HT
import Data.List.Split
import Types
import "monads-tf" Control.Monad.State
import Data.Accessor
import Data.Accessor.Template
import qualified Data.Accessor.Monad.TF.State as SA
import Language.Haskell.TH

import Bits

type LablId = Int

data Align = A1 | A2 | A4 | A8 deriving Show

data InstDef = IType { _opcode_ :: Opcode, _rs1_ :: Reg, _rd_  :: Reg, _imm_ :: Either Word LablId }
             | RType { _opcode_ :: Opcode, _rs1_ :: Reg, _rs2_ :: Reg, _rd_  :: Reg, _sa_ :: Word }
             | JType { _opcode_ :: Opcode, _imm_ :: Either Word LablId } deriving Show

data DataDef = BData Byte
             | HData Half
             | WData Word
             | SData String deriving Show

newtype Reg = Reg {_reg_id_ :: Int} deriving Show

data S  = S{ _labls_        :: [(LablId,  MemAddr)]
           , _instr_        :: [(InstDef, MemAddr)]
           , _datum_        :: [(DataDef, MemAddr)]
           , _next_label_   :: LablId
           , _position_     :: MemAddr
           , _alignment_    :: Align
           } deriving Show
$(deriveAccessors ''S)
$(deriveAccessors ''InstDef)
$(deriveAccessors ''Reg)

type IM = State S

cons_inst inst = SA.modify _instr (inst:)
cons_labl labl = SA.modify _labls (labl:)
cons_data dat  = SA.modify _datum (dat:)
get_align      = SA.get _alignment
set_align      = SA.set _alignment
get_next_lid   = SA.get _next_label
set_next_lid   = SA.set _next_label
get_position   = SA.get _position
set_position   = SA.set _position
get_aligned_position = do
  pos <- get_position
  ali <- get_align >>= (\a -> return $ case a of A1 -> 1; A2 -> 2; A4 -> 4;  A8 -> 8)
  let (d,m) = divMod pos ali
  return $ if m == 0 then d*ali else d*ali + ali

make_labl :: IM LablId
make_labl = do
  lid <- get_next_lid
  pos <- get_position
  cons_labl (lid, pos)
  set_next_lid (lid+1)
  return lid

make_inst :: InstDef -> IM ()
make_inst inst = do
  set_align A4
  pos <- get_aligned_position
  cons_inst (inst, pos)
  set_position (pos+4)

make_data :: DataDef -> IM ()
make_data dat  = do
  pos <- get_aligned_position
  cons_data (dat, pos)
  set_position (pos+data_size dat)
    where data_size (BData _) = 1
          data_size (HData _) = 2
          data_size (WData _) = 4
          data_size (SData s) = fromIntegral (length s)

_byte   = make_data . BData
_half   = make_data . HData
_word   = make_data . WData
_string = make_data . SData
_ii     = IndirectI
_il     = IndirectL
_di     = DirectI
_dl     = DirectL
_align  = set_align
_pos    = set_position

data Addressing = IndirectI Reg Word         -- indirect access
                | IndirectL Reg LablId       -- labeled indirect access
                | DirectI Word               -- direct access
                | DirectL LablId             -- labeld direct access

make_load opcode r (IndirectI a i) = make_inst (IType opcode r a (Left i))
make_load opcode r (IndirectL a l) = make_inst (IType opcode r a (Right l))
lb  = make_load Lb
lbu = make_load Lbu
lh  = make_load Lh
lhu = make_load Lhu
lw  = make_load Lw

make_store opcode (IndirectI a i) r = make_inst (IType opcode a r (Left i))
make_store opcode (IndirectL a l) r = make_inst (IType opcode a r (Right l))
sb  = make_store Sb
sh  = make_store Sh
sw  = make_store Sw


make_iarith opcode rs rd imm = make_inst (IType opcode rs rd (Left imm))
addi  = make_iarith Addi
addiu = make_iarith Addiu
subi  = make_iarith Subi
subiu = make_iarith Subiu
andi  = make_iarith Andi
ori   = make_iarith Ori
xori  = make_iarith Xori
lhgi  = make_iarith Lhgi

make_control opcode rs (DirectI i) = make_inst (IType opcode rs (Reg 0) (Left i))
make_control opcode rs (DirectL l) = make_inst (IType opcode rs (Reg 0) (Right l))
beqz  = make_control Beqz
bnez  = make_control Bnez
jr rs = make_control Jr rs (DirectI 0)
jalr  = make_control Jalr

make_rarith opcode rs1 rs2 rd = make_inst (RType opcode rs1 rs2 rd 0)
add  = make_rarith Add
addu = make_rarith Addu
sub  = make_rarith Sub
subu = make_rarith Subu
and  = make_rarith And
or   = make_rarith Or
xor  = make_rarith Xor
lhg  = make_rarith Lhg

-- declare registers
-- r1 = Reg 1
-- r2 = Reg 2
-- ...
-- r31 = Reg 31
$(mapM (\r -> let n = mkName ("r" ++ show r) in funD n [clause [] (normalB ([|Reg r|])) []]) [0..31])

dump :: IM () -> String
dump = undefined

assemble im = let inst_part = concatMap (inst_to_byte . check_consistent . resolve_labels) instr
                  data_part = concatMap data_to_byte datum
              in inst_part ++ data_part -- sortBy (\a b -> compare (snd a) (snd b)) (inst_part ++ data_part)
    where st   = execState im (S [] [] [] 0 0 A4)
          labls = reverse (st ^. _labls)
          instr = reverse (st ^. _instr)
          datum = reverse (st ^. _datum)
          
          resolve_labels (inst, this) =
              case inst of
                IType op rs rd (Right lid) -> 
                    case lookup lid labls of
                      Nothing -> error "undefined label"
                      Just target -> (IType op rs rd (Left (target - this)), this)
                JType op (Right lid) ->
                    case lookup lid labls of 
                      Nothing -> error "undefined label"
                      Just target -> (JType op (Left (target - this)), this)
                _ -> (inst, this)

          check_consistent i@(inst,_) = 
              case inst of
                IType _ _ _ (Left imm) | inRange (-32768,32767) imm        -> i
                RType _ _ _ _ sa       | inRange (-16,15) sa               -> i
                JType _ (Left imm)     | inRange (-33554432, 33554431) imm -> i
                _ -> error "inconsistent instrument"

          inst_to_byte :: (InstDef, MemAddr) -> [(Byte,MemAddr)]
          inst_to_byte (inst, this) = 
              let bits = case inst of
                           IType op rs rd (Left imm) -> op_to_bits op ++ int_to_bits 5 (rs ^. _reg_id)  ++ int_to_bits 5 (rd ^. _reg_id) ++ int_to_bits 16 imm
                           RType op rs1 rs2 rd sa    -> let func = func_to_bits op 
                                                        in int_to_bits 6 0 ++ concatMap (int_to_bits 5 . (^. _reg_id)) [rs1,rs2,rd] ++ int_to_bits 5 sa ++ func
                           JType op (Left imm)       -> op_to_bits op ++ int_to_bits 26 imm
              in zip (map bits_to_int (chunk 8 bits)) (enumFrom this)
              where
                op_to_bits   = int_to_bits 6 . opcode_lookupR
                func_to_bits = int_to_bits 6 . func_lookupR

          data_to_byte :: (DataDef, MemAddr) -> [(Byte, MemAddr)]
          data_to_byte (dat, this) = 
              case dat of
                BData byte -> [(byte, this)]
                HData half -> zip (map bits_to_int $ chunk 8 (int_to_bits 16 half)) (enumFrom this)
                WData word -> zip (map bits_to_int $ chunk 8 (int_to_bits 32 word)) (enumFrom this)
                SData string -> let convert c = let o = ord c
                                                in if inRange (0,127) o then
                                                       fromIntegral o :: Byte
                                                   else
                                                       error "char outside ASCII is not supported"
                                in zip (map convert string) (enumFrom this)

test = do rec _pos 0x00
              addi r1 r1 2
              xor  r1 r1 r1
              beqz r0 (_dl finish)
              jr r2
              finish <- make_labl
              
              _pos 0x100
              _word 0x1234
              _align A2
              _half 0x12
              _align A4
              _word 0x1234
              _string "ooxx,love"
              _word 0x1234
          return ()