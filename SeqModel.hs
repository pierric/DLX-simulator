module SeqModel where

import Data.Bits
import ForSyDe.Shallow hiding (Memory)
import ForSyDe.Shallow.BitVector
import Types

import Debug.Trace

-- signal of `next_pc` in IF
next_pc :: Signal Word  -- current PC
        -> Signal Word  -- output of ALU
        -> Signal Bool     -- signal of braching
        -> Signal Word  -- next pc
next_pc = zipWith3SY (\pc alu bch -> if bch then alu else pc + 4)

select s n = bitVectorToInt' . selectV s 1 n . intToBitVector' 32

-- select rs1 field from IR
rs1, rs2 :: Signal Word -> Signal Word
rs1 = mapSY (select 6 5)
-- select rs2 field from IR
rs2 = mapSY (select 11 5)

type RegFile = Vector Word

register_file :: Vector Word                   -- initial register
              -> Signal RegAddr                -- 1st ReadAddress
              -> Signal RegAddr                -- 2nd ReadAddress
              -> Signal (RegAddr, Word, Bool)  -- (WriteAddress, WriteData, WriteEnable)
              -> Signal Bool                   -- Reset
              -> Signal (Word,Word, RegFile)   -- (ReadData, ReadData)
register_file empty rd1 rd2 wrt rst = let (r1,r2,reg) = unzip3SY $ reg_2r1w reg' rd1 rd2 wrt rst
                                          reg'        = delaySY empty reg
                                      in zip3SY r1 r2 reg
    where reg_2r1w = zipWith5SY (\reg ra1 ra2 (wa,wd,we) reset ->
                                     if reset then
                                         (0,0,empty)
                                     else if we && wa /= 0 then
                                              let reg' = replaceV' reg wa wd
                                                  forward r = if r == wa then wd else reg `atV` r
                                              in (forward ra1, forward ra2, reg')
                                          else
                                              (reg `atV` ra1, reg `atV` ra2, reg))

imm_sign_ext :: Signal Word  -- ir
             -> Signal Word  -- signed extension of imm
imm_sign_ext = mapSY (\ir -> let imm = selectV 16 1 16 (intToBitVector' 32 ir) in bitVectorToInt' (copyV 16 (headV imm) <+> imm))

                                    
opcode :: Signal Word       -- ir
       -> Signal Opcode     -- opcode
opcode = mapSY (\ir -> let opcode = select 0 6 ir
                           func   = select 26 6 ir
                       in if opcode == 0 then func_lookup func else opcode_lookup opcode)

branch :: Signal Word       -- val_a
       -> Signal Opcode     -- opcode
       -> Signal Bool       -- wether do branching
branch = zipWithSY (\val_a opcode -> case opcode of
                                       Beqz -> val_a == 0
                                       Bnez -> val_a /= 0
                                       Jr   -> True
                                       Jalr -> True
                                       J    -> True
                                       Jal  -> True
                                       _    -> False)

-- TODO: requires overflow detecting
alu_out :: Signal Word       -- RS1
        -> Signal Word       -- RS2
        -> Signal Word       -- next pc
        -> Signal Word       -- imm
        -> Signal Opcode     -- opcode
        -> Signal Word       -- alu output
alu_out = zipWith5SY (\val_a val_b next_pc imm opcode -> 
                          case opcode of
                            Addi  -> val_a + imm
                            Addiu -> val_a + imm
                            Subi  -> val_a - imm
                            Subiu -> val_a - imm 
                            Andi  -> val_a .&. imm
                            Ori   -> val_a .|. imm
                            Xori  -> val_a `xor` imm
                            Lhgi  -> imm `shiftL` 16
                            
                            Beqz  -> next_pc + imm
                            Bnez  -> next_pc + imm
                            Jr    -> val_a
                            Jalr  -> val_a               -- save next_pc in R[31]?

                            Lb    -> val_a + imm
                            Lh    -> val_a + imm 
                            Lw    -> val_a + imm
                            Lbu   -> val_a + imm
                            Lhu   -> val_a + imm
                            Sb    -> val_a + imm
                            Sh    -> val_a + imm
                            Sw    -> val_a + imm

                            Add   -> val_a + val_b
                            Addu  -> val_a + val_b
                            Sub   -> val_a - val_b
                            Subu  -> val_a - val_b
                            And   -> val_a .&. val_b
                            Or    -> val_a .|. val_b
                            Xor   -> val_a `xor` val_b
                            Lhg   -> val_b `shiftL` 16

                            J     -> next_pc + imm
                            Jal   -> next_pc + imm       -- save next_pc in R[31]?
                     )

type Memory  = Vector Half
data MemAcc  = SByte Byte | SHalf Half | SWord Word
             | LByte      | LHalf      | LWord
             | NoAcc
data MemOut  = OByte Byte | OHalf Half | OWord Word | ONull

memory :: Vector Half                                 -- initial memory
       -> Signal MemAddr                              -- w, read only
       -> Signal (MemAddr, MemAcc)                    -- b/h/w, read | write
       -> Signal Bool                                 -- reset
       -> (Signal Word, Signal (MemOut, Memory))      -- one word and one b/h/w output
memory empty rd rw reset = let (o1,o2, mem) = unzip3SY (mem_2r1w mem' rd rw reset)
                               mem'       = delaySY empty mem
                           in (o1, zipSY o2 mem)
    where mem_2r1w = zipWith4SY (\mem rd (rw,acc) reset ->
                                     if reset then 
                                         (0, ONull, empty)
                                     else let get_ir = get_word rd
                                          in case acc of
                                               LByte -> (get_ir mem, OByte (get_byte rw mem), mem)
                                               LHalf -> (get_ir mem, OHalf (get_half rw mem), mem)
                                               LWord -> (get_ir mem, OWord (get_word rw mem), mem)
                                               SByte b -> let mem' = set_byte rw b mem 
                                                          in (get_ir mem', ONull, mem')
                                               SHalf h -> let mem' = set_half rw h mem 
                                                          in (get_ir mem', ONull, mem')
                                               SWord w -> let mem' = set_word rw w mem
                                                          in (get_ir mem', ONull, mem')
                                               NoAcc   -> (get_ir mem, ONull, mem))
              where get_byte addr memo = let (wrd,ofs) = divMod addr 2
                                             half      = memo `atV` wrd
                                         in fromIntegral (case ofs of 0 -> (shiftR half 8); 1 -> (half .&. 0xff))
                    get_half addr memo = let (wrd,ofs) = divMod addr 2
                                         in if ofs == 0 then
                                                memo `atV` wrd
                                            else 
                                                 error "unaligned halfword access"
                    get_word addr memo = let (wrd,ofs) = divMod addr 2
                                             h = fromIntegral (memo `atV` wrd) :: Word
                                             l = fromIntegral (memo `atV` (wrd + 1)) :: Word
                                         in if ofs == 0 && even wrd then 
                                                shiftL h 16 .|. (l .&. 0xffff)
                                            else 
                                                error "unailgned word access"
                    set_byte addr val memo = let (wrd,ofs) = divMod addr 2
                                                 half      = memo `atV` wrd
                                                 upd       = case ofs of 
                                                               0 -> (half .&. 0xff00) .|. fromIntegral val
                                                               1 -> (half .&. 0x00ff) .|. shiftL (fromIntegral val) 8
                                             in replaceV' memo addr upd
                    set_half addr val memo = let (wrd, ofs) = divMod addr 2
                                             in if ofs == 0 then
                                                    replaceV' memo wrd val
                                                else 
                                                    error "unailgned halfword access"
                    set_word addr val memo = let (wrd, ofs) = divMod addr 2
                                                 h = fromIntegral (shiftR (val .&. 0xffff0000) 16) :: Half
                                                 l = fromIntegral (val .&. 0x0000ffff) :: Half
                                             in if ofs == 0 && even wrd then 
                                                    let memo' = replaceV' memo wrd h
                                                    in replaceV' memo' (wrd + 1) l
                                                else 
                                                    error "unanligned word access"

mem_access :: Signal Opcode
           -> Signal Word
           -> Signal MemAcc
mem_access = zipWithSY (\opcode b -> 
                            case opcode of
                              Lb  -> LByte
                              Lh  -> LHalf
                              Lw  -> LWord
                              Lbu -> LByte
                              Lhu -> LHalf
                              Sb  -> SByte (fromIntegral (b .&. 0xff))
                              Sh  -> SHalf (fromIntegral (b .&. 0xffff))
                              Sw  -> SWord b
                              _   -> NoAcc
                       )
                                        

write_back :: Signal Word
           -> Signal Opcode
           -> Signal Word
           -> Signal MemOut
           -> Signal (RegAddr, Word, Bool)
write_back = zipWith4SY (\ir opcode alu_out lmd -> 
                             case type_of opcode of
                               ILoad -> let lmd_out = case lmd of 
                                                        OByte b -> fromIntegral b
                                                        OHalf h -> fromIntegral h
                                                        OWord w -> fromIntegral w
                                        in (select 11 5 ir, lmd_out, True)
                               IALUi -> (select 11 5 ir, alu_out, True)
                               IALUr -> (select 16 5 ir, alu_out, True)
                               _     -> (undefined, undefined, False))

-- input of the machine is initial memory image
-- output of the machine is the trace of (pc, register file, memory)
-- machine :: Snapshot -> Signal Snapshot
machine (initPC,initReg,initMem) =
    let reset   = infiniteS id False

        pc      = delaySY initPC npc
        npc     = next_pc pc alu brch

        -- fetch ir and access memory
        (ir,mem_out) = memory initMem pc (delaySY (0, NoAcc) $ zipSY alu (mem_access op b)) reset
        -- decode
        op      = opcode ir
        imm     = imm_sign_ext ir
        -- access register
        (a,b,reg) = unzip3SY $ register_file initReg (rs1 ir) (rs2 ir) (delaySY (undefined, undefined, False) wb) reset
        -- execution
        alu     = alu_out a b pc imm op
        -- braching
        brch    = branch a op
        -- write back
        (lmd,mem)  = unzipSY mem_out
        wb      = write_back ir op alu lmd
    in zip3SY pc reg mem

gen_snapshot :: [(MemAddr, Byte)] -> Snapshot
gen_snapshot image = (0, copyV 32 (0 :: Word), mem)
    where mem  = foldl update (copyV 65536 (0 :: Half)) image
          update m (addr, byte) = let (ha,ho) = divMod (fromIntegral addr) 2
                                      v0 = intToBitVector' 16 (m `atV` ha)
                                      v1 = intToBitVector' 8  byte
                                      v  = bitVectorToInt' (if ho == 0 then v1 <+> dropV 8 v0 else takeV 8 v0 <+> v1)
                                  in replaceV m ha v

zipWith5SY :: (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
zipWith5SY _ NullS   _       _       _      _     = NullS
zipWith5SY _ _       NullS   _       _      _     = NullS
zipWith5SY _ _       _       NullS   _      _     = NullS
zipWith5SY _ _       _       _       NullS  _     = NullS
zipWith5SY _ _       _       _       _      NullS = NullS
zipWith5SY f (v:-vs) (w:-ws) (x:-xs) (y:-ys) (z:-zs) = f v w x y z :- (zipWith5SY f vs ws xs ys zs)

replaceV' v n = replaceV v (fromIntegral n)
intToBitVector' b n = intToBitVector b (fromIntegral n)
bitVectorToInt' v | l <= 32 = fromIntegral $ bitVectorToInt $ copyV (32 - l) 0 <+> v
                  where l = lengthV v


t0 image = let (_,_,m) = gen_snapshot image
               (ir, _) = memory m (infiniteS (+4) 0) undefined (infiniteS id False)
           in ir
t1 image = machine $ gen_snapshot image
                 