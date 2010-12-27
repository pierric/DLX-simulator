module Bits where

import Data.List

data Bit = L | H deriving (Show, Eq, Enum)

-- 转换成Bit序列, 低位到高位
w2b :: Integral i => i -> [Bit]
w2b x = let (q,r) = quotRem x 2 in toEnum (fromIntegral r) : w2b q

b2w :: Integral i => [Bit] -> i
b2w []     = 0
b2w (b:bs) = fromIntegral (fromEnum b) + b2w bs * 2

int_to_bits sz n = reverse $ take sz $ w2b n

bits_to_int bs   = b2w $ reverse bs
