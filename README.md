# DLX-simulator
DLX simulator written with ForSyDe in Haskell

```
example = do rec 
    _pos 0x00
    addi r1 r1 2
    xor  r1 r1 r1
    bnez r0 (_dl finish)
    jr r0
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
```
