// The other half of `kk-k65.k65`: the same program in K816's native form -
// raw mnemonics, a written label, width annotations, a `$` literal and an
// `import` - none of which K65 has, so `--strict-k65` refuses every one of
// them. A `.kk` is K816 exactly as a `.k816` is; K65's own spellings live in
// `.k65` sources.

import "kk-lib.k816"

data table { 1 2 3 4 }

main @a8 @i8 {
    lda #0
    ldx #4
loop:
    clc
    adc table-1,x
    dex
    bne loop
    sta total
    rts
}
