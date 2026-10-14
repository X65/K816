// The other half of `kk-k65.kk`: the same program in K816's native form -
// raw mnemonics, a written label, width annotations, a `$` literal and an
// `import` - none of which K65 has, so `--strict-k65` refuses every one of
// them. The extension is not a dialect: a `.kk` takes full K816 exactly as
// a `.k816` does.

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
