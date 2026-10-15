// A constant's size (sub-plan P).
//
// `EUNKNOWN` has none, and each use gives it the width that use needs.
// `ERROR`, `PAGE` and `BANK` declare one, which is the range the value lies
// in and the size it is exported at - and their uses still take their own
// width, because the value stays the value.

const EUNKNOWN = -1
const ERROR:byte = -2
const PAGE:word = $1234
const BANK:far = -3

func main @a8 @i8 {
    lda #EUNKNOWN               // $FF
    sta codes
    lda #ERROR                  // $FE
    sta codes
    @a16 {
        lda #EUNKNOWN           // $FFFF
        sta codes
        lda #ERROR              // $FFFE, not $00FE
        sta codes
    }
    push ERROR:word             // PEA $FFFE: a `:byte` is pushed through its view
}

data codes {
    EUNKNOWN ERROR              // $FF $FE
    word EUNKNOWN PAGE          // $FFFF $1234
    far BANK EUNKNOWN           // $FFFFFD $FFFFFF
}
