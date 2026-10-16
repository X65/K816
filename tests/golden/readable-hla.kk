// K816's spellings of a loop, a flag test and the stack. `readable-hla.k65`
// beside it is the same program in K65's spellings, and the two compile to
// the same bytes (`tests/dialect.rs`).

var counter = $80
var flags = $81
const STATUS = $0012

// Counts the set bits of `flags` into `counter`, keeping `a` and `x`.
func count_bits @a8 @i8 {
    push a, x
    x = #8
    a = flags
    nocross loop {
        a >>
        if c? { counter++ }
        x--
    } while !=
    pull x, a
}

// Waits for bit 7 of `flags` to clear, or for the carry to say stop.
func wait_ready @a8 @i8 {
    loop {
        a&?flags
        if !n? break
        if c? break
    } always
    loop {
        a = flags
        if == break
        counter++
    } never
}

// Hands a routine two words on the stack, and drops them after.
func consume @a16 @i16 {
}

func call_with_frame @a16 @i16 {
    push STATUS, $0001
    consume
    pull x, x
}
