// Sub-plan S fixture: flag chains, and a step in a condition's slot.
// `conditions-native.kk` beside it is the same program with the branches
// and labels written out, and the two link to the same bytes
// (`tests/compile.rs`).

var mem   = $10
var flags = $11
var count = $12

// `|` before a block: every element but the last branches to the body, the
// last branches past it. `&` before a jump: every element but the last
// branches over the last.
func either @a8 @i8 {
    a = flags
    if c | z { x++ }
    if !c & !z goto .out
    y++
.out:
}

// A compare in a later element runs only when the branches before it fell
// through: a range test, and a two-value test.
func range @a8 @i8 {
    a = mem
    if a?#10 >= & a?#20 < { y++ }
    if a?#10 < | a?#20 == { y-- }
}

// A step in the slot tests `!=` unless a condition follows; the test is
// after the step, as `DEX`/`BNE` is.
func counted @a8 @i8 {
    x = #4
    loop { mem++ } while x--
    x = #4
    loop { mem++ } while x-- >=0
    loop { mem++ } while count-- | c
    if x-- { x++ }
}

// `continue` goes to the test, where the `while`'s step runs; `repeat` to the
// top. In an `always` loop `continue` reaches the back edge, in a `never`
// loop the end.
func continued @a8 @i8 {
    x = #4
    loop {
        a = mem
        if z continue
        mem++
    } while x--
    loop {
        a = mem
        if c continue
        mem++
        break
    } always
    loop {
        a = mem
        continue
    } never
}

// A later element's immediate is sized at the width in force there.
func wide @a16 @i8 {
    a = mem
    if a?#10 >= & a?#$2000 < { y++ }
}
