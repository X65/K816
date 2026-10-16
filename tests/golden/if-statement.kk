// Sub-plan Q fixture: K816's `if`, and a compare written in an `if` or a
// `while`. `if-statement.k65` beside it is the same program in K65's
// spellings, and the two link to the same bytes (`tests/compile.rs`).

var key   = $80
var mode  = $81
var count = $82

// Classifies `key` into `mode`: 1 below '0', 2 for a digit, 3 above.
func classify @a8 @i8 {
    a = key
    if a?#'0' < {
        mode = x = #1
    } else if a?#':' < {
        mode = x = #2
    } else {
        mode = x = #3
    }
}

// Counts `count` down to one, leaving early once `mode` is zero and
// skipping the store when the count reaches four.
func drain @a8 @i8 {
    x = count
    loop {
        a = mode
        if == break
        x--
        if x?#4 == repeat
        count = x
    } while x?#1 >=
}

// Bumps `count` when `mode` is non-zero and shares a bit with `key`.
func bump @a8 @i8 {
    a = mode
    if == goto .done
    if a&?key !z? {
        count++
    }
.done:
}
