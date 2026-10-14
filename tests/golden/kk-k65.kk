// `.kk` is the third spelling of a K816 source, after K65's author. This
// half of the pair is written in the original K65 language and nothing
// else, so `--strict-k65` accepts it without a word; `kk-native.kk` is the
// same program in K816's native form, and `tests/driver.rs` builds both
// through `cl65` and requires the two images to be the same bytes.

var total = 0x80

data table { 1 2 3 4 }

// Sums the table into `total`.
main {
    a = 0
    x = 4
    {
        c-
        a + table-1,x
        x--
    } !=
    total = a
    return
}
