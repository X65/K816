// The registers of an imaginary machine, one layout every unit reads.
var IO[
    .status       :byte         // ready bits
    .data         :byte         // one byte in or out
    .vector       :word         // the interrupt handler
    ._reserved_4  :byte [4]     // reserved
] = $D000

const IO_READY = %10000000
const IO_TIMEOUT = 200
