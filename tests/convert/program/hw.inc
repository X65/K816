; The registers of an imaginary machine, one layout every unit reads.
.struct IO
        .org    $D000
    status      .byte           ; ready bits
    data        .byte           ; one byte in or out
    vector      .addr           ; the interrupt handler
                .byte 4         ; reserved
.endstruct

IO_READY = %10000000
.define IO_TIMEOUT 200
