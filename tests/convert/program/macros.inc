; Width helpers, as most ca65 sources write them.
.macro _a8
        sep #%00100000
        .a8
.endmacro
.macro _a16
        rep #%00100000
        .a16
.endmacro
.macro _ai16
        rep #%00110000
        .a16
        .i16
.endmacro

; Shift left by a count.
.macro shl count
        .repeat count
        asl
        .endrep
.endmacro
