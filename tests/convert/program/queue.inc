; A slot of the queue, and what state one is in.
.struct SLOT
    owner   .byte
    length  .word
    name    .byte 4
.endstruct

.enum
    QUEUE_IDLE
    QUEUE_BUSY      ; being served
    QUEUE_DONE = 9
.endenum

SLOTS_NO = 3
