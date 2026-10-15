// The program: reset, the main loop, and the interrupt it waits for.

import "hw.kk"
import "macros.kk"
import "queue.kk"
import "generic.kk"

var dp pointer:word

segment VECTORS
data {
    word irq_handler reset
}

segment RODATA
data banner {
    "Ready\r\n" 0
}
data handlers {
    word idle busy done
}

var screen = $4000

segment CODE
naked reset {
    sei
    clc
    xce
    @a16 @i16
    queue_init
    lda #slots[1].name
    sta pointer
    @a8
    ldx #0
    loop {
        lda banner,x
        == break
        sta IO.data
        inx
    } always
    @a16
    queue_find
    bcs .reset_wait
    @a8 {
        lda #QUEUE_BUSY
        sta slots[1].owner
    }
.reset_wait:
    lda IO.status:word
    and #IO_READY
    beq .reset_wait
    add #2
    shl #2
    sta screen
    ldx queue_head
    jmp (handlers,x)
}

func idle {
}
func busy @a16 {
    lda #QUEUE_DONE
}
naked done {
    jmp idle
}

naked irq_handler {
    @a16 @i16
    pha
    @a8 {
        lda IO.status
        sta IO.data
    }
    pla
    rti
}
