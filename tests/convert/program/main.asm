; The program: reset, the main loop, and the interrupt it waits for.
.feature string_escapes
.import queue_init, queue_find, slot1, queue_head

.include "hw.inc"
.include "macros.inc"
.include "queue.inc"

.macpack generic

.zeropage
pointer: .res 2

.segment "VECTORS"
        .addr irq_handler, reset

.segment "RODATA"
banner: .asciiz "Ready\r\n"
handlers:
        .addr idle, busy, done

.segment "DISPLAY"
.org $4000
screen:
.reloc

.code
.a8
.i8
reset:
        sei
        clc
        xce
        _ai16
        jsr queue_init
        lda #slot1+SLOT::name
        sta pointer
        _a8
        ldx #0
@print:
        lda banner,x
        beq @done
        sta IO::data
        inx
        bra @print
@done:
        _a16
        jsr queue_find
        bcs reset_wait
        _a8
        lda #QUEUE_BUSY
        sta slot1+SLOT::owner
        _a16
reset_wait:
        lda IO::status
        and #IO_READY
        beq reset_wait
        add #2
        shl 2
        sta screen
        ldx queue_head
        jmp (handlers,x)

idle:
        rts
busy:
        lda #QUEUE_DONE
        rts
done:
        jmp idle

irq_handler:
        _ai16
        pha
        _a8
        lda IO::status
        sta IO::data
        _a16
        pla
        rti
