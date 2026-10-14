; A queue of slots.
.include "queue.inc"
.include "macros.inc"

.export queue_init, queue_find, slots, slot1, queue_head

.segment "BSS"
slots:
slot0:  .tag SLOT
slot1:  .tag SLOT
slot2:  .tag SLOT
queue_head: .res 2

.code
.a16
.i16
; Clears every slot.
queue_init:
        stz queue_head
        ldx #0
@clear:
        _a8
        stz slots+SLOT::owner,x
        _a16
        stz slots+SLOT::length,x
        txa
        clc
        adc #.sizeof(SLOT)
        tax
        cpx #SLOTS_NO * .sizeof(SLOT)
        bne @clear
        rts

; Finds the first idle slot; carry set when there is none.
queue_find:
        ldx #0
:       lda slots+SLOT::owner,x
        and #$00FF
        beq found
        txa
        clc
        adc #.sizeof(SLOT)
        tax
        cpx #SLOTS_NO * .sizeof(SLOT)
        bne :-
        sec
        rts
found:  clc
        rts
