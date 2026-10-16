// A queue of slots.
// A slot of the queue, and what state one is in.

import "macros.kk"

abstract var SLOT[
    .owner  :byte
    .length :word
    .name   :byte [4]
]

const QUEUE_IDLE,
      QUEUE_BUSY,     // being served
      QUEUE_DONE = 9

const SLOTS_NO = 3

segment BSS
var slots[
    .owner  :byte
    .length :word
    .name   :byte [4]
] * 3
var queue_head:word

// Clears every slot.
segment CODE
func queue_init @a16 @i16 {
    stz queue_head
    ldx #0
    loop {
        @a8 {
            stz slots.owner,x
        }
        stz slots.length,x
        txa
        clc
        adc #SLOT:sizeof
        tax
    } while x?#SLOTS_NO * SLOT:sizeof !=
}

// Finds the first idle slot; carry set when there is none.
func queue_find @a16 @i16 {
    ldx #0
    loop {
        lda slots.owner:word,x
        and #$00FF
        beq .found
        txa
        clc
        adc #SLOT:sizeof
        tax
    } while x?#SLOTS_NO * SLOT:sizeof !=
    sec
    rts
.found:
    clc
}
