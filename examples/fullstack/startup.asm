;*****************************************************************************
;
;                                startup.asm
;
;      Hand-written ca65 65816 startup stub and vector table for the
;              full-stack example; calls the K816 `main`
;
;
; Copyright (c) 2026 Tomasz Sterna (smokku)
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;
;*****************************************************************************

.setcpu "65816"

.import main

.segment "STARTUP"

; The CPU resets in 6502 emulation mode with IRQs masked by hardware; the
; stub switches to native mode and hands control to the K816 program.
reset:  sei
        clc
        xce                     ; leave emulation mode
        jsr     main
halt:   stp                     ; stop the processor until the next reset

; Stray interrupts return immediately.
irq:    rti

.segment "VECTORS"

; WDC 65C816 vector table, $FFE4-$FFFF (reserved slots are zero).
        .addr   irq             ; $FFE4 native COP
        .addr   irq             ; $FFE6 native BRK
        .addr   irq             ; $FFE8 native ABORT
        .addr   irq             ; $FFEA native NMI
        .addr   0               ; $FFEC reserved
        .addr   irq             ; $FFEE native IRQ
        .addr   0               ; $FFF0 reserved
        .addr   0               ; $FFF2 reserved
        .addr   irq             ; $FFF4 emulation COP
        .addr   0               ; $FFF6 reserved
        .addr   irq             ; $FFF8 emulation ABORT
        .addr   irq             ; $FFFA emulation NMI
        .addr   reset           ; $FFFC emulation RESET
        .addr   irq             ; $FFFE emulation IRQ/BRK
