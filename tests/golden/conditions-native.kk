// Sub-plan S fixture: `conditions.kk` with every chain and step written as
// the branches and labels it stands for. The two link to the same bytes.

var mem   = $10
var flags = $11
var count = $12
var limit = $13

const LIMIT = 30

func either @a8 @i8 {
    a = flags
    bcs .body
    bne .end
.body:
    inx
.end:
    bcs .past
    bne .out
.past:
    y++
.out:
}

func range @a8 @i8 {
    a = mem
    cmp #10
    bcc .end1
    cmp #20
    bcs .end1
    iny
.end1:
    cmp #10
    bcc .body2
    cmp #20
    bne .end2
.body2:
    dey
.end2:
}

func counted @a8 @i8 {
    x = #4
.top1:
    inc mem
    dex
    bne .top1
    x = #4
.top2:
    inc mem
    dex
    bpl .top2
.top3:
    inc mem
    dec count
    bne .top3
    bcs .top3
    dex
    beq .end4
    inx
.end4:
}

func continued @a8 @i8 {
    x = #4
.top1:
    a = mem
    beq .test1
    inc mem
.test1:
    dex
    bne .top1
.top2:
    a = mem
    bcs .test2
    inc mem
    goto .end2
.test2:
    goto .top2
.end2:
    a = mem
    goto .end3
.end3:
}

func wide @a16 @i8 {
    a = mem
    cmp #10
    bcc .end
    cmp #$2000
    bcs .end
    iny
.end:
}

func ladder @a8 @i8 {
    a = mem
    a?4
    bne .not4
    x++
    goto .end
.not4:
    a?10
    beq .tens
    a?20
    bne .not20
.tens:
    y++
    goto .end
.not20:
    a?#LIMIT
    beq .limit
    a?LIMIT + 1
    bne .notlimit
.limit:
    x--
    goto .end
.notlimit:
    a?limit
    bne .other
    y--
    goto .end
.other:
    x = #0
.end:
    x?1
    bne .done
    mem++
.done:
}

inline bump { mem++ }

func unrolled @a8 @i8 {
    bump
    bump
    y++
    y++
    y++
}
