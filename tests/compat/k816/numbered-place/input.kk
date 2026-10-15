// Numbers where HLA reaches memory, in a K816 source.

var flags = $10

func work @a8 @i8 {
    a = $10,x
    $10 = a
    $10++
    a = flags,x
    a = $10
    a = 3,s
}
