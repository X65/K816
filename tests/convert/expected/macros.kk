// Width helpers, as most ca65 sources write them.
// `_a8`: a width macro, written `@a8` wherever it was used
// `_a16`: a width macro, written `@a16` wherever it was used
// `_ai16`: a width macro, written `@a16 @i16` wherever it was used

// Shift left by a count.
inline shl (#count) {
    asl * count
}
