# k816 VSCode Extension: Feature Proposals

## Current Features

- TextMate syntax highlighting for K65 assembly (65816)
- LSP integration (hovers, error reporting, resolveAddresses, code formatting)
- DAP debugging with breakpoint address resolution
- Build command (F7) with task provider (build, clean, run)
- LSP server status bar item
- File info status bar item (K65 + address at cursor)
- Server/debugger path configuration
- Server restart command

## Proposed Features

### Extension-Side (no LSP changes needed)

1. **Code Snippets** - Predefined snippets for common directives (`func`, `data`,
   `segment`, `var`, `const`, etc.), instruction templates with addressing mode
   placeholders, control flow skeletons.

2. **Problem Matcher for Build Output** - Parse `k816 build` error/warning output
   to populate the Problems panel with clickable error locations.

3. **Custom File Icon** - Contribute a file icon for `.k65` files so they're
   visually distinct in the explorer (needs icon asset).

4. **Chat Participant API** (`@k816`) - Register a chat participant so users can
   ask `@k816 how do I set 16-bit accumulator mode?`. Provides K65-specific
   documentation/instruction reference to the LLM.

5. **Language Model Tools** - Register tools that Copilot's agent mode can invoke,
   e.g. "look up 65816 instruction info", "query current memory map".

6. **Instruction Set Documentation as Context** - Ship a bundled reference file
   (65816 opcodes, K65 directives) that a chat participant or tool can feed to
   the LLM.

### LSP-Dependent Features (need k816 LSP server support)

7. **Go to Definition / Find References** - Navigate to label definitions, `func`
   declarations, `const`/`var` definitions. Find all references across files.

8. **Code Completion / IntelliSense** - Complete labels, directives, instruction
   mnemonics, register names. Show documentation in completion items.

9. **Document Symbols / Outline View** - Show functions, labels, segments, data
   blocks in the Outline panel. Enable breadcrumb navigation.

10. **Semantic Tokens** - Richer highlighting that understands context (distinguish
    local vs global labels, resolved vs unresolved symbols, addressing modes).

11. **Diagnostics Enhancements** - Real-time error squiggles for undefined labels,
    addressing mode mismatches, etc. (basic diagnostics already exist).

12. **Inlay Hints** - Show resolved address values inline next to labels, byte sizes
    of instructions, register width mode (`a8`/`a16`/`i8`/`i16`) context.

13. **Hover Enhancements** - Number base conversion (hex/decimal/binary), instruction
    documentation (affected flags, cycle counts, addressing modes).

14. **Rename Symbol** - Rename labels, functions, constants across all files.

15. **Code Lens** - Show reference counts above labels/functions, resolved memory
    addresses above `segment`/`func` declarations.

16. **Folding Ranges** - Smart folding for `func`...`return` blocks, `for`/`repeat`
    loops, `#if`...`#endif` preprocessor blocks.

17. **Signature Help** - Show parameter info for macros, built-in evaluator functions
    (`sin`, `cos`, `clamp`, etc.).

### Debug Enhancements (need emulator DAP support)

18. **Disassembly View** - Implement DAP `disassemble` request so VSCode's built-in
    Disassembly View works. Step through code at instruction level.

19. **Memory Inspector / Hex View** - Implement DAP `readMemory`/`writeMemory` so
    the Memory Inspector extension works. Browse/edit emulator memory.

20. **Register View Customization** - Rich register display showing CPU flags
    individually (N, V, M, X, D, I, Z, C, E), accumulator/index in 8/16-bit.

21. **Conditional Breakpoints** - Break on conditions like `A == $FF`, `X > $10`,
    specific memory values.

22. **Data Breakpoints / Watchpoints** - Break when a memory address is read/written.

23. **Inline Debug Values** - Show register values and resolved expressions inline
    in the editor during debugging.

### Advanced / Ambitious

24. **Memory Map Webview** - Custom webview showing memory layout with segments, code
    regions, data regions color-coded. Visual ROM/RAM usage.

25. **Sprite / Tile Editor** - Custom editor for viewing/editing image/tile data
    referenced by `image`, `tiles` directives in source.

26. **Peripheral / Hardware Register Browser** - Tree view showing target platform
    hardware registers (SNES PPU, SPC700, etc.). Quick insert into code.

27. **Binary Size / Resource Usage** - Show build output size, memory utilization
    percentages, warnings when approaching limits.

28. **Welcome / Getting Started Walkthrough** - Guided setup for new users (install
    k816 toolchain, configure paths, create first project).

## Priority Order

1. Code Snippets
2. Problem matcher
3. Document symbols / Outline (LSP)
4. Go to definition / References (LSP)
5. Code completion (LSP)
6. Diagnostics enhancements (LSP)
7. Disassembly view (DAP)
8. Memory inspector (DAP)
9. Inlay hints (LSP)
10. Hover enhancements (LSP)
