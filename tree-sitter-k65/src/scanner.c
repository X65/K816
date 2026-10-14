/*****************************************************************************/
/*                                                                           */
/*                                 scanner.c                                 */
/*                                                                           */
/*          External scanner for the K65/K816 Tree-sitter grammar            */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

/*
 * One token the grammar cannot express by itself: "a line ends here".
 *
 * K65 is newline-insensitive, and its grammar says so by making every
 * whitespace character an extra. Raw mnemonics are K816's, and a mnemonic
 * may stand alone or take an operand, so with newlines invisible `pla`
 * followed by `task_init` on the next line is `pla task_init` -- an
 * instruction with an operand -- and the bare call is gone.
 *
 * `_line_end` is valid only after a raw mnemonic and after its operand, and
 * only the reading that ends the instruction there accepts it. The scanner
 * produces it where a line break comes before the next token, so the
 * reading that would carry on across the break has nothing to shift and
 * dies; where there is no break the scanner produces nothing, and the
 * parse is exactly what it was. A line comment ends the line as a newline
 * does. The token is zero-width.
 */

#include "tree_sitter/parser.h"

#include <stdbool.h>

enum TokenType {
    LINE_END,
    /* Never produced. Tree-sitter marks every external token valid while it
     * recovers from an error, and a scanner that answers then decides the
     * recovery; this one names that state so it can stay out of it. */
    ERROR_SENTINEL,
};

void *tree_sitter_k65_external_scanner_create(void) { return NULL; }

void tree_sitter_k65_external_scanner_destroy(void *payload) { (void)payload; }

unsigned tree_sitter_k65_external_scanner_serialize(void *payload, char *buffer)
{
    (void)payload;
    (void)buffer;
    return 0;
}

void tree_sitter_k65_external_scanner_deserialize(void *payload, const char *buffer,
                                                  unsigned length)
{
    (void)payload;
    (void)buffer;
    (void)length;
}

bool tree_sitter_k65_external_scanner_scan(void *payload, TSLexer *lexer,
                                           const bool *valid_symbols)
{
    (void)payload;
    if (valid_symbols[ERROR_SENTINEL] || !valid_symbols[LINE_END]) {
        return false;
    }
    lexer->result_symbol = LINE_END;
    for (;;) {
        switch (lexer->lookahead) {
        case ' ':
        case '\t':
        case '\f':
        case '\v':
            lexer->advance(lexer, true);
            break;
        case '\n':
        case '\r':
            lexer->mark_end(lexer);
            return true;
        case '/':
            /* A line comment runs to the end of the line, so the next token
             * is on another one. A block comment may not; the extras take
             * it, and the scanner is asked again after it. */
            lexer->mark_end(lexer);
            lexer->advance(lexer, false);
            return lexer->lookahead == '/';
        default:
            if (lexer->eof(lexer)) {
                lexer->mark_end(lexer);
                return true;
            }
            return false;
        }
    }
}
