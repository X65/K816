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
    /* `loop`, `push` and `pull`, as keywords. They begin statements, and a
     * statement may begin wherever a mnemonic's operand may — `bne loop`
     * is either — so a keyword the internal lexer knew would take every
     * label of that name (keyword extraction prefers the keyword wherever
     * both are valid). Each is a keyword only when what follows it on the
     * same line says so: `loop` before `{`, `push` and `pull` before an
     * operand. Anything else is left to the internal lexer, which reads an
     * identifier. */
    LOOP_KEYWORD,
    PUSH_KEYWORD,
    PULL_KEYWORD,
};

static bool is_word_char(int32_t c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

static bool is_horizontal_space(int32_t c)
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
}

/* Consumes `rest` if it is what comes next, and reports whether it was. */
static bool consume(TSLexer *lexer, const char *rest)
{
    for (; *rest; rest++) {
        if (lexer->lookahead != *rest) {
            return false;
        }
        lexer->advance(lexer, false);
    }
    return true;
}

/* What may follow `push` on its line: a register, a number, a name, `#`, a
 * parenthesis, an address-byte operator, a character, or a sign. */
static bool starts_push_item(int32_t c)
{
    return is_word_char(c) || c == '$' || c == '%' || c == '#' || c == '(' || c == '&' ||
           c == '\'' || c == '-' || c == '~' || c == '[';
}

/* `loop`, `push` or `pull` at the lexer, if what follows makes it one. */
static bool keyword(TSLexer *lexer, const bool *valid_symbols)
{
    enum TokenType word;
    switch (lexer->lookahead) {
    case 'l':
        if (!valid_symbols[LOOP_KEYWORD]) {
            return false;
        }
        lexer->advance(lexer, false);
        if (!consume(lexer, "oop") || is_word_char(lexer->lookahead)) {
            return false;
        }
        lexer->mark_end(lexer);
        while (is_horizontal_space(lexer->lookahead)) {
            lexer->advance(lexer, false);
        }
        if (lexer->lookahead != '{') {
            return false;
        }
        lexer->result_symbol = LOOP_KEYWORD;
        return true;
    case 'p':
        if (!valid_symbols[PUSH_KEYWORD] && !valid_symbols[PULL_KEYWORD]) {
            return false;
        }
        lexer->advance(lexer, false);
        if (!consume(lexer, "u")) {
            return false;
        }
        if (lexer->lookahead == 's') {
            lexer->advance(lexer, false);
            word = PUSH_KEYWORD;
            if (!consume(lexer, "h")) {
                return false;
            }
        } else if (lexer->lookahead == 'l') {
            lexer->advance(lexer, false);
            word = PULL_KEYWORD;
            if (!consume(lexer, "l")) {
                return false;
            }
        } else {
            return false;
        }
        if (!valid_symbols[word] || is_word_char(lexer->lookahead)) {
            return false;
        }
        lexer->mark_end(lexer);
        while (is_horizontal_space(lexer->lookahead)) {
            lexer->advance(lexer, false);
        }
        /* `pull` takes registers, which are letters; `push` anything it
         * can push. */
        if (word == PULL_KEYWORD ? !is_word_char(lexer->lookahead) ||
                                       (lexer->lookahead >= '0' && lexer->lookahead <= '9')
                                 : !starts_push_item(lexer->lookahead)) {
            return false;
        }
        lexer->result_symbol = word;
        return true;
    default:
        return false;
    }
}

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
    if (valid_symbols[ERROR_SENTINEL]) {
        return false;
    }
    bool keywords =
        valid_symbols[LOOP_KEYWORD] || valid_symbols[PUSH_KEYWORD] || valid_symbols[PULL_KEYWORD];
    if (!valid_symbols[LINE_END]) {
        if (!keywords) {
            return false;
        }
        while (is_horizontal_space(lexer->lookahead) || lexer->lookahead == '\n' ||
               lexer->lookahead == '\r') {
            lexer->advance(lexer, true);
        }
        return keyword(lexer, valid_symbols);
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
            /* The line goes on, so this is no line end; it may still be
             * one of the keywords, after a mnemonic that stands alone. */
            return keywords && keyword(lexer, valid_symbols);
        }
    }
}
