#ifndef __METACORE_SYMBOL_H__
#define __METACORE_SYMBOL_H__

/*
  I assert:
    1. Having Lisp/Prolog-style symbols in your language is fantastic.
    2. Having a global symbol table sucks. Giant global state. Blech.
    3. Interning strings for symbols feels heavy.

  So, can't we just represent symbols by words? (We'll assume that modern
  hardware is moving enough toward 64-bit that we can safely equate "word" and
  "64 bits".) Can't we just have some deterministic algorithm to convert back
  and forth? The answer is, of course, "yes"; the real question is not the
  tractability, but how to do this without driving the users of the language
  crazy---bearing in mind, of course, that this is intended as a somewhat
  low-level language for bootstrapping the system, and while it needs to be
  readable and writable, it does not need to be anyone's favorite language,
  unless a minimalism nerd, who won't mind a little bit of inconvenience anyhow.

  It is desirable for the algorithm to be straightforward since we are going for
  minimalism and speed. It should not be excessively difficult to implement, and
  it should run fast, since symbols are going to be bread and butter. The
  obvious thing to do is some human-friendly base-X encoding. Human-friendly,
  not in the sense that the programmer can easily convert to the decimal form in
  his mind (for that should not be important), but in that it provides you
  enough characters to form relatively pleasant identifiers. A base-X encoding,
  because that will be lossless. But what base should be used?

  The primary factors when choosing a base are:
    1. The number of characters available to the programmer in identifiers.
    2. The maximum length of identifiers that will fit into 64 bits.

  However, there is a further kink in that it would be nice to have the
  syntactic convenience available in most languages, that identifiers are
  distinguishable from numbers by the first character being non-numeric, despite
  numeric symbols being allowed.

  1    2    3    4    5    6    7    8    9    10   11   12   13
  2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁵ × 2⁴
  5    10   15   20   25   30   35   40   45   50   55   60   64

  Little-endian. Twelve 32-character digits; the thirteenth is 16 characters.

  An underscore can be used as syntactic sugar to skip specifying the rest of
  the base-32 digits, i.e. populate them with a's (zeros), to allow an easy way
  to name identifiers with numbers at the end, e.g. `x_0` and `x_1`, which will
  be equivalent to `xaaaaaaaaaaa0` and `xaaaaaaaaaaa1`, respectively.
*/

typedef unsigned long Symbol;

const Symbol compressChar(const char);
const Symbol compress13thChar(const char);
/**
 * Returns `!!!!!!!!!!!!!` on failure (which is therefore indistinguishable from
 * succeeding with the maximum symbol).
 */
const Symbol compressSymbol(const char* const);
const char decompressChar(const Symbol);
const char decompress13thChar(const Symbol);
char* decompressSymbol(const Symbol);

#endif
