#include <assert.h>
#include <gc.h>
#include "Symbol.h"

#define BASE 32
#define MAX_SYMBOL_LENGTH 13
#define DIGIT_13_BASE 16
#define MAX_ULONG 18446744073709551615UL

const Symbol compressChar(const char symChar) {
  switch (symChar) {
  case 'a': return  0;
  case 'b': return  1;
  case 'c': return  2;
  case 'd': return  3;
  case 'e': return  4;
  case 'f': return  5;
  case 'g': return  6;
  case 'h': return  7;
  case 'i': return  8;
  case 'j': return  9;
  case 'k': return 10;
  case 'l': return 11;
  case 'm': return 12;
  case 'n': return 13;
  case 'o': return 14;
  case 'p': return 15;
  case 'q': return 16;
  case 'r': return 17;
  case 's': return 18;
  case 't': return 19;
  case 'u': return 20;
  case 'v': return 21;
  case 'w': return 22;
  case 'x': return 23;
  case 'y': return 24;
  case 'z': return 25;
  case '-': return 26;
  case '+': return 27;
  case '/': return 28;
  case '*': return 29;
  case '?': return 30;
  case '!': return 31;
  default:  return BASE;
  }
}

const Symbol compress13thChar(const char symChar) {
  switch (symChar) {
  case '0': return  0;
  case '1': return  1;
  case '2': return  2;
  case '3': return  3;
  case '4': return  4;
  case '5': return  5;
  case '6': return  6;
  case '7': return  7;
  case '8': return  8;
  case '9': return  9;
  case '-': return 10;
  case '+': return 11;
  case '/': return 12;
  case '*': return 13;
  case '?': return 14;
  case '!': return 15;
  default:  return DIGIT_13_BASE;
  }
}

const Symbol compressSymbol(const char* const symbol) {
  Symbol acc = 0; // `acc` is our accumulator
  unsigned long c = 0; // `c` is our loop counter
  unsigned long i = 1; // `i` is our digit power counter
  unsigned long base = BASE;
  // Compression function is different for the 13th digit
  const unsigned long (*compress)(const char) = compressChar;
  // Iterate through the c-string.
  while (*(c+symbol)) {
    if (compress == compress13thChar) { // Prevent overflows
      return MAX_ULONG;
    } else if (c == MAX_SYMBOL_LENGTH - 1) {
      compress = compress13thChar;
    }
    if (compress != compress13thChar && *(c+symbol) == '_') {
      for (unsigned long j = c; j < MAX_SYMBOL_LENGTH - 1; j++) {
        // Empower the digit counter
        i *= base;
      }
      // Increment the loop counter
      c++;
      compress = compress13thChar;
      base = DIGIT_13_BASE;
    }
    // Convert a single symbol character to its numeric value
    Symbol x = compress(*(c+symbol));
    // The character is not a valid symbol character
    if (x >= base) return MAX_ULONG;
    // Accumulate
    acc += i * x;
    // Increment the loop counter
    c++;
    // Empower the digit counter
    i *= base;
    if (c == MAX_SYMBOL_LENGTH - 1) {
      base = DIGIT_13_BASE;
    }
  }
  // If we had a problem we would have short-circuited before now
  return acc;
}

const char decompressChar(const Symbol symChar) {
  switch (symChar) {
  case  0: return 'a';
  case  1: return 'b';
  case  2: return 'c';
  case  3: return 'd';
  case  4: return 'e';
  case  5: return 'f';
  case  6: return 'g';
  case  7: return 'h';
  case  8: return 'i';
  case  9: return 'j';
  case 10: return 'k';
  case 11: return 'l';
  case 12: return 'm';
  case 13: return 'n';
  case 14: return 'o';
  case 15: return 'p';
  case 16: return 'q';
  case 17: return 'r';
  case 18: return 's';
  case 19: return 't';
  case 20: return 'u';
  case 21: return 'v';
  case 22: return 'w';
  case 23: return 'x';
  case 24: return 'y';
  case 25: return 'z';
  case 26: return '-';
  case 27: return '+';
  case 28: return '/';
  case 29: return '*';
  case 30: return '?';
  case 31: return '!'; // BASE-32
  default: return '\0';
  }
}

const char decompress13thChar(const Symbol symChar) {
  switch (symChar) {
  case  0: return '0';
  case  1: return '1';
  case  2: return '2';
  case  3: return '3';
  case  4: return '4';
  case  5: return '5';
  case  6: return '6';
  case  7: return '7';
  case  8: return '8';
  case  9: return '9';
  case 10: return '-';
  case 11: return '+';
  case 12: return '/';
  case 13: return '*';
  case 14: return '?';
  case 15: return '!'; // BASE-16
  default: return '\0';
  }
}

char* decompressSymbol(const Symbol x) {
  Symbol rem = x;
  char* symbol = (char*)GC_MALLOC(sizeof(char));
  unsigned char i = 0;
  unsigned char base = BASE;
  unsigned char zeros = 0;
  const char (*decompress)(const unsigned long) = decompressChar;
  do {
    if (i == MAX_SYMBOL_LENGTH - 1) {
      base = DIGIT_13_BASE;
      decompress = decompress13thChar;
      if (zeros > 1) {
        i = i - zeros;
        *((char*)((unsigned long)symbol+i)) = '_';
        i++;
      }
    }
    char c = decompress(rem % base);
    switch (c) {
    case 'a': zeros++; break;
    default: zeros=0; break;
    }
    *((char*)((unsigned long)symbol+i)) = c;
    rem /= base;
    i++;
  } while (rem > 0);
  *((char*)((unsigned long)symbol+i)) = '\0';
  return symbol;
}

