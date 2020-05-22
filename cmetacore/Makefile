# Depends on the Boehm GC:
#
#     sudo apt install libgc-dev

FILES		= Bool.c Error.c Eval.c Lexer.c Nat.c Pair.c Parser.c PrimFun.c Primitives.c Struct.c Str.c Symbol.c SymbolValue.c StructValue.c main.c
CC			= g++
CFLAGS	= -Wall -g -ansi -DYY_NO_UNPUT=1

test: $(FILES)
	$(CC) $(CFLAGS) $(FILES) -o test -lgc

Lexer.c: Lexer.l
	flex Lexer.l

Parser.c:	Parser.y Lexer.c
	bison Parser.y

clean:
	rm -f *.o Lexer.c Lexer.h Parser.c Parser.h test
