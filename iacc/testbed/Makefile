FILES		= scheme.s main.c
CC			= gcc
CFLAGS	= -Wall -g

compile: $(FILES)
	$(CC) $(CFLAGS) $(FILES) -o scheme.exe

assemble: scheme.c
	$(CC) -O3 --omit-frame-pointer -S scheme.c

clean:
	rm -f *.o scheme
