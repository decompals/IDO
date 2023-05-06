#!smake
SHELL = /bin/sh
CFLAGS = -O 
CC =cc

readltc:       readltc.o
	$(CC) -o $@ readltc.o -laudiofile -lm
.c.o:
	$(CC) $(CFLAGS) -c $<
clean:
	rm -f *.o

clobber:clean
	rm -f readltc
