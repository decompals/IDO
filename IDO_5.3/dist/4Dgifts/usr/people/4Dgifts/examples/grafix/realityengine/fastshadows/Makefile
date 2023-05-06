#
# Makefile for 'shad' program
#
# $Revision: 1.3 $
#
SHELL = /bin/sh
CFLAGS = -O -prototypes -float
LDFLAGS = -s
OBJS = shad.o sgiobjgl.o glshade.o matrix.o

install all: shad

shad: $(OBJS)
	cc $(CFLAGS) $(LDFLAGS) -o $@ $(OBJS) -limage -lgl -lc

clean:
	/bin/rm -f *.o core

clobber: clean
	/bin/rm -f shad

.c.o:
	cc $(CFLAGS) -c $<
