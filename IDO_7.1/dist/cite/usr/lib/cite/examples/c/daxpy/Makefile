#!smake

.SUFFIXES: .f .loc .l .s

default all: \
	daxpy.s 

.c.s:
	- /usr/bin/cc -c -64 -mips4 -O3 -LIST:cite $*.c 


clean clobber:
	 rm -f *.l *.s *.loc *after-lno.* core *.B *.N *.o *~ *.flc
