
#!smake

.SUFFIXES: .f .loc .l .s

default all: \
	mmj.l
 

.f.l:
	- /usr/bin/f77 -o $*.O2 -64 -mips4 -O2 $*.f 
	- /usr/bin/f77 -o $*.O3 -64 -mips4 -O3 -LIST:cite $*.f 


clean clobber:
	rm -f *.l *.s *.loc *after-lno.* core *.B *.N *.o *.O2 *.O3 *~ *.flc
