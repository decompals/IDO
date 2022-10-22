#
#
# Makefile for IL test programs
#

SHELL = /bin/sh
MP_SET_NUMTHREADS = 2
FLAGS = -g
MP = -mp 


CMAINS = fibo.c
COBJS = ${CMAINS:.c=.o}
CPROGS = ${CMAINS:.c=}
CLIBS =  

FMAINS = total.f
FOBJS = ${FMAINS:.f=.o}
FROGS = ${FMAINS=.f=}
FLIBS = 

.SUFFIXES : .o .f .c

.f:
	f77 $(MP) -mp_keep $(FLAGS) $< -o $@ 

.c:
	cc $(MP) $(FLAGS) $< -o $@ 

.c.o:
	cc $(INCLUDE) $(FLAGS) $<

clean:
	rm -rf *.o
	rm -rf core
	rm -rf *.sav

clobber: clean
	rm $(FROGS) $(CPROGS)
