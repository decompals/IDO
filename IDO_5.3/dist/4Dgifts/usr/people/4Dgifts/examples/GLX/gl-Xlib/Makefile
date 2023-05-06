#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = mix mix2 mixexamp keyboard mouse swirl

# If you want to run a debugger on these programs, remove the -s from LD*FLAGS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LDMFLAGS= -lsphere -lgl -lm -lX11 -s
LDCFLAGS= -lgl -lX11 -lsun -s
LDSFLAGS= -lgl -lX11 -s
LCOPTS  =

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=

all: $(TARGETS)

include $(COMMONRULES)

mix: mix.o glxhelper.o
	$(CCF) -o $@ $@.o glxhelper.o $(LDMFLAGS)

mix2: mix2.o glxh2.o
	$(CCF) -o $@ $@.o glxh2.o -lgutil $(LDMFLAGS)

mixexamp: mixexamp.c
	$(CCF) -o $@ $@.c $(LDSFLAGS)

swirl: swirl.o line.o glxhelper.o
	$(CCF) -o $@ $@.o line.o glxhelper.o $(LDSFLAGS)

keyboard: keyboard.c
	$(CCF) -I/usr/include/bsd -o $@ $@.c $(LDCFLAGS)

mouse: mouse.c
	$(CCF) -I/usr/include/bsd -o $@ $@.c $(LDCFLAGS)
