#!smake

include $(ROOT)/usr/include/make/commondefs


TARGETS	= iisc inquire 

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS = libds.a -lgl -lm -s
LCOPTS	=
LDIRT = libds.a

# see sgiimg.c and rasterimg.c
IISCFLAGS=-DSGI_IMAGE
CVERSION = -cckr


# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=

default all: $(TARGETS)

$(TARGETS): libds.a

include $(COMMONRULES)

iisc: $$@.c rasterimg.c sgiimg.c 
	$(CCF) $(IISCFLAGS) -o $@ $@.c sgiimg.c rasterimg.c -limage -lgutil $(LLDLIBS)

iisc.o: iisc.c iisc.h

libds.a: libds.a(dstab.o) libds.a(dslib.o)
	$(AR) vr $@ $?

