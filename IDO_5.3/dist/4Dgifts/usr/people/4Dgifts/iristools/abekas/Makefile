#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = degoop engoop mycp readlabel ready rgb2yuv scread scscan status \
	scwrite yuv yuv2rgb
# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS = -s
LCOPTS  =

# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=


default: $(TARGETS)

include $(COMMONRULES)


scread: scread.c
	$(CCF) -o $@ $@.c -lds $(LDFLAGS)

readlabel: readlabel.c
	$(CCF) -o $@ $@.c -lds $(LDFLAGS)

ready: ready.c
	$(CCF) -o $@ $@.c -lds $(LDFLAGS)

status: status.c
	$(CCF) -o $@ $@.c -lds $(LDFLAGS)

scwrite: scwrite.c
	$(CCF) -o $@ $@.c -lds $(LDFLAGS)

yuv: yuv.o abklib.o
	$(CCF) -o $@ $@.c abklib.o -lgl -lds $(LDFLAGS)
