#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = `ls *.f | sed 's/\.f$$//'`

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and change the -O in LF77OPTS to -g.
LLDLIBS = -lfgl -lgl -s
LF77OPTS= -O
SHDLIBC	=

# override the prototypes macro in commondefs:  if you want 
# prototyping turned on, comment out the next line.
PROTOTYPES=

COMMONPREF=ftn

all $(COMMONTARGS): $(COMMONPREF)$$@
	cd defpat; $(MAKE) $@
	cd globe; $(MAKE) $@
	cd quick; $(MAKE) $@
	cd shmemf77; $(MAKE) $@

$(COMMONPREF)all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)
