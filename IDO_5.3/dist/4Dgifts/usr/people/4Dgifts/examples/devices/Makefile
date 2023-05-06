#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = `ls *.c | sed 's/\.c$$//'`

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS = -lgl -s
LCOPTS	=

# override the prototypes macro in commondefs:  if you want 
# prototyping turned on, comment out the next line.
PROTOTYPES=

COMMONPREF=dev


all $(COMMONTARGS): $(COMMONPREF)$$@
	cd Spaceball; $(MAKE) $@
	cd StereoView; $(MAKE) $@
	cd devscsi; $(MAKE) $@

$(COMMONPREF)all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)
