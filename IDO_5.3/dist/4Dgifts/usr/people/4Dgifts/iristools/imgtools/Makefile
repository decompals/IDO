#!smake

include $(ROOT)/usr/include/make/commondefs

LCOPTS	= -xansi -D__STDC__
LCINCS	= -I../include -I/usr/include/gl
CVERSION=
TARGETS	= `ls *.c | sed 's/\.c$$//'` 

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the swank macro, "VCOPTS=-g", on
# the command line (this will automatically override optimization).
LLDLIBS	= ../libgutil/libgutil.a ../libimage/libimage.a -lgl -lm
LCDEFS	= -g

# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=

default:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)
