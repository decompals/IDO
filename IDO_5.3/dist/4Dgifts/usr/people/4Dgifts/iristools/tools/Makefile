#!smake

include $(ROOT)/usr/include/make/commondefs

LCOPTS	= -xansi -D__STDC__
LCINCS  = -I../include -I/usr/include/gl
CVERSION=
TARGETS = `ls *.c | sed 's/\.c$$//'`

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the swank macro, "VCOPTS=-g", on
# the command line (this will automatically override optimization).
LLDLIBS = ../libgutil/libgutil.a ../libimage/libimage.a -lgl -lm -s
LCDEFS  = -O

# override prototype errors warnings:  if you want to see the errors,
# uncomment the next line.
#PROTOTYPES=

default:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)

ical:
	$(CC) $(CFLAGS) -o $@ $@.c ../libgutil/libgutil.a ../libimage/libimage.a -lfm -lgl -lm -s

makemap:
	$(CC) $(CFLAGS) -o $@ $@.c ../libgutil/libgutil.a ../libimage/libimage.a -lgl -lX11 -lm -s
