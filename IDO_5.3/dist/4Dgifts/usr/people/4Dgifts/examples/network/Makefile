#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = `ls *.c | sed 's/\.c$$//'`

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS = -s
LCOPTS  =

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=


default all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)


ercv:  ercv.c
	$(CC) $(CFLAGS) -o $@ $@.c -lsun $(LDFLAGS)

esnd:  esnd.c
	$(CC) $(CFLAGS) -o $@ $@.c -lsun $(LDFLAGS)

nettest:  nettest.c
	$(CC) $(CFLAGS) -DNO_ISO -o $@ $@.c -lsun $(LDFLAGS)

nettestd:  nettestd.c
	$(CC) $(CFLAGS) -DNO_ISO -o $@ $@.c -lsun $(LDFLAGS)

whois:  whois.c
	$(CC) $(CFLAGS) -o $@ $@.c -lsun $(LDFLAGS)
