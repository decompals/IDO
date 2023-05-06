#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = shmem

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and change the -O in LF77OPTS to -g.
LLDLIBS	= -s
LF77OPTS= -O

# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=

all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)

shmem:	shmem.o	shmget.o shmfree.o
	f77 $(LF77OPTS) -o $(TARGETS) shmem.o shmget.o shmfree.o $(LLDLIBS)
