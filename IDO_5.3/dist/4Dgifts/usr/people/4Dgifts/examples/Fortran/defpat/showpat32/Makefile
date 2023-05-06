#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = showpat32
FFILES	= showpat32.f getfile.f buildicon.f gsetup.f readicon.f

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and change the -O in LF77OPTS to -g.
LLDLIBS = -lfgl -lgl -s
LF77OPTS= -O
SHDLIBC	=

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=

all: $(TARGETS)

include $(COMMONRULES)

$(TARGETS): $(OBJECTS)
	$(F77) $(FFLAGS) $(OBJECTS) $(LDFLAGS) -o $@
