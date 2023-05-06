#!smake

include $(ROOT)/usr/include/make/commondefs

#
# Simplified makefile for example trackball user interface
# written by Gavin Bell for Silicon Graphics
#
TARGETS = example
CFILES	= example.c event.c trackball.c ui.c vect.c

#
LLDLIBS = -lsun -lgl -lm
LCDEFS	= -O -float

# If you want prototypes turned off, un-comment the following line:
# PROTOTYPES=

default all: $(TARGETS)

include $(COMMONRULES)

event.o:	event.c event.h
example.o:	event.h example.c trackball.h ui.h vect.h
trackball.o:	trackball.c trackball.h vect.h
ui.o:		event.h trackball.h ui.c ui.h vect.h
vect.o:		vect.c vect.h

$(TARGETS): $(OBJECTS)
	$(CCF) $(OBJECTS) $(LDFLAGS) -o $@

