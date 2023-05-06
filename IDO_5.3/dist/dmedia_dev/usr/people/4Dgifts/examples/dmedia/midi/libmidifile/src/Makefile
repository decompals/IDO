#!smake

#
# Makefile for MIDI and MIDI file parsing.
#
# "$Revision: 1.1 $"
#
include $(ROOT)/usr/include/make/commondefs

C++FILES= midifile.c++ \
	miditrack.c++ \
	$(NULL)

LC++INCS = -I -I../include

#LC++OPTS= +I

LIBRARY=	libmidifile.a
TARGETS=	$(LIBRARY)

default all $(COMMONTAGS): $(TARGETS)

include $(COMMONRULES)

install: default
	${INSTALL} -m 444 -F /usr/lib $(LIBRARY)

$(TARGETS): $(OBJECTS)
	$(AR) cruv $@ $?
