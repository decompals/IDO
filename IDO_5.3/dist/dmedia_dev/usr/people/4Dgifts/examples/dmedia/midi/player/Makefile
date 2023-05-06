#!make 
#

include	$(ROOT)/usr/include/make/commondefs

DEPTH=..

LC++INCS = -I$(DEPTH)/libmidifile/include

LC++DEFS = -float
C++FILES = 	playfile.c++ recordmidi.c++

OBJS	= $(C++FILES:.c++=.o) 

LLDOPTS = -L$(DEPTH)/libmidifile/src
LLDLIBS	= -lmidifile -lmd

OPTIMIZER = -O2

TARGETS =   playfile recordmidi

install: default

default: $(TARGETS)

include	$(COMMONRULES)

playfile: playfile.o
	$(C++F) -o playfile playfile.o $(LDOPTS) $(LDFLAGS) 

recordmidi: recordmidi.o
	$(C++F) -o recordmidi recordmidi.o $(LDOPTS) $(LDFLAGS) 

