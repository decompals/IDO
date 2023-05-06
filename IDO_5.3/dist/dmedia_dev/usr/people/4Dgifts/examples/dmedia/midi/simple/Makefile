#!make 
#

include	$(ROOT)/usr/include/make/commondefs

LLDLIBS	= -lmd

OPTIMIZER = -O2

default: scale thru

include	$(COMMONRULES)

thru: thru.o
	$(CCF) $@.o -o $@ $(LDFLAGS)

scale: scale.o
	$(CCF) $@.o -o $@ $(LDFLAGS)

