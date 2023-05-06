#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS	= playfile scale thru

# If you want to run a debugger on these programs, add
# -g to the OPTIMIZER line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS	= -lmd -lm -lmpc 
OPTIMIZER =

SUBDIRS =libmidifile mfp player simple

default: 
	$(SUBDIRS_MAKERULE)

include $(COMMONRULES)
