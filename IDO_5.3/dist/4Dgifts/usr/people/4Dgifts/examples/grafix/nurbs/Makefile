#!smake

include $(ROOT)/usr/include/make/commondefs

#  Fortran and Pascal versions of the nurbs.c program exist in this directory,
#  but have not been explicitly added to the following "PROGS" line.  If you
#  wish to compile either the Fortran or Pascal version, simply add the 
#  executable names "fnurbs" or "pnurbs" respective to the "PROGS" line.
TARGETS = nurbs model sgiflag heart


# If you want to run a debugger on these programs, remove the -s from the
# appropriate LD*FLAGS line(s), and add -g to whichever L*OPTS language 
# you need.  NOTE:  as of IRIX release 4.0, you can bypass having to change
# this file AT ALL by using the bitchen macro, "VCOPTS=-g", on the command 
# line (this will automatically override optimization).
LDCFLAGS= -lgl -s
LDFFLAGS= -lfgl -lgl -s
LDPFLAGS= /usr/lib/p2cstr.o -lpgl -lgl -s
LCOPTS	=
LF77OPTS=
LPCOPTS	=

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=

default all: $(TARGETS) 

include $(COMMONRULES)

# C version
nurbs: nurbs.c
	$(CCF) nurbs.c $(LDCFLAGS) -o $@

# Fortran version
fnurbs:	nurbs.f
	$(F77F) nurbs.f $(LDFFLAGS) -o $@

# Pascal version; can't optimize because of compiler bug
pnurbs:	nurbs.p
	$(PCF)  nurbs.p  $(LDPFLAGS) -o $@


model:	model.o modeldata.o
	$(CCF) -o $@ $@.o modeldata.o $(LDCFLAGS) -lm

sgiflag: sgiflag.c
	$(CCF) -o $@ $@.c $(LDCFLAGS) -lm

heart: heart.c
	$(CCF) -o $@ $@.c $(LDCFLAGS) -lm

