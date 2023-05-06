# #!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = jisraster

LLDLIBS	= -lgl 
LCOPTS	=
CFLAGS = -cckr -I -I$(ROOT)/usr/include -I/usr/include/X11/fonts -L -L$(ROOT)/usr/lib

default all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)


jisraster:  jisraster.o jistools.o pcftogl.o
	$(CC) $(CFLAGS) -o $@ $@.o jistools.o pcftogl.o $(LLDLIBS) 

