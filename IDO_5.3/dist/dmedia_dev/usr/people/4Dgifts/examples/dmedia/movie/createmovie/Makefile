#! smake
# 
# $Revision: 1.6 $

include /usr/include/make/commondefs

CREATEMOVIE_OBJECTS = createmovieArgs.o createmovieInit.o createmovieResize.o createmovieConvert.o createmovieFiles.o createmovie.o

OBJECTS = $(CREATEMOVIE_OBJECTS)

TARGETS = createmovie

C++FILES = \
	createmovieArgs.c++ \
	createmovieInit.c++ \
	createmovieFiles.c++ \
	createmovieResize.c++ \
	createmovie.c++

CFILES = \
	createmovieConvert.c

OPTIMIZER =-g 
SHDLIBC =
LLDLIBS = -lmovie -ldmedia -lcl -lawareaudio -laudio -lm -lX11 -lgl

default all: $(TARGETS)

clean:
	rm -f $(OBJECTS)

clobber:
	rm -f $(TARGETS) $(OBJECTS) core Makedepend

targets: $(TARGETS)

createmovie: $(CREATEMOVIE_OBJECTS) $(LIBRARY)
	rm -f $@
	$(C++F) -o $@ $(CREATEMOVIE_OBJECTS) $(LDFLAGS) -lmalloc -laudiofile -laudioutil -lil $(C++FILT)
