#! smake
# 
# $Revision: 1.5 $

include /usr/include/make/commondefs

EDITMOVIE_OBJECTS = editmovieArgs.o editmovieCopy.o editmovieEdit.o editmovie.o

OBJECTS = $(EDITMOVIE_OBJECTS)

TARGETS = editmovie

CFILES = \
	editmovieArgs.c \
	editmovieCopy.c \
	editmovieEdit.c \
	editmovie.c

OPTIMIZER =-g 
SHDLIBC =
LLDLIBS = -lmovie -ldmedia -lcl -lawareaudio -laudio -laudiofile -lgl -lm -lil -lcil -lX11

default all: $(TARGETS)

clean:
	rm -f $(OBJECTS)

clobber:
	rm -f $(TARGETS) $(OBJECTS) core Makedepend

targets: $(TARGETS)

editmovie: $(EDITMOVIE_OBJECTS) $(LIBRARY)
	rm -f $@
	$(CC) -o $@ $(EDITMOVIE_OBJECTS) $(LDFLAGS) -lmalloc
