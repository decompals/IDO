#! smake
# 
# $Revision: 1.2 $

include /usr/include/make/commondefs

OBJECTS = simplemovie.o

TARGETS = simplemovieGL

CFILES = \
	simplemovieGL.c

OPTIMIZER =-g 
SHDLIBC =
LCINCS = -I../common
LLDLIBS = -lmovieGL -ldmedia -lcl -lawareaudio -laudio -lX11_s -lGL -lGLU -lm
CVERSION = -ansi

default all: $(TARGETS)

clean:
	rm -f $(OBJECTS)

clobber:
	rm -f $(TARGETS) $(OBJECTS) core Makedepend

targets: $(TARGETS)

simplemovieGL: simplemovieGL.o
	rm -f $@
	$(CC) -o $@ simplemovieGL.o $(LDFLAGS)
