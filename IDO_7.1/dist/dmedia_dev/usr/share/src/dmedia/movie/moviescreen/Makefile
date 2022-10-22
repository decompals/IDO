#! smake
# 
# $Revision: 1.5 $

include /usr/include/make/commondefs

MOVIESCREEN_OBJECTS  = moviescreenArgs.o moviescreenWin.o moviescreenGl.o moviescreenEvents.o moviescreen.o 

OBJECTS = $(MOVIESCREEN_OBJECTS)

TARGETS = moviescreen

CFILES = \
	moviescreenArgs.c \
	moviescreenWin.c \
	moviescreenGl.c \
	moviescreenEvents.c \
	moviescreen.c

OPTIMIZER =-g 
SHDLIBC =
LCINCS = -I../common
LLDLIBS = -lmovie -ldmedia -lcl -lawareaudio -laudio -lX11_s -lgl_s -lm

default all: $(TARGETS)

clean:
	rm -f $(OBJECTS)

clobber:
	rm -f $(TARGETS) $(OBJECTS) core Makedepend

targets: $(TARGETS)

moviescreen: $(MOVIESCREEN_OBJECTS) ;cd ../common ;make ;cd ../moviescreen
	rm -f $@
	$(CC) -o $@ ../common/glxhelper.o $(MOVIESCREEN_OBJECTS) $(LDFLAGS)
