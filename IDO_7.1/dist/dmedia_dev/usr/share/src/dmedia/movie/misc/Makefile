#! smake
# 
# $Revision: 1.5 $

include /usr/include/make/commondefs

OBJECTS = mvinfo.o simplemovie.o aud-to-movie.o img-to-movie.o mvPlayTimeUtils.o mvtimeframe.o

TARGETS = mvinfo simplemovie mvtimeframe aud-to-movie img-to-movie

CFILES = \
	simplemovie.c \
	mvinfo.c \
	aud-to-movie.c \
	mvPlayTimeUtils.c \
	mvtimeframe.c

C++FILES = \
	img-to-movie.c++

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

mvinfo: mvinfo.o $(LIBRARY)
	rm -f $@
	$(CC) -o $@ mvinfo.o $(LDFLAGS) -lmalloc

simplemovie: simplemovie.o ;cd ../common ;make ;cd ../misc
	rm -f $@
	$(CC) -o $@ ../common/glxhelper.o simplemovie.o $(LDFLAGS)

mvtimeframe: mvPlayTimeUtils.o mvtimeframe.o $(LIBRARY)
	rm -f $@
	$(CC) -o $@ mvPlayTimeUtils.o mvtimeframe.o $(LDFLAGS) -lmalloc

img-to-movie : img-to-movie.o
	rm -f $@
	$(C++F) -o $@ $@.o $(LDFLAGS) -lmalloc -lil $(C++FILT)

aud-to-movie : aud-to-movie.o
	rm -f $@
	$(CC) -o $@ $@.o $(LDFLAGS) -lmalloc -laudiofile -laudioutil
