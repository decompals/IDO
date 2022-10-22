#
# Makefile for SGI Movie Library source
#

include /usr/include/make/commondefs

TARGETS = guimvplayer simplevkplayer

OBJECTS = guimvplayer.o simplevkplayer.o

OPTIMIZER = -g
LC++INCS = -DFUNCPROTO
VKLIBS =  -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG) -lSgm
LLDOPTS   = -lMovieController -lmovie -ldmedia -lXirisw -lXm -lXmu -lXt -lX11_s -lgl_s -lcl -limp -lawareaudio -laudio -laudiofile -lm

default all:	$(TARGETS)

$(TARGETS):	$(OBJECTS)

guimvplayer:	guimvplayer.o
		$(C++F) -o $@ guimvplayer.o $(LDFLAGS) $(C++FILT)

simplevkplayer:	 simplevkplayer.o 
		$(C++) -o $@ simplevkplayer.o $(LDFLAGS) $(VKLIBS) $(C++FILT)

clean:
	rm -f core *.BAK *.bak *.ckp *.und *~ $(OBJECTS)

clobber:
	rm -f core *.BAK *.bak *.ckp *.und *~ $(OBJECTS) $(TARGETS)

install:
	install -m 444 -F /usr/lib/X11/app-defaults Guimvplayer
