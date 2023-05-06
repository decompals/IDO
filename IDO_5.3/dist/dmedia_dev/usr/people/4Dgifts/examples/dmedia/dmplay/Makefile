include $(ROOT)/usr/include/make/commondefs

CFILES = dmplay.c \
	vlInit.c \
	clInit.c \
	alInit.c \
	mvInit.c \
	Xinit.c \
	singleFrameDecompress.c \
	streamDecompress.c \
	parseargs.c \
	$(NULL)

HFILES = dmplay.h
LLDLIBS=-lSgm -lXm -lXt -lX11 -lmovie -lcl -ldmedia -lgl -laudio -lm -lvl -lgen

TARGETS =  dmplay

default all: $(TARGETS)


dmplay: $(OBJECTS)
	$(CCF) -o $@ $(OBJECTS) $(LLDLIBS)
