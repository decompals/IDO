include $(ROOT)/usr/include/make/commondefs

CFILES   = \
vidmemcomp.c \
$(NULL)

TARGETS = vidmemcomp

MOTIFLIBS 	= -lSgm -lXm -lXt -lX11
LLDLIBS     	= -lvl -lmovie  -lcl -lm -laudio -ldmedia $(MOTIFLIBS) -lmovie \
                    -lgl -lX11

default all: $(TARGETS)

include $(COMMONRULES)

vidmemcomp: vidmemcomp.o
	$(CC) -o $@ vidmemcomp.o $(LDFLAGS)
