#!smake

include $(ROOT)/usr/include/make/commondefs


LCINCS	= -I.
LCOPTS	= -g
CFILES	= GlxDraw.c GlxMDraw.c

TARGETS	= libsgiw.a

default: $(TARGETS)

include $(COMMONRULES)

GlxMDraw.o : GlxDraw.c
	cc -c $(CFLAGS) -D__GLX_MOTIF -o $@ GlxDraw.c

$(TARGETS): $(OBJECTS)
	$(AR) cr $(TARGETS) $(OBJECTS)

GlxDraw.o GlxMDraw.o: GlxDraw.h GlxMDraw.h GlxDrawP.h GlxMDrawP.h
