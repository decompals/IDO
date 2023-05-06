#!smake
#
# Makefile for IndigoVideo example programs
#
# $Revision: 1.6 $

include $(ROOT)/usr/include/make/commondefs

GLPROGS	= burstcapt chromamap contcapt oneframe rgbgrab \
	  simplegl simpleinput sizeinput vmirror voverlay
XPROGS	= vout xevents xrgbgrab
TARGETS	= $(GLPROGS) $(XPROGS) svtomovie vbounce vclock vgrab vpaste vnurbtex

CFILES = \
	burstcapt.c \
	chromamap.c \
	contcapt.c \
	oneframe.c \
	rgbgrab.c \
	simplegl.c \
	simpleinput.c \
	sizeinput.c \
	svtomovie.c \
	vbounce.c \
	vclock.c \
	vgrab.c \
	vmirror.c \
	vnurbtex.c \
	vout.c \
	voverlay.c \
	vpaste.c \
	xevents.c \
	xrgbgrab.c

HFILES = Movie.h

GL_LIBS = -lsvideo -lgl -lXext -lX11
X_LIBS  = -lsvideo -lXext -lX11

GL_LDFLAGS=$(LDOPTS) $(GL_LIBS) $(LDLIBS)
X_LDFLAGS =$(LDOPTS) $(X_LIBS) $(LDLIBS)

LMKDEPFLAGS= $(NULLSUFFIX_MKDEPFLAG)
CVERSION=-cckr

default all::
#	@$(MAKE) incdepend
	@$(MAKE) targets

targets: $(TARGETS)

include $(COMMONRULES)

$(GLPROGS): $$@.c
	$(CCF) $@.c $(GL_LDFLAGS) -o $@

$(XPROGS): $$@.c
	$(CCF) $@.c $(X_LDFLAGS) -o $@

vclock vnurbtex:	$$@.c
	$(CCF) $@.c $(LDOPTS) $(GL_LIBS) -lm $(LDLIBS) -o $@

vgrab:	$$@.c
	$(CCF) $@.c $(LDOPTS) $(GL_LIBS) -limage $(LDLIBS) -o $@

vpaste:	$$@.c
	$(CCF) $@.c $(LDOPTS) $(X_LIBS) -limage $(LDLIBS) -o $@

svtomovie vbounce:	$$@.c
	$(CCF) $@.c $(LDOPTS) $(GL_LIBS) -laudio $(LDLIBS) -o $@
