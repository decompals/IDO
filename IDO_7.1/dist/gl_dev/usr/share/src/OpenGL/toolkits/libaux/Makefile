#!smake

GLSTYLE ?= ALL

COMMONPREF = standard
include $(ROOT)/usr/include/make/commondefs
sinclude $(ROOT)/usr/include/make/$(PRODUCT)defs

#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
STYLES = 32 N32 64
TARGETS = libaux.32 libaux.N32 libaux.64
#else
STYLES = 32
TARGETS = libaux.32
#endif

GLDEBUG ?= no
#if $(GLDEBUG) != "no"
WOFF =
OPTIMIZER = -g
#endif

TOOLKITDIR ?= /usr/share/src/OpenGL/toolkits
LIBTKDIR = ../../libtk/libtk.$(OBJECT_STYLE)
LIBRGBDIR = ../../libaux/librgb.$(OBJECT_STYLE)

LCINCS = -I../../libtk -I../../librgb
LLDLIBS = $(LIBTKDIR)/libtk.a $(LIBRGBDIR)/librgb.a -lXext -lX11 -lm

HFILES = \
	3d.h \
	aux.h \
	teapot.h \
	$(NULL)

CFILES = \
	aux.c \
	shapes.c \
	teapot.c \
	vect3d.c \
	xform.c \
	$(NULL)

OFILES = \
	$(LIBTKDIR)/cursor.o \
	$(LIBTKDIR)/event.o \
	$(LIBTKDIR)/getset.o \
	$(LIBTKDIR)/shapes.o \
	$(LIBTKDIR)/window.o \
	$(NULL)

default all: $(_FORCE)
	@if [ "$(GLSTYLE)" = "ALL" ]; then \
	    ALLSTYLES="$(STYLES)"; \
	else \
	    ALLSTYLES="$(GLSTYLE)"; \
	fi; \
	for STYLE in $$ALLSTYLES; do \
	    OBJECT_STYLE=$$STYLE $(MAKE) library; \
	done;

include $(COMMONRULES)

library: $(_FORCE)
	@if [ ! -d libaux.$(OBJECT_STYLE) ]; then \
	    mkdir libaux.$(OBJECT_STYLE); \
	fi; \
	cd libaux.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile links;
	cd libaux.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile libaux.a;

links:
	@for i in $(HFILES) $(CFILES); do \
	    if [ ! -r $$i ]; then \
		ln -s ../$$i $$i; \
	    fi; \
	done;

libaux.a: $(OFILES) $(OBJECTS)
	$(AR) crl $@ $(OFILES) $(OBJECTS);

clean: $(COMMONPREF)$$@
	for i in $(TARGETS); do \
	    if [ -d $$i ]; then \
		( cd $$i; $(MAKE) -f ../Makefile $(COMMONPREF)clean ); \
	    fi; \
	done;

clobber: $(COMMONPREF)$$@
	for i in $(TARGETS); do \
	    if [ -d $$i ]; then \
		rm -rf $$i; \
	    fi; \
	done;

headers:
	$(INSTALL) -F $(TOOLKITDIR)/include -idb "LIBAUX" aux.h;

install: headers default
	$(INSTALL) -F $(TOOLKITDIR)/lib -idb "LIBAUX" -src libaux.32/libaux.a -O libaux.a;
#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
	$(INSTALL) -F $(TOOLKITDIR)/lib32 -idb "LIBAUX" -src libaux.N32/libaux.a -O libaux.a;
	$(INSTALL) -F $(TOOLKITDIR)/lib64 -idb "LIBAUX" -src libaux.64/libaux.a -O libaux.a;
#endif

$(COMMONTARGS): $(COMMONPREF)$$@
