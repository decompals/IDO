#!smake

GLSTYLE ?= ALL

COMMONPREF = standard
include $(ROOT)/usr/include/make/commondefs
sinclude $(ROOT)/usr/include/make/$(PRODUCT)defs

#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
STYLES = 32 N32 64
TARGETS = librgb.32 librgb.N32 librgb.64
#else
STYLES = 32
TARGETS = librgb.32
#endif

GLDEBUG ?= no
#if $(GLDEBUG) != "no"
WOFF =
OPTIMIZER = -g
#endif

TOOLKITDIR ?= /usr/share/src/OpenGL/toolkits

HFILES = \
	rgb.h \
	$(NULL)

CFILES = \
	rgb.c \
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
	@if [ ! -d librgb.$(OBJECT_STYLE) ]; then \
	    mkdir librgb.$(OBJECT_STYLE); \
	fi; \
	cd librgb.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile links;
	cd librgb.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile librgb.a;

links:
	@for i in $(HFILES) $(CFILES); do \
	    if [ ! -r $$i ]; then \
		ln -s ../$$i $$i; \
	    fi; \
	done;

librgb.a: $(OBJECTS)
	$(AR) crl $@ $(OBJECTS);

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
	$(INSTALL) -F $(TOOLKITDIR)/include -idb "LIBRGB" rgb.h;

install: headers default
	$(INSTALL) -F $(TOOLKITDIR)/lib -idb "LIBRGB" -src librgb.32/librgb.a -O librgb.a;
#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
	$(INSTALL) -F $(TOOLKITDIR)/lib32 -idb "LIBRGB" -src librgb.N32/librgb.a -O librgb.a;
	$(INSTALL) -F $(TOOLKITDIR)/lib64 -idb "LIBRGB" -src librgb.64/librgb.a -O librgb.a;
#endif

$(COMMONTARGS): $(COMMONPREF)$$@
