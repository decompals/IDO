#!smake

GLSTYLE ?= ALL

COMMONPREF = standard
include $(ROOT)/usr/include/make/commondefs
sinclude $(ROOT)/usr/include/make/$(PRODUCT)defs

#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
STYLES = 32 N32 64
TARGETS = libGLw.32 libGLw.N32 libGLw.64
#else
STYLES = 32
TARGETS = libGLw.32
#endif

GLDEBUG ?= no
#if $(GLDEBUG) != "no"
WOFF =
OPTIMIZER = -g
#endif

TOOLKITDIR ?= /usr/share/src/OpenGL/toolkits

LCINCS = -I$(ROOT)$(TOOLKITDIR)/include

DFILES = \
	README \
	$(NULL)

HFILES = \
	GLwDrawA.h \
	GLwDrawAP.h \
	GLwMDrawA.h \
	GLwMDrawAP.h \
	$(NULL)

CFILES = \
	GLwDrawA.c \
	GLwMDrawA.c \
	GLwDAUtil.c \
	$(NULL)

default: $(_FORCE)
	@if [ "$(GLSTYLE)" = "ALL" ]; then \
	    ALLSTYLES="$(STYLES)"; \
	else \
	    ALLSTYLES="$(GLSTYLE)"; \
	fi; \
	for STYLE in $$ALLSTYLES; do \
	    OBJECT_STYLE=$$STYLE $(MAKE) library; \
	done;

include $(COMMONRULES)

library:
	@if [ ! -d libGLw.$(OBJECT_STYLE) ]; then \
	    mkdir libGLw.$(OBJECT_STYLE); \
	fi; \
	cd libGLw.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile links;
	cd libGLw.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile libGLw.a;

links:
	@for i in $(HFILES) $(CFILES); do \
	    if [ ! -r $$i ]; then \
		ln -s ../$$i $$i; \
	    fi; \
	done;

libGLw.a: $(OBJECTS)
	$(AR) crl $@ $(OBJECTS);

clean: $(COMMONPREF)$$@
	for i in $(TARGETS); do \
	    if [ -d $$i ]; then \
		( cd $$i; $(MAKE) $(COMMONPREF)clean ); \
	    fi; \
	done;

clobber: $(COMMONPREF)$$@
	for i in $(TARGETS); do \
	    if [ -d $$i ]; then \
		rm -rf $$i; \
	    fi; \
	done;

headers:
	$(INSTALL) -F $(TOOLKITDIR)/include/GL -idb "LIBWIDGET" $(HFILES);

install: headers default
	$(INSTALL) -F $(TOOLKITDIR)/lib -idb "LIBWIDGET" -src libGLw.32/libGLw.a -O libGLw.a;
#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
	$(INSTALL) -F $(TOOLKITDIR)/lib32 -idb "LIBWIDGET" -src libGLw.N32/libGLw.a -O libGLw.a;
	$(INSTALL) -F $(TOOLKITDIR)/lib64 -idb "LIBWIDGET" -src libGLw.64/libGLw.a -O libGLw.a;
#endif

$(COMMONTARGS): $(COMMONPREF)$$@
