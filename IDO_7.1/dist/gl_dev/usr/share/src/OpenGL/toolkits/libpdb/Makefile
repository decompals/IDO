#!smake

GLSTYLE ?= ALL

COMMONPREF = standard
include $(ROOT)/usr/include/make/commondefs
sinclude $(ROOT)/usr/include/make/$(PRODUCT)defs

#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
STYLES = 32 N32 64
TARGETS = libpdb.32 libpdb.N32 libpdb.64
#else
STYLES = 32
TARGETS = libpdb.32
#endif

GLDEBUG ?= no
#if $(GLDEBUG) != "no"
WOFF =
OPTIMIZER = -g
#endif

TOOLKITDIR ?= /usr/share/src/OpenGL/toolkits

LCINCS = -I$(ROOT)$(TOOLKITDIR)/include

HFILES = \
	pdb.h \
	$(NULL)

CFILES = \
	pdb.c \
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
	@if [ ! -d libpdb.$(OBJECT_STYLE) ]; then \
	    mkdir libpdb.$(OBJECT_STYLE); \
	fi; \
	cd libpdb.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile links;
	cd libpdb.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile libpdb.a;

links:
	@for i in $(HFILES) $(CFILES); do \
	    if [ ! -r $$i ]; then \
		ln -s ../$$i $$i; \
	    fi; \
	done;

libpdb.a: $(OBJECTS)
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
	$(INSTALL) -F $(TOOLKITDIR)/include -idb "LIBPDB" pdb.h;

install: headers default
	$(INSTALL) -F $(TOOLKITDIR)/lib -idb "LIBPDB" -src libpdb.32/libpdb.a -O libpdb.a;
#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
	$(INSTALL) -F $(TOOLKITDIR)/lib32 -idb "LIBPDB" -src libpdb.N32/libpdb.a -O libpdb.a;
	$(INSTALL) -F $(TOOLKITDIR)/lib64 -idb "LIBPDB" -src libpdb.64/libpdb.a -O libpdb.a;
#endif

$(COMMONTARGS): $(COMMONPREF)$$@
