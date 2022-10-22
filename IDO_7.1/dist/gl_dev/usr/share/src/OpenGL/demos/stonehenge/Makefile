#!smake

GLSTYLE ?= ALL

COMMONPREF = standard
include $(ROOT)/usr/include/make/commondefs
sinclude $(ROOT)/usr/include/make/$(PRODUCT)defs

#if defined(RELEASE_MAJOR) && $(RELEASE_MAJOR) == "6"
STYLES = 32 N32 64
TARGETS = stonehenge.32 stonehenge.N32 stonehenge.64
#else
STYLES = 32
TARGETS = stonehenge.32
#endif

GLDEBUG ?= no
#if $(GLDEBUG) != "no"
WOFF =
OPTIMIZER = -g
#endif

TOOLKITDIR ?= /usr/share/src/OpenGL/toolkits

LC++DEFS = -DDATADIR=\"..\/..\/data\/\" -DTEXTURE
LC++INCS = -I$(ROOT)$(TOOLKITDIR)/include -I$(ROOT)$(TOOLKITDIR)/include/GL
OBJECT_STYLE ?= 32
#if $(OBJECT_STYLE) == "64"
LLDLIBS = -L$(ROOT)$(TOOLKITDIR)/lib64
#elif $(OBJECT_STYLE) == "N32"
LLDLIBS = -L$(ROOT)$(TOOLKITDIR)/lib32
#else
LLDLIBS = -L$(ROOT)$(TOOLKITDIR)/lib
#endif
LLDLIBS += -lrgb -lGLw -lGLU -lGL -lXm -lXt -lXext -lX11 -lm

HFILES = \
	Color.h \
	Ellipse.h \
	Point.h \
	Ring.h \
	Roundwall.h \
	Stone.h \
	Telescope.h \
	TimeDate.h \
	atmosphere.h \
	callbacks.h \
	scene.h \
	$(NULL)

C++FILES = \
	Ellipse.c++ \
	Point.c++ \
	Ring.c++ \
	Roundwall.c++ \
	Stone.c++ \
	Telescope.c++ \
	TimeDate.c++ \
	atmosphere.c++ \
	callbacks.c++ \
	main.c++ \
	scene.c++ \
	$(NULL)

default all: $(_FORCE)
	@if [ "$(GLSTYLE)" = "ALL" ]; then \
	    ALLSTYLES="$(STYLES)"; \
	else \
	    ALLSTYLES="$(GLSTYLE)"; \
	fi; \
	for STYLE in $$ALLSTYLES; do \
	    OBJECT_STYLE=$$STYLE $(MAKE) program; \
	done;

include $(COMMONRULES)

program: $(_FORCE)
	@if [ ! -d stonehenge.$(OBJECT_STYLE) ]; then \
	    mkdir stonehenge.$(OBJECT_STYLE); \
	fi;
	cd stonehenge.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile links;
	cd stonehenge.$(OBJECT_STYLE); OBJECT_STYLE=$(OBJECT_STYLE) $(MAKE) -f ../Makefile stonehenge;

links:
	@for i in $(HFILES) $(C++FILES); do \
	    if [ ! -r $$i ]; then \
	        ln -sf ../$$i $$i; \
	    fi; \
	done;

stonehenge: $(OBJECTS)
	$(C++F) -o $@ $(OBJECTS) $(LDFLAGS);

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

headers exports install: default

$(COMMONTARGS): $(COMMONPREF)$$@
