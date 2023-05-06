#
include ${ROOT}/usr/include/make/commondefs

TARGETS = jet spaceball_fly

SUBDIRS= gl.or.x

#LCOPTS = -g
LLDLIBS = -lspaceball -lgl -lm

# If you can't use prototype checking, uncomment the following line.
#PROTOTYPES=

default all: $(TARGETS)
	@for d in ${SUBDIRS}; do			\
		echo "======\tcd $$d; ${MAKE} $@";	\
		(cd $$d; ${MAKE} $@);			\
	done

$(TARGETS): $$@.o
	$(CC) $(GFLAG) -o $@ $@.o ${LDFLAGS}

include ${COMMONRULES}
