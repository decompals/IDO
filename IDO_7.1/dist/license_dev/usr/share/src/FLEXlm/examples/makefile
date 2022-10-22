#!smake

#
# Commondefs
#

include $(ROOT)/usr/include/make/commondefs

OPTIMIZER= ${UOPTIMIZER}
LIBDIR=/usr/lib

LDLIBS =  -lgen -llmvksgi -llmsgi -lvk -lvkhelp -lXm -lXt -lX11

LC++INCS=${NORMALC++INCS}

LLDOPTS=-L${LIBDIR}

#
# my targets
#

TARGETS=apitest vktest

default: $(TARGETS)

install: default

apitest: apitest.o
	$(C++F) -o $@ apitest.o $(LDOPTS) $(LDLIBS)

vktest: vktest.o
	$(C++F) -o $@ vktest.o $(LDOPTS) $(LDLIBS)

#
# commonrules
#

include $(COMMONRULES)
