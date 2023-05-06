
include $(ROOT)/usr/include/make/commondefs

OPTIMIZER= -O2 -32
LDIRT = ptrepository
INSTDIR = /usr/people/4Dgifts/bin
PARDIR = /usr/people/4Dgifts/OpenGL
INSTSRC = /usr/people/4Dgifts/OpenGL/toogl
SRC_FILES =  Makefile Makefile.lib README irisgl_light.c perlassoc.h \
	perlclass.c++ perlclass.h regex.c regex.h regexp.h regmagic.h \
	search.c++ search.h toogl.c++
 

default: toogl

include $(COMMONRULES)

C++FILES = perlclass.c++ toogl.c++ search.c++
CFILES = regex.c

TARGETS = toogl

toogl : $(OBJECTS)
	$(C++F) -pte.c++ -L$(ROOT)/usr/lib -o toogl $(OBJECTS) 	

toogl.o : toogl.c++
	$(C++F) -c +d toogl.c++

install: $(TARGETS)
	$(INSTALL) -O -m 555 -idb FULL -u 4Dgifts -g guest -F $(INSTDIR) $(TARGETS)
	$(INSTALL)  -m 0755 -idb BASIC -u 4Dgifts -g guest -dir -F $(PARDIR) toogl
	$(INSTALL) -O -m 444 -idb FULL -u 4Dgifts -g guest -F $(INSTSRC) $(SRC_FILES)
