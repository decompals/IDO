include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=notBusy MapMaker

default all: $(TARGETS)

targets: $(TARGETS)

notBusy: notBusy.o
	$(C++)  -o notBusy notBusy.o $(LLDLIBS)

MapMaker: MapMaker.o
	$(C++)  -o MapMaker MapMaker.o $(LLDLIBS)

include $(COMMONRULES)
