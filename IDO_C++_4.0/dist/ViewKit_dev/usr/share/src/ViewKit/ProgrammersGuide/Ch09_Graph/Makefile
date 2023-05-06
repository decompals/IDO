include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=sampleGraph

default all: $(TARGETS)

targets: $(TARGETS)

sampleGraph: sampleGraph.o
	$(C++)  -o sampleGraph sampleGraph.o $(LLDLIBS)

include $(COMMONRULES)
