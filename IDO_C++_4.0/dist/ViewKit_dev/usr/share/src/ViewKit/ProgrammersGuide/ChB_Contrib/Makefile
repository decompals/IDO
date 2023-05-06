include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=outline outline2

default all: $(TARGETS)

targets: $(TARGETS)

outline: outline.o
	$(C++)  -o outline outline.o $(LLDLIBS)

outline2: outline2.o
	$(C++)  -o outline2 outline2.o $(LLDLIBS)

include $(COMMONRULES)
