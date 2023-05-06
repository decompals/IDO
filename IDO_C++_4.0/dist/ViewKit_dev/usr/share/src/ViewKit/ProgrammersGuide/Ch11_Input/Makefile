include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=tabHoriz tabVert

default all: $(TARGETS)

targets: $(TARGETS)


tabVert: tabVert.o
	$(C++)  -o tabVert tabVert.o $(LLDLIBS)

tabHoriz: tabHoriz.o
	$(C++)  -o tabHoriz tabHoriz.o $(LLDLIBS)


include $(COMMONRULES)
