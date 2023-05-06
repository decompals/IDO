include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=tickMarks tickMarks2 tickMarks3

default all: $(TARGETS)

targets: $(TARGETS)

tickMarks: tickMarks.o
	$(C++)  -o tickMarks tickMarks.o $(LLDLIBS)

tickMarks2: tickMarks2.o
	$(C++)  -o tickMarks2 tickMarks2.o $(LLDLIBS)

tickMarks3: tickMarks3.o
	$(C++)  -o tickMarks3 tickMarks3.o $(LLDLIBS)

include $(COMMONRULES)
