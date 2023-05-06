include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11
INVXTLIBS = -lInventorXt
INVLIBS = -lInventor -limage -lfm -lgl
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lvkhelp$(DBG) -lXpm$(DBG)
LLDLIBS=  $(VKLIBS) $(INVXTLIBS) $(MOTIFLIBS) $(INVLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH) -g

TARGETS=IvClock

OBJECTS= main.o VkInventorApp.o IvClock.o Clock.o

default all: $(TARGETS)

targets: $(TARGETS)

$(TARGETS): $(OBJECTS)
	$(C++) -o $(TARGETS) $(OBJECTS) $(LLDLIBS)

include $(COMMONRULES)
