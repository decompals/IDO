include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lvkhelp$(DBG) -lXpm$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lgl -lm -lC
LC++FLAGS=$(VKINCLUDEPATH) -g

TARGETS= Rotate

OBJECTS= main.o Rotate.o Box.o VkGLX.o \
	copycmap.o installcmap.o

default all: $(TARGETS)

targets: $(TARGETS)

$(TARGETS): $(OBJECTS)
	$(C++) -o $(TARGETS) $(OBJECTS) $(LLDLIBS)

include $(COMMONRULES)
