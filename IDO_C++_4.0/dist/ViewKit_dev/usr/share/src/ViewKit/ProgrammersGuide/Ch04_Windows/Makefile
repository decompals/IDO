include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=scaleApp radioApp radioApp2 colors

default all: $(TARGETS)

targets: $(TARGETS)

scaleApp: scaleApp.o ScaleWindow.o
	$(C++)  -o scaleApp scaleApp.o ScaleWindow.o $(LLDLIBS)

radioApp: radioApp.o RadioWindow.o
	$(C++)  -o radioApp radioApp.o RadioWindow.o $(LLDLIBS)

radioApp2: radioApp2.o RadioWindow2.o
	$(C++)  -o radioApp2 radioApp2.o RadioWindow2.o $(LLDLIBS)

colors: colors.o ColorWindow.o
	$(C++)  -o colors colors.o ColorWindow.o $(LLDLIBS)


include $(COMMONRULES)
