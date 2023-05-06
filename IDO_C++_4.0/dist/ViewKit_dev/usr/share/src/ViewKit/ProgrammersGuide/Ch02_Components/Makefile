include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=PanelTest PanelTest2

default all: $(TARGETS)

targets: $(TARGETS)

PanelTest: StartStopPanel.o PanelWindow.o PanelTest.o
	$(C++)  -o PanelTest PanelTest.o PanelWindow.o StartStopPanel.o $(LLDLIBS)

PanelTest2: StartStopPanel.o ControlPanel.o PanelTest2.o
	$(C++)  -o PanelTest2 PanelTest2.o ControlPanel.o StartStopPanel.o $(LLDLIBS)


include $(COMMONRULES)
