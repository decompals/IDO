include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm 
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=cbrowser

all: $(TARGETS)

targets: $(TARGETS)

cbrowser: main.o CBrowser.o Data.o List.o Area.o Overview.o \
	Element.o RepeatButton.o Modified.o TextIO.o Resizer.o \
	Scroll.o Graph.o PrefItem.o ListSearch.o CompletionField.o \
	CheckBox.o RadioBox.o Deck.o GangedGroup.o Meter.o Pie.o \
	Outline.o FileSet.o TabPanel.o VuMeter.o TickMarks.o
	$(C++)  -o cbrowser main.o CBrowser.o Data.o List.o Area.o Overview.o \
	Element.o RepeatButton.o Modified.o TextIO.o Resizer.o \
	Scroll.o Graph.o PrefItem.o ListSearch.o CompletionField.o \
	CheckBox.o RadioBox.o Deck.o GangedGroup.o Meter.o Pie.o \
	Outline.o FileSet.o TabPanel.o VuMeter.o TickMarks.o\
	$(LLDLIBS)

include $(COMMONRULES)
