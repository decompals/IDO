include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvk$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm
LC++FLAGS=$(VKINCLUDEPATH)

TARGETS=rolodex

all: $(TARGETS)

targets: $(TARGETS)

rolodex: main.o Rolodex.o  LabeledText.o Address.o Command.o  Database.o
	$(C++)  -o rolodex main.o  Rolodex.o  LabeledText.o Address.o Command.o Database.o  $(LLDLIBS)


include $(COMMONRULES)
