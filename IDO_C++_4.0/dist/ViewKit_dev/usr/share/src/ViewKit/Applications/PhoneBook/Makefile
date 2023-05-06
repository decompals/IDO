include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lXpm$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lC
LC++FLAGS=$(VKINCLUDEPATH) -g

TARGETS=PhoneBook

OBJECTS= main.o path.o PhoneBook.o Info.o Entries.o Entry.o Data.o \
	EntryDialog.o Preferences.o Print.o PrefDialog.o HelpDialog.o HelpAPI.o

default all: $(TARGETS)

targets: $(TARGETS)

$(TARGETS): $(OBJECTS)
	$(C++) -o $(TARGETS) $(OBJECTS) $(LLDLIBS)

include $(COMMONRULES)
