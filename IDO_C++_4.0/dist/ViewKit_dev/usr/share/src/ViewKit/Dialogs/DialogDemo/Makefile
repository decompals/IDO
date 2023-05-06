include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvk$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm 
LC++FLAGS=$(VKINCLUDEPATH)

all: dialogTest

dialogTest: dialogTest.o DialogPosterUI.o DialogPoster.o 
	$(C++) -o dialogTest  dialogTest.o DialogPosterUI.o DialogPoster.o $(LDLIBS)


DialogPoster.o: DialogPoster.c++  DialogPoster.h  DialogPosterUI.h  
dialogTest.o: DialogPoster.h  DialogPosterUI.h  

targets: all


include $(COMMONRULES)
