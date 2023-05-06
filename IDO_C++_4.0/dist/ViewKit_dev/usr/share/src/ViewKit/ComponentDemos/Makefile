include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH)  -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm 
LC++FLAGS=$(VKINCLUDEPATH)

DIRS=\
	$(NULL)


C++FILES=	\
	$(NULL)

HEADERS=	\
	$(NULL)


TARGETS=graph\
	NodesDemo\
	checkbox\
	complete\
	resizer\
	modified\
	outliner\
	outliner_asb\
	scrolledbox\
	$(NULL)

default all:
	@$(MAKE) targets

headers:

targets: $(TARGETS)


include $(COMMONRULES)












