include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvk$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm
LC++FLAGS=$(VKINCLUDEPATH)

OPTIMIZER=-g

C++FILES=\
	$(NULL)

HEADERS=	\
	$(NULL)


TARGETS=busy\
	callback\
	hello\
	stopwatch\
	cmdline\
	cmdline2\
	generic\
	generic2\
	wmquit\
	afterRealize\
	$(NULL)

default all:
	@$(MAKE) targets

headers:

targets: $(TARGETS)

include $(COMMONRULES)












