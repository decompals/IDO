include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm  -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvk$(DBG) -lXpm$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm -lnetls -lnck 
LC++FLAGS=$(VKINCLUDEPATH)
OPTIMIZER = -g

DIRS=\
	AlignTest\
	RadioGroup\
	$(NULL)


TARGETS=\
	runonce\
	runonce2\
	netls\
	list\
	$(NULL)

default all:
	@$(MAKE) targets

headers:

targets: $(TARGETS)
	@-for i in $(DIRS); do \
	    (echo "== "$$i $@; cd $$i; $(MAKE) -e $@); \
	done

include $(COMMONRULES)












