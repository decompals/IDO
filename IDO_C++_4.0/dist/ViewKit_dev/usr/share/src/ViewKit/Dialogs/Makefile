include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvk$(DBG) -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm 
LC++FLAGS=$(VKINCLUDEPATH)

DIRS=\
	DialogDemo\
	$(NULL)


C++FILES=	\
	$(NULL)

HEADERS=	\
	$(NULL)


TARGETS= custom\
	fileSBdialog\
	question\
	dialog\
	$(NULL)

default all:
	@$(MAKE) targets

headers:

targets: $(TARGETS)
	@-for i in $(DIRS); do \
	    (echo "== "$$i $@; cd $$i; $(MAKE) -e $@); \
	done

include $(COMMONRULES)












