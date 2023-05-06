include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lSgw -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH)  -lvk_d -lvkhelp$(DBG)
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm 
LC++FLAGS=$(VKINCLUDEPATH)

DIRS=\
	Rolodex\
	CBrowser\
	$(NULL)

C++FILES= meter.c++	\
	$(NULL)

HEADERS=	\
	$(NULL)

TARGETS= meter \
	pie\
	$(NULL)

default all:
	@$(MAKE) targets

headers:

targets: $(TARGETS)
	@-for i in $(DIRS); do \
	    (echo "== "$$i $@; cd $$i; $(MAKE) -e $@); \
	done


include $(COMMONRULES)












