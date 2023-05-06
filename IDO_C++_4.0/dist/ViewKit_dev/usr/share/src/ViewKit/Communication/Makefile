include $(ROOT)/usr/include/make/commondefs

MOTIFLIBS = -lSgm -lXm -lXt -lX11 -lPW
VKLIBS =  $(VKLIBPATH) -lvkmsg$(DBG) -lvk$(DBG) -lvkhelp$(DBG) -ltt -lsun
LLDLIBS=  $(VKLIBS) $(MOTIFLIBS) -lm 
LC++FLAGS=$(VKINCLUDEPATH)

OPTIMIZER=-O

C++FILES = \
	sample1.c++ \
	sample2_client.c++ \
	sample2_service.c++ \
	$(NULL)

HEADERS=	\
	$(NULL)

TARGETS= \
	sample1 \
	sample2_client \
	sample2_service \
	sample2_ptypes.xdr \
	$(NULL)

SERVICES= \
	sample2_service \
	$(NULL)

default all:
	@$(DOINCDEPEND)
	@$(MAKE) targets

headers:

targets: $(TARGETS)

include $(COMMONRULES)

sample2_ptypes.xdr:
	/bin/echo "Make sure you have the directory .tt in your home directory"
	tt_type_comp sample2_ptypes

