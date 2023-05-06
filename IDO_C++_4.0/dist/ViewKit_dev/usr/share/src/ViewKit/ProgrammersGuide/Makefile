include $(ROOT)/usr/include/make/commondefs

DIRS=Ch04_Windows\
      	Ch11_Input\
	Ch02_Components\
	Ch09_Graph\
	ChB_Contrib\
	Ch03_App\
	Ch10_Display\
	$(NULL)

C++FILES=	\
	$(NULL)

HEADERS=	\
	$(NULL)

TARGETS=\
	$(NULL)

default all:
	@-for i in $(DIRS); do \
	    (echo "== "$$i $@; cd $$i; $(MAKE) -e $@); \
	done

headers:
targets: $(TARGETS)

include $(COMMONRULES)












