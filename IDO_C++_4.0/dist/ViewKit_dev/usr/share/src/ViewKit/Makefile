include $(ROOT)/usr/include/make/commondefs

DIRS=\
	Applications\
	Basic\
	Communication\
	ComponentDemos\
	Components\
	Communication\
	Dialogs\
	Icons\
	Menus\
	Preferences\
	Process\
	Utilities\
	$(NULL)

C++FILES=	\
	$(NULL)

HEADERS=	\
	$(NULL)

TARGETS=\
	$(NULL)

default clobber all:
	@-for i in $(DIRS); do \
	    (echo "== "$$i $@; cd $$i; $(MAKE) -e $@); \
	done

headers:
targets: $(TARGETS)

include $(COMMONRULES)












