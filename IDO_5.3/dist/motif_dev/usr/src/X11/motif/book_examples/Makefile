#
# Makefile for widget examples
#

  SHELL = /bin/sh
SUBDIRS = lib ch2 ch3 ch4 ch5 ch6 ch7 ch9 ch10 ch11 ch12 ch13 ch14

all clean::
	for D in $(SUBDIRS); do \
		echo "\n===> making \"$@\" in $$D <==="; \
		cd $$D; \
		$(MAKE) $@; \
		cd ..; \
	done

clean::
	rm -f *.out *.bak
