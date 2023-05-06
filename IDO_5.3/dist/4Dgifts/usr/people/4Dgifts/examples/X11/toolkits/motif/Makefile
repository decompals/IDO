SHELL=/bin/sh
SUBDIRS= *

LIBXM=/usr/lib/libXm.a

all .DEFAULT: 
	if [ ! -r ${LIBXM} ] ; then \
		echo No motif library installed ; \
		exit 0 ; \
	fi ; \
	for d in ${SUBDIRS}; do \
	if [ -d $$d ]; then \
		echo "=====\tcd $$d; ${MAKE} $@"; \
		(cd $$d; ${MAKE} $@); \
	fi \
	done
