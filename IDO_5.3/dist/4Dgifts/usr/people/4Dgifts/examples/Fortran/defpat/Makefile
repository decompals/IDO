SHELL=/bin/sh
SUBDIRS= *

all .DEFAULT: 
	for d in ${SUBDIRS}; do \
	if [ -d $$d ]; then \
		( cd $$d; if [ -f Makefile ]; then \
			echo "====\t `pwd`; ${MAKE} $@"; \
			${MAKE} $@  ; \
			fi \
		); \
	fi \
	done
