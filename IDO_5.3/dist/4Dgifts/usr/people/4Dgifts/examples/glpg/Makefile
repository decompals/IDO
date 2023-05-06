SHELL=/bin/sh
SUBDIRS= ch??

default .DEFAULT: 
	for d in ${SUBDIRS}; do \
	if [ -d $$d ]; then \
		echo "====\tcd $$d; ${MAKE} $@"; \
		(cd $$d; ${MAKE} $@); \
	fi \
	done
