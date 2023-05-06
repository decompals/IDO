SHELL=/bin/sh
SUBDIRS= *

all .DEFAULT: 
	for d in ${SUBDIRS}; do \
	if [ -d $$d ]; then \
		( cd $$d; \
		if [ -f Makefile ]; then \
			echo "====\t `pwd`  ${MAKE} $@ "; \
			${MAKE} $@  ; \
		elif [ -f Makefile.basic ]; then \
			echo "====\t `pwd`  ${MAKE} -f Makefile.basic $@ "; \
			${MAKE} -f Makefile.basic $@ ;  \
		fi \
		); \
	fi \
	done

