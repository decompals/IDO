#!smake
#
#  This is a driver makefile for the CITE tests
#
#.SHELL = /bin/sh

#
# These are the tests to be executed
#

CITETESTLIST = afold artdif cholesky dflux4 dflux5 dflux6 \
               dgetf3 dmxpy gmtry inlining-btrix interchange jacobi livrm \
               lu  mm prefetch shallow sor1k vpenta


CURRENT_DIR != pwd

tests: 
	-for TESTDIR in $(CITETESTLIST); do \
	  if [ -d $$TESTDIR ] ; then \
	  echo $$TESTDIR ====================== ; \
	  ( cd $$TESTDIR; \
	    smake ) ;  \
	  fi; done

clean:
	 for TESTDIR in $(CITETESTLIST); do (cd $$TESTDIR; smake clean) ; done
