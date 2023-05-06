#!smake
# $Id: Makefile,v 1.3 1994/07/25 18:29:13 dpb Exp $

INCFLAGS = -I../ -I../..

GFLAGS = -g -fullwarn -wlint -woff 813,826,852
OFLAGS = 

CFLAGS = $(INCFLAGS) $(GFLAGS) $(OFLAGS)

DSO_LD_FLAGS = \
	-shared \
	-all \
	-no_unresolved \
	-check_registry /usr/lib/so_locations \
	-soname libsv.so \
	-set_version sgi1.0

DSO_LIBS = -lvl -ldmedia -limage -lgl -lc

default: libsv.so

libsv.a: svUtil.o vlUtil.o svImage.o
	ar cruv libsv.a svUtil.o svImage.o vlUtil.o

libsv.so: libsv.a
	   ld $(DSO_LD_FLAGS) libsv.a -o libsv.so $(DSO_LIBS)

clean:
	rm -f *.o *.a *.so *~ core Makedepend

clobber: clean

vlUtil.c: ../../vlUtil/vlUtil.c
	-rm -f $@
	-cp -p $? $@
	-chmod ugo-w $@
	