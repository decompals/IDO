#
include ${ROOT}/usr/include/make/commondefs
 
TARGETS = cube.x cube.gl

XOBJS = cube.x.o glxhelper.o
GLOBJS = cube.gl.o

#LCOPTS = -g
#
# libXi.a is the X Input extension library.
# libXext.a is the X extension library.
# These are needed only for cube.x, not cube.gl.
#
LLDLIBS = -lXi -lXext -lX11 -lspaceball -lgl -lm

# If you can't use prototype checking, uncomment the following line.
#PROTOTYPES=

default all: $(TARGETS)

cube.x: $(XOBJS)
	$(CC) -o $@ $(XOBJS) $(LDFLAGS)

cube.gl: $(GLOBJS)
	$(CC) -o $@ $(GLOBJS) $(LDFLAGS)

#
# If cube.c is compiled with -DUSE_XINPUT, the result is a "mixed model"
# program, where the GL is used for imaging, but events are read from X.
#
cube.x.o: cube.c
	$(CC) $(CFLAGS) -c -DUSE_XINPUT $? -o $@

cube.gl.o: cube.c
	$(CC) $(CFLAGS) -c $? -o $@

include ${COMMONRULES}
