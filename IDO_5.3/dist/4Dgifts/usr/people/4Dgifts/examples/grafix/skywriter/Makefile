#!smake

include $(ROOT)/usr/include/make/commondefs

#
# Simplified makefile for skyfly demo/sample application
#
TARGETS = skyfly
HFILES =  skyfly.h
CFILES	= skyfly.c database.c fly.c  read_bwimage.c  perfdraw.c xinput.c
DATAFILES = terrain.bw clouds.bw

LLDLIBS = -lX11 -limage -lgutil -lgl -lm -lmpc -lc
LCDEFS	= -O2

# If you want prototypes turned off, un-comment the following line:
# PROTOTYPES=

default all: $(TARGETS) $(DATAFILES)

include $(COMMONRULES)

${CFILES}: ${HFILES}

$(TARGETS): $(OBJECTS)
	$(CCF) $(OBJECTS) $(LDFLAGS) -o $@

# Note: It is difficult to place binary files
# under RCS control.  Uuencoding them to make
# safe asci files is one solution.

terrain.bw: terrain.uu
	uudecode terrain.uu

clouds.bw: clouds.uu
		uudecode clouds.uu

