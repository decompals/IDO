#!/usr/sbin/smake
# $Revision: 1.3 $

include /usr/include/make/commondefs

OBJECT_STYLE=32
LLDLIBS = -lvl -limage
# OPTIMIZER = -g
LD_QUICKSTART_INFO=

TARGETS=contcapt vidtomem mtov showgl

default: $(TARGETS)

mtov: mtov.c
	$(CCF) -o $@ mtov.c -lvl -lmovie -ldmedia -lcl -lawareaudio -laudio \
		-lX11 -lgl -lm

showgl: showgl.c
	$(CCF) -o $@ showgl.c -lGLw -lGL -lXm -lXt -lX11 -limage

contcapt:
	$(CCF) -o $@ contcapt.c -lvl -lGLw -lGL -lXm -lXt -lX11 
	

include $(COMMONRULES)

