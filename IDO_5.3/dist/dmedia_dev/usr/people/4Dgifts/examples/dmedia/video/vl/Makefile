#!/usr/sbin/smake
# $Revision: 1.17 $

include /usr/include/make/commondefs

LLDLIBS = -lvl -lX11 -limage -lgl
# OPTIMIZER = -g
LD_QUICKSTART_INFO=

#
# The following two programs are Galileo-specific and depend on the 
# header <dmedia/vl_ev1.h> which is only shipped with the galileo
# product.
# Add them to TARGETS if you have installed this product on your system.
#
#   simpleblend simplewipe
#
TARGETS=contcapt eventex simpleccapt simplegrab \
	simplem2v simplev2s simplev2v \
	videoin vidtomem mtov

default: $(TARGETS)

mtov: mtov.c
	cc -o $@ mtov.c -lvl -lmovie -ldmedia -lcl -lawareaudio -laudio -lgl -lm -lXext -lX11

include $(COMMONRULES)

