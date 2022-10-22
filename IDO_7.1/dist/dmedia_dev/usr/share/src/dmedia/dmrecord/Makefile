include $(ROOT)/usr/include/make/commondefs

CFILES   = \
	main.c \
	cosmo_capture.c \
	parseargs.c \
	$(NULL)

COBJS = $(CFILES:.c=.o)

TARGETS = dmrecord

LLDLIBS = -lcl -lvl -ldmedia -lmovie -lgl -lX11 -laudio -lm -lgen

default all: $(TARGETS)

include $(COMMONRULES)


dmrecord: $(COBJS)
	$(CC) -o $@ $(COBJS) $(LDFLAGS) $(LDFLAGS)
