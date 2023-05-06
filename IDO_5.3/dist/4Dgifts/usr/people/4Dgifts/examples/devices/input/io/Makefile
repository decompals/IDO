# Makefile for 4Dgifts input drivers

$(STATIC)DYNAMIC=XXX

$(STATIC)SRCS = idev.c mouse.c keybrd.c pcmouse.c pckbd.c
$(STATIC)OBJS = idev.o mouse.o keybrd.o pcmouse.o pckbd.o
$(DYNAMIC)SRCS= tablet.c dial.c sball.c hitachi.c imp.c
$(DYNAMIC)OBJS= tablet.o dial.o sball.o hitachi.o imp.o

CC = /usr/bin/cc
LD = /usr/bin/ld

$(DYNAMIC)DCOPTS=-Wx,-G0 -Wc,-jalr
$(STATIC)DYNAMICTARGETS=makedynamic

CDEFS=-DDEBUG -D_K32U32 -D_KERNEL
COPTS=-non_shared -coff -O -Wc,-pic0 -MDupdate Makedepend -prototypes $(DCOPTS)
CINCS=-nostdinc -I.. -I/usr/include
CFLAGS=$(CDEBUGFLAGS) -xansi $(CDEFS) $(COPTS) $(CINCS)

LDFLAGS=-G $(GNUMVAL) -coff -non_shared -r
$(STATIC)GNUMVAL=8
$(DYNAMIC)GNUMVAL=0

all: $(OBJS) $(DYNAMICTARGETS)

# build dynamic objects seperately since they require additional COPTS
makedynamic: $(DYNAMICSRC)
	make STATIC=no dynamicobjs

dynamicobjs: $(OBJS)

.c.o:
	$(CC) $(CFLAGS) -c $*.c &&		\
	$(LD) $(LDFLAGS) $*.o -o $$$$.o	&&	\
	mv $$$$.o $*.o

clean:
	/bin/rm -f *.a *.o *.out *.bak
