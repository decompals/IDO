
SHELL = /bin/sh
CC = /usr/bin/cc

SRCS = inputtest.c idev.c \
	mouse.c keybrd.c tablet.c dial.c sball.c hitachi.c imp.c
OBJS =  inputtest.o idev.o \
	mouse.o keybrd.o tablet.o dial.o sball.o hitachi.o imp.o

CDEFS = -DDEBUG -DINPUT_TEST
COPTS = -cckr -g -prototypes -MDupdate Makedepend
CINCS = -nostdinc -I.. -I/usr/include
CFLAGS = $(CDEBUGFLAGS) $(CDEFS) $(COPTS) $(CINCS)

all:	inputtest

inputtest:	$(OBJS)
	$(CC) $(CFLAGS) -o inputtest $(OBJS)

idev.c:	../io/idev.c
	ln -s ../io/idev.c idev.c

mouse.c:	../io/mouse.c
	ln -s ../io/mouse.c mouse.c

keybrd.c:	../io/keybrd.c
	ln -s ../io/keybrd.c keybrd.c

dial.c:	../io/dial.c
	ln -s ../io/dial.c dial.c

tablet.c:	../io/tablet.c
	ln -s ../io/tablet.c tablet.c

sball.c:	../io/sball.c
	ln -s ../io/sball.c sball.c

hitachi.c:	../io/hitachi.c
	ln -s ../io/hitachi.c hitachi.c

imp.c:	../io/imp.c
	ln -s ../io/imp.c imp.c

clean:
	-/bin/rm -f *.a *.o *.out *.bak *.log inputtest
	-/bin/rm -f idev.c mouse.c keybrd.c tablet.c dial.c sball.c hitachi.c imp.c
