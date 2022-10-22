DRIVER_SRCS =		\
	idev.c		\
	mouse.c		\
	keyboard.c	\
	tablet.c	\
	dial.c		\
	sball.c		\
	hitachi.c	\
	imp.c

SRCS = inputtest.c $(DRIVER_SRCS)
OBJS = $(SRCS:.c=.o)

CDEFS = -DDEBUG -DINPUT_TEST
COPTS = -g -MDupdate Makedepend
CINCS = -I..
CFLAGS = $(CDEBUGFLAGS) $(CDEFS) $(COPTS) $(CINCS)

all:  inputtest

inputtest:  $(OBJS)
	$(CC) $(CFLAGS) -o inputtest $(OBJS)

$(DRIVER_SRCS):  ../$$@
	ln -s ../$@ $@

clean:
	rm -f inputtest $(OBJS) $(DRIVER_SRCS) Makedepend
