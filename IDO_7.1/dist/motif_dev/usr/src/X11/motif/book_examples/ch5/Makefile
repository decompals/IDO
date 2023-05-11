       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = ../lib/libXs.a -lXm -lXt -lX11 -lPW -lc
     OBJS = mousetracks.o tracker1.o tracker2.o tracker3.o
    SHELL = /bin/sh
     TGTS = select stopwatch trackmouse1 trackmouse2 trackmouse3 xbc xclock
 XSLIBDIR = ../lib

all:: $(TGTS)

trackmouse1: mousetracks.o tracker1.o
	$(CC) -o $@ mousetracks.o tracker1.o $(LDFLAGS)

trackmouse2 : mousetracks.o tracker2.o
	$(CC) -o $@ mousetracks.o tracker2.o $(LDFLAGS)

trackmouse3 : mousetracks.o tracker3.o
	$(CC) -o $@ mousetracks.o tracker3.o $(LDFLAGS)

clean::
	rm -f $(OBJS) $(TGTS) *.out *.bak
