       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = ../lib/libXs.a -lXm -lXt -lX11 -lPW -lc
    SHELL = /bin/sh
     TGTS = memo memo1 memo2 memo3 memo4 memo5
 XSLIBDIR = ../lib

all:: $(TGTS)

clean::
	rm -f $(TGTS) *.out *.bak
