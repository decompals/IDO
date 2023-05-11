       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = ../lib/libXs.a -lXm -lXt -lX11 -lPW -lc
    SHELL = /bin/sh
     TGTS = fileview fileview2 fileview3 fileview4
 XSLIBDIR = ../lib

all:: $(TGTS)

clean::
	rm -f $(TGTS) *.out *.bak
