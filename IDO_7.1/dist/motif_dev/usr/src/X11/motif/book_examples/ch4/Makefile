       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = ../lib/libXs.a -lXm -lXt -lX11 -lPW -lc
    SHELL = /bin/sh
     TGTS = chooseone dialog formedit formtest formtest1 generic helptest \
	    menu menu2 menu3 pushbutton rctest traverse twoshells

 XSLIBDIR = ../lib

all:: $(TGTS)

clean::
	rm -f $(TGTS) *.out *.bak
