       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = $(XSLIB) -lXm_s -lXt_s -lX11_s -lPW -lm -lc_s
     OBJS = Dial.o SquareDial.o
    SHELL = /bin/sh
     TGTS = lib dial sqdial
    XSLIB = $(XSLIBDIR)/libXs.a
 XSLIBDIR = ../lib
.PRECIOUS: $(XSLIB)

all:: $(TGTS)

lib::	$(XSLIB)

$(XSLIB):: $(XSLIB)(Dial.o) $(XSLIB)(SquareDial.o)

clean::
	rm -f $(TGTS) $(OBJS) *.out *.bak
