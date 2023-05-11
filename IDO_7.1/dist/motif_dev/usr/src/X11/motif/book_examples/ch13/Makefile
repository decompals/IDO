       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = $(XSLIB) -lXm -lXt -lX11 -lPW -lc
     OBJS = Row.o
    SHELL = /bin/sh
     TGTS = lib rowtest
    XSLIB = $(XSLIBDIR)/libXs.a
 XSLIBDIR = ../lib
.PRECIOUS: $(XSLIB)

all:: $(TGTS)

lib::	$(XSLIB)

$(XSLIB):: $(XSLIB)(Row.o)

clean::
	rm -f $(TGTS) $(OBJS) *.bak
