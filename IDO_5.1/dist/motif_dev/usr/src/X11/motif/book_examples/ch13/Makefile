       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = $(XSLIB) -lXm_s -lXt_s -lX11_s -lPW -lc_s
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
