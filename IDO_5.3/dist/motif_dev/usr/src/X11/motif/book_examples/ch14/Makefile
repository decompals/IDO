       CC = cc 
   CFLAGS = $(DEBUG) -cckr -I$(INC)/X11 -DSYSV  -I$(XSLIBDIR)
    DEBUG = -g
      INC = /usr/include
  LDFLAGS = $(XSLIB) -lXm_s -lXt_s -lX11_s -lPW -lc_s
     OBJS = Tree.o
    SHELL = /bin/sh
     TGTS = lib sort
    XSLIB = $(XSLIBDIR)/libXs.a
 XSLIBDIR = ../lib
.PRECIOUS: $(XSLIB)

all:: $(TGTS)

lib:: $(XSLIB)

$(XSLIB):: $(XSLIB)(Tree.o)

sort : sort.c $(XSLIB)
	$(CC) $(CFLAGS) -o $@ $@.c $(LDFLAGS)

clean::
	rm -f $(TGTS) $(OBJS) *.out *.bak
