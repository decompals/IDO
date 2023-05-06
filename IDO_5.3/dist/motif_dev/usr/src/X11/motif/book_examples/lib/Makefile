       CC = cc 
   CFLAGS = $(DEBUG) -cckr -DSYSV -I/usr/include/X11
    DEBUG = -O
     OBJS = concat.o help_dialog.o insert.o invert.o menus.o pix_buttons.o \
	    quit.o send_msg.o str2flt.o strtoxmstr.o talkto.o wprintf.o \
	    xmstr_to_str.o xor.o
    SHELL = /bin/sh
     TGTS = libXs.a

all:: $(TGTS)

libXs.a: $(OBJS)
	rm -f $@
	ar scq $@ $(OBJS)

clean::
	rm -f $(TGTS) $(OBJS) *.out *.bak
