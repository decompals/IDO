#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = 3dcursor backface boxcirc curve1 curve2 curve3 curved db \
	depthcue doily draw grin hollowcube iobounce maketorus mapw \
	multpl_cursors overlay overlay_cursor paint patch1 pick1 pick2 popup \
	prompt prompt_ginit scrn_rotate select1 shadeclip skeleton spiral \
	text tpbig trans upat vlsi world2scrn worms zbuffer1 zcmapmenu zoing \
	zrgb zrgbmenu

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS	= -lgl -s
LCOPTS	=

# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=

default all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)


MATHLIBLINK = $(CCF) -o $@ $@.o $(LLDLIBS) -lm

doily:  doily.o
	$(MATHLIBLINK)

maketorus:      maketorus.o
	$(MATHLIBLINK)

overlay:   $@.o
	$(CCF) $(LCDEFS) -o $@ $@.o -lgutil $(LLDLIBS) -lm

paint:  paint.o
	$(MATHLIBLINK)

scrn_rotate:    scrn_rotate.o
	$(MATHLIBLINK)

trans:  popuputils.o trans.o
	$(CCF) $(CFLAGS) -o $@ $@.o popuputils.o $(LLDLIBS) -lm

upat:	upat.o upat-main.o
	$(CC) $(CFLAGS) -o $@ $@.o upat-main.o $(LLDLIBS) -lm
