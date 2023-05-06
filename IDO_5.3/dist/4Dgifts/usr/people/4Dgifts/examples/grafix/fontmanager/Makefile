#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS	= `ls *.c | sed 's/\.c$$//'`

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and change the -O in LCDEFS to -g.
LLDLIBS = -lfm -lgl -s
LCDEFS  = -O

# override the prototypes macro in commondefs:  if you want 
# prototyping turned on, comment out the next line.
PROTOTYPES=


default all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)


MATHLIBLINK = $(CC) $(CFLAGS) -o $@ $@.c $(LDFLAGS) -lm 

speedtest: speedtest.c
	$(MATHLIBLINK)
