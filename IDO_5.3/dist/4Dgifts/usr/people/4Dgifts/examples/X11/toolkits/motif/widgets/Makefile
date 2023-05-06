SHELL	= /bin/sh
LIBXM	= /usr/lib/libXm.a

ALL	= label list push_button scroll_bar text

# If you want to run a debugger on these programs, remove the -s from LDFLAGS
# and change the -O in CFLAGS to -g.
LDFLAGS	= -lXm -lXt -lX11 -s
CFLAGS  = -O


all .DEFAULT: 
	if [ ! -r ${LIBXM} ] ; then \
		echo No motif library installed ; \
		exit 0 ; \
	fi ; 
	@$(MAKE) $(ALL)

.c:
	cc $(CFLAGS) -o $@ $< $(LDFLAGS)


clobber clean :
	/bin/rm -f $(ALL)
