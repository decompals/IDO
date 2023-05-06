CFLAGS = -O2 -I$(ROOT)/usr/include
LFLAGS = -L$(ROOT)/usr/lib
ALL= mpegplay
 
all: $(ALL)

.c.o:
	cc $(CFLAGS) -c $<

clean:
	rm -f mpegplay.o

clobber: clean
	rm -f mpegplay

mpegplay: mpegplay.o 
	cc $(LFLAGS) mpegplay.o -o mpegplay -lgl -lcl -lawareaudio -lm
