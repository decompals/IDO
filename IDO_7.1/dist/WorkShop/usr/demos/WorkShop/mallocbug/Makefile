LLDLIBS = -lc
LCDEFS=-g

mallocbug: mallocbug.c
	cc $(LCDEFS) -o mallocbug mallocbug.c $(LLDLIBS)

mallocbug_u: mallocbug.c
	cc $(LCDEFS) -o mallocbug_u mallocbug.c -lc

mallocbug_cv: mallocbug.c
	cc $(LCDEFS) -o mallocbug_cv mallocbug.c -lmalloc_cv $(LLDLIBS)

corrupt:
	cc $(LCDEFS) -o corrupt corrupt.c -lmalloc_cv $(LLDLIBS)

clean:
	rm -f mallocbug mallocbug_u mallocbug_cv corrupt

clobber:
	rm -f mallocbug mallocbug_u mallocbug_cv corrupt
