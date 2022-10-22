LLDLIBS = -lc
LCDEFS=-g

rantest: rantest.c
	cc $(LCDEFS) -o rantest rantest.c $(LLDLIBS)

rantest_u: rantest.c
	cc $(LCDEFS) -o rantest_u rantest.c -lc

clean:
	rm -f rantest rantest_u

clobber:
	rm -f rantest rantest_u
