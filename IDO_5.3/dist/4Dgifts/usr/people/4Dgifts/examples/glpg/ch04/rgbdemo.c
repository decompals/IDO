#include <stdio.h>
#include <gl/gl.h>

#define R	0
#define G	1
#define B	2
#define RGB	3

#define	XSPACING    120	    /* strwidth(" 255 255 255 ") */
#define	SIZE	    50
#define	SEP	    30
#define	YSPACING    (SIZE + SEP)

short whitevec[RGB] = {255, 255, 255};
short blackvec[RGB] = {0, 0, 0};

main()
{
    int i, j, k;
    Icoord majorx, majory;
    long xoff, yoff;
    char str[20];
    short rgbvec[RGB];

    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	fprintf(stderr, "Single buffered RGB not available on this machine\n");
	return 1;
    }
    prefsize(8*XSPACING + SEP, 8*YSPACING + SEP);
    winopen("rgbdemo");
    RGBmode();
    gconfig();
    c3s(blackvec);
    clear();

    yoff = getheight();
    for (i = 0; i < 4; i++) {
	rgbvec[R] = i*255/3;
	if (i < 2)
	    majory = SEP;
	else
	    majory = SEP + YSPACING*4;
	if (i == 0 || i == 2)
	    majorx = SEP;
	else
	    majorx = SEP + XSPACING*4;
	for (j = 0; j < 4; j++) {
	    rgbvec[G] = j*255/3;
	    for (k = 0; k < 4; k++) {
		rgbvec[B] = k*255/3;
		c3s(rgbvec);
		sboxfi(majorx + XSPACING*j,	   majory + YSPACING*k,
		       majorx + XSPACING*j + SIZE, majory + YSPACING*k + SIZE);
		c3s(whitevec);
		sprintf(str, "%d %d %d", rgbvec[R], rgbvec[G], rgbvec[B]);
		xoff = (strwidth(str) - SIZE)/2;
		cmov2i(majorx + XSPACING*j - xoff, 
		       majory + YSPACING*k - yoff);
		charstr(str);
	    }
	}
    }
    sleep(10);
    gexit();
    return 0;
}
