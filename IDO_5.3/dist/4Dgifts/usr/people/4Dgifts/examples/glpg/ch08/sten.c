#include <stdio.h>
#include <gl/gl.h>

float	rect0[4][2] = {
	{-0.5, -0.5},
	{ 0.5, -0.5},
	{ 0.5,  0.5},
	{-0.5,  0.5},
	};

main()
{
    if (getgdesc(GD_BITS_STENCIL) == 0) {
	fprintf(stderr, "stencil not available on this machine\n");
	return 1;
    }

    prefsize(400, 400);
    winopen("stencil");
    RGBmode();
    stensize(3);
    gconfig();
    mmode(MVIEWING);
    ortho2(-10.0, 10.0, -10.0, 10.0);

    cpack(0);
    clear();
    sclear(0);

    wmpack(0);
    stencil(1, 0x0, SF_ALWAYS, 0x7, ST_INCR, ST_INCR, ST_INCR);
    viewport(0, 398, 0, 398);
    checker();
    viewport(1, 399, 0, 398);
    checker();
    viewport(1, 399, 1, 399);
    checker();
    viewport(0, 398, 1, 399);
    checker();

    stencil(1, 0x0, SF_NOTEQUAL, 0x3, ST_KEEP, ST_KEEP, ST_KEEP);
    wmpack(0xffffffff);
    scale(15.0, 15.0, 0.0);
    bgnpolygon();
	v2f(rect0[0]);
	v2f(rect0[1]);
	v2f(rect0[2]);
	v2f(rect0[3]);
    endpolygon();

    sleep(10);
    gexit();
    return 0;
}

checker()
{
    int	i,j;

    cpack(0x0000ff00);
    pushmatrix();
    translate(-5.0, -5.0, 0.0);
    for (i=0; i<9; i++) {
	for (j=0; j<9; j++) {
	    translate(1.0, 0.0, 0.0);
	    if ((i^j) & 0x1) {		/* checker board */
		bgnpolygon();
		    v2f(rect0[0]);
		    v2f(rect0[1]);
		    v2f(rect0[2]);
		    v2f(rect0[3]);
	        endpolygon();
	    }
	}
	translate(-9.0, 1.0, 0.0);
    }
    popmatrix();
}
