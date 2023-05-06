#include <stdio.h>
#include <gl/gl.h>

float mt0[3][3] = {		/* mountain 0 coordinates */
    {-15.0, -10.0, -15.0},
    { 10.0, -10.0, -15.0},
    { -5.0,   5.0, -15.0},
};

float mt1[3][3] = {		/* mountain 1 coordinates */
    {-10.0, -10.0, -17.0},
    { 15.0, -10.0, -17.0},
    {  6.0,  12.0, -17.0},
};

float bldg[4][3] = {		/* building coordinates */
    {-8.0, -10.0, -12.0},
    { 8.0, -10.0, -12.0},
    { 8.0,  10.0, -12.0},
    {-8.0,  10.0, -12.0},
};

float tbldg[4][2] = {		/* building texture coordinates */
    {0.0, 1.0},
    {1.0, 1.0},
    {1.0, 0.0},
    {0.0, 0.0},
};

/*
 * Building texture and texture environment
 */

unsigned long	bldgtex[8*4] = {
    0xffffffff, 0xffff0000, 0x00000000, 0x00000000,
    0xffffffff, 0xffff0000, 0x0000ffcf, 0xffcfffcf,
    0xffff0000, 0xffff0000, 0x0000ffcf, 0x0000ffcf,
    0xffffffff, 0xffffffff, 0xffffffcf, 0xffcfffcf,
    0xffff0000, 0xffffffff, 0xffffffcf, 0x0000ffcf,
    0xffffffff, 0xffffffff, 0xffffffcf, 0xffcfffcf,
    0xffff0000, 0xffff0000, 0x0000ffcf, 0x0000ffcf,
    0xffffffff, 0xffff0000, 0x0000ffcf, 0xffcfffcf,
};

float txlist[] = {TX_MAGFILTER, TX_POINT, TX_NULL};
float tvlist[] = {TV_NULL};

main()
{
    if (getgdesc(GD_BITS_NORM_ZBUFFER) == 0) {
	fprintf(stderr, "Z-buffer not available on this machine\n");
	return 1;
    }
    if (getgdesc(GD_TEXTURE) == 0) {
	fprintf(stderr, "Texture mapping not available on this machine\n");
	return 1;
    }
    if (getgdesc(GD_AFUNCTION) == 0) {
	fprintf(stderr, "afunction not available on this machine\n");
	return 1;
    }

    prefsize(400, 400);
    winopen("afunction");
    RGBmode();
    gconfig();
    mmode(MVIEWING);
    ortho(-20.0, 20.0, -20.0, 20.0, 10.0, 20.0);
    zbuffer(TRUE);
    czclear(0, getgdesc(GD_ZMAX));

    /*
     * Draw 2 mountains
     */
    cpack(0xff3f703f);
    bgnpolygon();
	v3f(mt0[0]);
	v3f(mt0[1]);
	v3f(mt0[2]);
    endpolygon();

    cpack(0xff234f00);
    bgnpolygon();
	v3f(mt1[0]);
	v3f(mt1[1]);
	v3f(mt1[2]);
    endpolygon();

    /*
     * Draw the building
     */
    texdef2d(1, 2, 8, 8, bldgtex, 0, txlist);
    tevdef(1, 0, tvlist);
    texbind(TX_TEXTURE_0, 1);
    tevbind(TV_ENV0, 1);
    afunction(4, AF_GEQUAL);
    cpack(0xffffffff);
    bgnpolygon();
	t2f(tbldg[0]);
	v3f(bldg[0]);
	t2f(tbldg[1]);
	v3f(bldg[1]);
	t2f(tbldg[2]);
	v3f(bldg[2]);
	t2f(tbldg[3]);
	v3f(bldg[3]);
    endpolygon();
    sleep(10);
    gexit();
    return 0;
}
