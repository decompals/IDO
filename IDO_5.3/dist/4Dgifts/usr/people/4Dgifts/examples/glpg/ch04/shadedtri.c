#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

float blackvect[3]  = {0.0, 0.0, 0.0};
float redvect[3]    = {1.0, 0.0, 0.0};
float greenvect[3]  = {0.0, 1.0, 0.0};
float bluevect[3]   = {0.0, 0.0, 1.0};

long triangle[3][2] = {
    { 20,  20},
    { 20, 380},
    {380,  20}
};

main()
{
    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	fprintf(stderr, "Single buffered RGB not available on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("shadedtri");
    RGBmode();
    gconfig();

    c3f(blackvect);
    clear();
    bgnpolygon();
	c3f(redvect);
	v2i(triangle[0]);
	c3f(greenvect);
	v2i(triangle[1]);
	c3f(bluevect);
	v2i(triangle[2]);
    endpolygon();
    sleep(10);
    gexit();
    return 0;
}
