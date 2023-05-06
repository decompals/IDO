#include <stdio.h>
#include <gl/gl.h>

#define RGB_BLACK   0x000000  
#define RGB_RED	    0x0000ff
#define RGB_GREEN   0x00ff00

float v[4][3] = {
    {-3.0,  3.0,  0.0},
    {-3.0, -3.0,  0.0},
    { 2.0, -3.0, -4.0},
    { 2.0,  3.0, -4.0}
};

main()
{
    long xsize, ysize;
    float aspect;   

    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	fprintf(stderr, "Single buffered RGB not available on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("perspective");
    mmode(MVIEWING);
    getsize(&xsize, &ysize);
    aspect = (float)xsize / (float)ysize;
    perspective(900, aspect, 2.0, 5.0);
    RGBmode();
    gconfig();
    cpack(RGB_BLACK);
    clear();
    bgnpolygon();
	cpack(RGB_RED);
	v3f(v[0]);
	v3f(v[1]);
	cpack(RGB_GREEN);
	v3f(v[2]);
	v3f(v[3]);
    endpolygon();
    sleep(10);
    gexit();
    return 0;
}
