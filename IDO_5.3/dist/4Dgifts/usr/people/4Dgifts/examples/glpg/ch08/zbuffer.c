#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

float v[8][3] = {
    {-1.0, -1.0, -1.0},
    {-1.0, -1.0,  1.0},
    {-1.0,  1.0,  1.0},
    {-1.0,  1.0, -1.0},
    { 1.0, -1.0, -1.0},
    { 1.0, -1.0,  1.0},
    { 1.0,  1.0,  1.0},
    { 1.0,  1.0, -1.0},
};
unsigned int delaycount;

void delay()
{
    if (delaycount)
	sleep(delaycount);
}

void drawcube()
{
    color(RED);
    bgnpolygon();
	v3f(v[0]);
	v3f(v[1]);
	v3f(v[2]);
	v3f(v[3]);
    endpolygon();
    delay();
    color(GREEN);
    bgnpolygon();
	v3f(v[0]);
	v3f(v[4]);
	v3f(v[5]);
	v3f(v[1]);
    endpolygon();
    delay();
    color(BLUE);
    bgnpolygon();
	v3f(v[4]);
	v3f(v[7]);
	v3f(v[6]);
	v3f(v[5]);
    endpolygon();
    delay();
    color(YELLOW);
    bgnpolygon();
	v3f(v[3]);
	v3f(v[7]);
	v3f(v[6]);
	v3f(v[2]);
    endpolygon();
    delay();
    color(MAGENTA);
    bgnpolygon();
	v3f(v[5]);
	v3f(v[1]);
	v3f(v[2]);
	v3f(v[6]);
    endpolygon();
    delay();
    color(CYAN);
    bgnpolygon();
	v3f(v[0]);
	v3f(v[4]);
	v3f(v[7]);
	v3f(v[3]);
    endpolygon();
}

main(argc, argv)
int argc;
char *argv[];
{
    Angle xrot, yrot, zrot;
    short val;

    xrot = yrot = zrot = 0;
    if (argc == 1)
	delaycount = 0;
    else
	delaycount = 1;

    if (getgdesc(GD_BITS_NORM_ZBUFFER) == 0) {
	fprintf(stderr, "Z-buffer not available on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("zbuffer");
    if (delaycount == 0)
	doublebuffer();
    gconfig();
    mmode(MVIEWING);
    ortho(-4.0, 4.0, -4.0, 4.0, -4.0, 4.0);
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);	    /* don't want window manager to act on clicks */ 

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	pushmatrix();
	    rotate(xrot, 'x');
	    rotate(yrot, 'y');
	    rotate(zrot, 'z');
	    color(BLACK);
	    clear();
	    if (getbutton(LEFTMOUSE)) {
		zbuffer(TRUE);
		zclear();
	    }
	    else
		zbuffer(FALSE);
	    pushmatrix();
		scale(1.2, 1.2, 1.2);
		translate(0.3, 0.2, 0.2);
		drawcube();
	    popmatrix();
	    pushmatrix();
		rotate(450 + zrot, 'x');
		rotate(300 - xrot, 'y');
		scale(1.8, 0.8, 0.8);
		drawcube();
	    popmatrix();
	    pushmatrix();
		rotate(500 + yrot, 'z');
		rotate(-zrot, 'x');
		translate(-0.3, -0.2, 0.6);
		scale(1.4, 1.2, 0.7);
		drawcube();
	    popmatrix();
	popmatrix();
	if (delaycount == 0)
	    swapbuffers();
	xrot += 11;
	yrot += 15;
	if (xrot + yrot > 3500)
	    zrot += 23;
	if (xrot > 3600)
	    xrot -= 3600;
	if (yrot > 3600)
	    yrot -= 3600;
	if (zrot > 3600)
	    zrot -= 3600;
    }
    gexit();
    return 0;
}
