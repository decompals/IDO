#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

#define RGB_BLACK   0x000000  
#define RGB_WHITE   0xffffff  

#define X	0
#define Y	1
#define XY	2

#define SIZE	0.05
#define BOUNDS	(1.0 + SIZE)
#define EDGE	(1.1 * BOUNDS)

float boundary[4][XY] = {
    {-BOUNDS, -BOUNDS},
    {-BOUNDS,  BOUNDS},
    { BOUNDS,  BOUNDS},
    { BOUNDS, -BOUNDS}
};

struct ball_s {
    float pos[XY];
    float delta[XY];
    unsigned long col;
};

main(argc, argv)
int argc;
char *argv[];
{
    int i, j;
    int nballs;
    struct ball_s *balls;
    short val;

    if (getgdesc(GD_BITS_NORM_DBL_RED) == 0) {
	fprintf(stderr, "Double buffered RGB not available on this machine\n");
	return 1;
    }

    if (argc != 2) {
	fprintf(stderr, "Usage: bounce <ball count>\n");
	return 1;
    }

    nballs = atoi(argv[1]);
    if (!(balls = (struct ball_s *)malloc(nballs * sizeof(struct ball_s)))) {
	fprintf(stderr, "bounce: malloc failed\n");
	return 1;
    }
    for (i = 0; i < nballs; i++) {
	for (j = 0; j < XY; j++) {
	    balls[i].pos[j] = 2.0 * (drand48() - 0.5);
	    balls[i].delta[j] = 2.0 * (drand48() - 0.5) / 50.0;
	}
	balls[i].col = drand48() * 0xffffff;
    }

    prefsize(400, 400);
    winopen("bounce");
    doublebuffer();
    RGBmode();
    gconfig();
    shademodel(FLAT);
    qdevice(ESCKEY);
    ortho2(-EDGE, EDGE, -EDGE, EDGE);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	for (i = 0; i < nballs; i++) {
	    for (j = 0; j < XY; j++) {
		balls[i].pos[j] += balls[i].delta[j];
		if ((balls[i].pos[j] >= 1.0) || (balls[i].pos[j] <= -1.0))
		    balls[i].delta[j] = -balls[i].delta[j];
	    }
	}
	cpack(RGB_BLACK);
	clear();
	cpack(RGB_WHITE);
	bgnclosedline();
	for (i = 0; i < 4; i++)
	    v2f(boundary[i]);
	endclosedline();
	for (i = 0; i < nballs; i++) {
	    cpack(balls[i].col);
	    sboxf(balls[i].pos[X]-SIZE, balls[i].pos[Y]-SIZE,
		  balls[i].pos[X]+SIZE, balls[i].pos[Y]+SIZE);
	}
	swapbuffers();
    }
    gexit();
    return 0;
}
