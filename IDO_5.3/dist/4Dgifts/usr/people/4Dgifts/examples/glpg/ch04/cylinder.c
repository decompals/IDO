#include <stdio.h>
#include <math.h>
#include <gl/gl.h>

#define X	0
#define Y	1
#define Z	2
#define XYZ	3
#define R	0
#define G	1
#define B	2
#define RGB	3

#define RADIUS .9
#define MAX(x,y) (((x) > (y)) ? (x) : (y))

float blackvec[RGB] = {0.0, 0.0, 0.0};
float lightpos[XYZ] = {1.8, 0.0, 1.2};
float dot(), dist2();

main()
{
    int i, nperside;
    float p[4][XYZ];
    float n[4][XYZ];
    float c[4][RGB];
    float x, dx;
    float theta, dtheta;
    float comp;

    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	fprintf(stderr, "Single buffered RGB not available on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("cylinder");
    RGBmode();
    gconfig();
    ortho(-2.5, 2.5, -2.5, 2.5, -3.0, 3.0);
    c3f(blackvec);
    clear();

    for (nperside = 1; nperside < 10; nperside++) {
	dx = 3.0 / nperside;
	dtheta = M_PI / nperside;
	for (x = -1.5; x < 1.5; x = x + dx) {
	    for (theta = 0.0; theta < M_PI; theta += dtheta) {
		p[0][X] = p[1][X] = x;
		p[0][Y] = p[3][Y] = RADIUS * fcos(theta);
		p[0][Z] = p[3][Z] = RADIUS * fsin(theta);
		p[2][X] = p[3][X] = x + dx;
		p[1][Y] = p[2][Y] = RADIUS * fcos(theta + dtheta);
		p[1][Z] = p[2][Z] = RADIUS * fsin(theta + dtheta);

		for (i = 0; i < 4; i++) {
		    n[i][X] = 0.0;
		    n[i][Y] = p[i][Y] / RADIUS;
		    n[i][Z] = p[i][Z] / RADIUS;
		}

		for (i = 0; i < 4; i++) {
		    comp = dot(lightpos, n[i])/(0.5 + dist2(lightpos, p[i]));
		    c[i][R] = c[i][G] = c[i][B] = MAX(comp, 0.0);
		}

		bgnpolygon();
		for (i = 0; i < 4; i++) {
		    c3f(c[i]);
		    v3f(p[i]);
		}
		endpolygon();
	    }
	}
	sleep(2);
    }
    sleep(5);
    gexit();
    return 0;
}

/* dot: find the dot product of two vectors */
float dot(v1, v2)
float v1[XYZ], v2[XYZ];
{
    return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2];
}


/* dist2: find the square of the distance between two points */
float dist2(v1, v2)
float v1[XYZ], v2[XYZ];
{
    return (v1[0] - v2[0])*(v1[0] - v2[0]) +
	   (v1[1] - v2[1])*(v1[1] - v2[1]) +
	   (v1[2] - v2[2])*(v1[2] - v2[2]);
}
