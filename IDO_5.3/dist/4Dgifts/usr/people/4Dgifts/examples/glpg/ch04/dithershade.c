#include <stdio.h>
#include <gl/gl.h>

float v[4][3] = {
    {50.0, 50.0, 0.0},    
    {200.0, 50.0, 0.0},
    {250.0, 250.0, 0.0},
    {50.0, 200.0, 0.0}
};

main()
{
    if (getgdesc(GD_CIFRACT) == 0) {
	fprintf(stderr, "Fractional color index shading not available "
			"on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("dithershade");
    color(BLACK);
    clear();
    bgnpolygon();
	colorf(BLACK + 0.0);
	v3f(v[0]);
	colorf(BLACK + 0.25);
	v3f(v[1]);
	colorf(BLACK + 1.0);	    /* = RED */
	v3f(v[2]);
	colorf(BLACK + 0.5);
	v3f(v[3]);
    endpolygon();
    sleep(10);
    gexit();
    return 0;
}
