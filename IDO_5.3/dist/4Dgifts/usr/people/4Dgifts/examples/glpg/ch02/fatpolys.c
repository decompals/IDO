#include <gl/gl.h>

#define X	0
#define Y	1
#define XY	2

long v[4][XY] = {
    {0, 0},
    {1, 0},
    {1, 1},
    {0, 1},
};

main()
{
    prefsize(400, 400);
    winopen("fatpolys");
    color(BLACK);
    clear();

    /* high-performance routines draw point-sampled polygons */
    translate(50.0, 50.0, 0.0);
    color(CYAN);
    bgnpolygon();
	v2i(v[0]);
	v2i(v[1]);
	v2i(v[2]);
	v2i(v[3]);
    endpolygon();

    /* default mode for old-style polygon routines is "fat", i.e. outlined */
    translate(50.0, 0.0, 0.0);
    color(GREEN);
    polf2i(4, v);
    translate(0.0, 50.0, 0.0);
    rectfi(v[0][X], v[0][Y], v[2][X], v[2][Y]);
    translate(0.0, 50.0, 0.0);
    circfi(v[0][X], v[0][Y], 1);
    translate(0.0, 50.0, 0.0);
    arcfi(v[0][X], v[0][Y], 1, 0, 900);

    /* make old-style polygon routines draw point-sampled */
    glcompat(GLC_OLDPOLYGON, 0);
    translate(50.0, -150.0, 0.0);
    color(RED);
    polf2i(4, v);
    translate(0.0, 50.0, 0.0);
    rectfi(v[0][X], v[0][Y], v[2][X], v[2][Y]);
    translate(0.0, 50.0, 0.0);
    circfi(v[0][X], v[0][Y], 1);
    translate(0.0, 50.0, 0.0);
    arcfi(v[0][X], v[0][Y], 1, 0, 900);

    sleep(10);
    gexit();
    return 0;
}
