#include <gl/gl.h>

#define NPOINTS	    5

#define RAMPBASE    64	    /* avoid the first 64 colors */
#define RAMPSIZE    128
#define RAMPSTEP    (255 / (RAMPSIZE-1))
#define RAMPCOLOR(fract) \
    ((Colorindex)((fract)*(RAMPSIZE-1) + 0.5) + RAMPBASE)

Coord parray[NPOINTS][2] = {
    { 50.0,  50.0},
    {250.0,  50.0},
    {350.0, 200.0},
    {250.0, 350.0},
    { 50.0, 350.0}
};

Colorindex iarray[NPOINTS] = {
    RAMPCOLOR(0.0),
    RAMPCOLOR(0.25),
    RAMPCOLOR(0.5),
    RAMPCOLOR(0.75),
    RAMPCOLOR(1.0)
};

main()
{
    int i;
    short red[RAMPSIZE], blue[RAMPSIZE], green[RAMPSIZE];

    prefsize(400, 400);
    winopen("splf");

    for (i = 0; i < RAMPSIZE; i++) /* save current color information */
        getmcolor( i + RAMPBASE, &red[i], &blue[i], &green[i]);

    /* create a red ramp */
    for (i = 0; i < RAMPSIZE; i++)
	mapcolor(i + RAMPBASE, i * RAMPSTEP, 0, 0);
    gflush();

    color(BLACK);
    clear();
    splf2(NPOINTS, parray, iarray);
    sleep(10);

    for (i = 0; i < RAMPSIZE; i++)
        mapcolor(i + RAMPBASE,red[i],blue[i],green[i]); /* restore colormap */
    gflush(); /* flush color map requests to the server */
    gexit();
    return 0;
}
