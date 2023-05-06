#include <gl/gl.h>

#define RAMPBASE    64	    /* avoid the first 64 colors */
#define RAMPSIZE    128
#define RAMPSTEP    (255 / (RAMPSIZE-1))
#define RAMPCOLOR(fract) \
    ((Colorindex)((fract)*(RAMPSIZE-1) + 0.5) + RAMPBASE)

float v[4][3] = {
    {50.0, 50.0, 0.0},    
    {200.0, 50.0, 0.0},
    {250.0, 250.0, 0.0},
    {50.0, 200.0, 0.0}
};

main()
{
    int i;
    short red[RAMPSIZE], blue[RAMPSIZE], green[RAMPSIZE];

    prefsize(400, 400);
    winopen("rampshade");
    color(BLACK);
    clear();

    for (i = 0; i < RAMPSIZE; i++) /* save current color information */
        getmcolor( i + RAMPBASE, &red[i], &blue[i], &green[i]);

    /* create a red ramp */
    for (i = 0; i < RAMPSIZE; i++)
	mapcolor(i + RAMPBASE, i * RAMPSTEP, 0, 0);
    gflush();

    bgnpolygon();
	color(RAMPCOLOR(0.0));
	v3f(v[0]);
	color(RAMPCOLOR(0.25));
	v3f(v[1]);
	color(RAMPCOLOR(1.0));
	v3f(v[2]);
	color(RAMPCOLOR(0.5));
	v3f(v[3]);
    endpolygon();
    sleep(10);

    for (i = 0; i < RAMPSIZE; i++)
        mapcolor(i + RAMPBASE,red[i],blue[i],green[i]); /* restore colormap */
    gflush(); /* flush color map requests to the server */
    gexit();
    return 0;
}