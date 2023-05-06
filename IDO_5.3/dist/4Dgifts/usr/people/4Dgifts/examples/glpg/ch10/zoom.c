#include <gl/gl.h>
#include <gl/device.h>

#define X	0
#define Y	1
#define XY	2

#define	ZOOM	3

main()
{
    long org[XY], size[XY], readsize[XY];
    Device dev;
    short val;
    Device mdev[XY];
    short mval[XY];
    Boolean run;

    prefsize(400, 400);
    winopen("zoom");
    qdevice(ESCKEY);
    qdevice(TIMER0);
    noise(TIMER0, getgdesc(GD_TIMERHZ)/10);	/* sample 10 times per second */
    getorigin(&org[X], &org[Y]);
    getsize(&size[X], &size[Y]);
    readsize[X] = size[X] / ZOOM;
    readsize[Y] = size[Y] / ZOOM;
    rectzoom((float)ZOOM, (float)ZOOM);
    mdev[X] = MOUSEX;
    mdev[Y] = MOUSEY;

    run = TRUE;
    while (run) {
	switch (qread(&val)) {
	case TIMER0:
	    getdev(XY, mdev, mval);
	    mval[X] -= org[X];
	    mval[Y] -= org[Y];
	    rectcopy(mval[X], mval[Y], 
		     mval[X] + readsize[X], mval[Y] + readsize[Y], 
		     0, 0);
	    break;
	case ESCKEY:
	    if (val == 0)
		run = FALSE;
	    break;
	}
    }
    gexit();
    return 0;
}
