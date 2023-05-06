#include <gl/gl.h>
#include <gl/device.h>

#define X	0
#define Y	1

main()
{
    short val, mval[2], lastval[2];
    long org[2], size[2];
    Device dev, mdev[2];
    Boolean run;
    int leftmouse_down = 0;
    lastval[X] = -1;

    prefsize(400, 400);
    winopen("input");
    color(BLACK);
    clear();
    getorigin(&org[X], &org[Y]);
    getsize(&size[X], &size[Y]);
    mdev[X] = MOUSEX;
    mdev[Y] = MOUSEY;
    getdev(2, mdev, lastval);	/* initialize lastval[] */
    lastval[X] -= org[X];
    lastval[Y] -= org[Y];
    qdevice(LEFTMOUSE);
    qdevice(ESCKEY);
    qdevice(MOUSEX);
    qdevice(MOUSEY);
    color(WHITE);		/* prepare to draw white lines */

    while (1) {
	switch (dev = qread(&val)) {
	    case LEFTMOUSE:
		leftmouse_down = val;
		break;

	    case MOUSEX:
		mval[X] = val - org[X];
		break;
	    case MOUSEY:
		mval[Y] = val - org[Y];
		if (leftmouse_down) {
		    bgnline();
			v2s(lastval);
			v2s(mval);
		    endline();
		}
		lastval[X] = mval[X];
		lastval[Y] = mval[Y];
		break;

	    case ESCKEY:
		exit(0);
	}
    }
}
