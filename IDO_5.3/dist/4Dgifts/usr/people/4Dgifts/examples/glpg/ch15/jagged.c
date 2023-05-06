/*
 * Drag a color map aliased line with the cursor.
 */

#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE     400

Device devs[2] = {MOUSEX,MOUSEY};
float orig[2] = {100.,100.};

main()
{
    short val, vals[2];
    long xorg, yorg;
    float vert[2];

    prefsize(WINSIZE, WINSIZE);
    winopen("jagged");
    mmode(MVIEWING);
    ortho2(-0.5, WINSIZE-0.5, -0.5, WINSIZE-0.5);
    doublebuffer();
    gconfig();
    qdevice(ESCKEY);
    getorigin(&xorg, &yorg);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	color(BLACK);
	clear();
	getdev(2,devs,vals);
	vert[0] = vals[0] - xorg;
	vert[1] = vals[1] - yorg;
	color(WHITE);
	bgnline();
	v2f(orig);
	v2f(vert);
	endline();
	swapbuffers();
    }
    gexit();
    return 0;
}
