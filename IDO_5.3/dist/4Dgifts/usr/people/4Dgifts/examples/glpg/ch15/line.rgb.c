/*
 * Rotate a pinwheel of antialiased lines drawn in RGB mode.
 * Change to the "smoother" sampling function when the left mouse button
 *   is depressed.
 * Change to the "end-corrected" sampling function when the middle mouse
 *   button is depressed.
 * Change to a "color index like" blend function when the i-key is depressed.
 * Change from merge-blend to accumulate-blend when the a-key is depressed.
 * Disable subpixel positioning when the s-key is depressed.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE    400
#define MAXLINE    48
#define ROTANGLE   (360.0 / MAXLINE)

float vert0[2] = {0.0,0.0};
float vert1[2] = {0.8,0.0};

main()
{
    int i;
    int smoothmode;
    short val;

    if (getgdesc(GD_LINESMOOTH_RGB) == 0) {
	fprintf(stderr, "RGB mode line antialiasing not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("linesmooth.rgb");
    mmode(MVIEWING);
    ortho2(-1.0,1.0,-1.0,1.0);
    doublebuffer();
    RGBmode();
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	cpack(0);
	clear();
	cpack(0xffffffff);
	smoothmode = SML_ON;
	if (getbutton(LEFTMOUSE))
	    smoothmode |= SML_SMOOTHER;
	if (getbutton(MIDDLEMOUSE))
	    smoothmode |= SML_END_CORRECT;
	linesmooth(smoothmode);
	if (getbutton(IKEY))
	    blendfunction(BF_SA,BF_ZERO);
	else if (getbutton(AKEY))
	    blendfunction(BF_SA,BF_ONE);
	else
	    blendfunction(BF_SA,BF_MSA);
	subpixel(getbutton(SKEY) ? FALSE : TRUE);
	pushmatrix();
	rot(getvaluator(MOUSEX) / 25.0,'z');
	for (i=0; i<MAXLINE; i++) {
	    bgnline();
	    v2f(vert0);
	    v2f(vert1);
	    endline();
	    rot(ROTANGLE,'z');
	}
	popmatrix();
	swapbuffers();
    }
    gexit();
    return 0;
}
