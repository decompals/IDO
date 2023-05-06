/*
 *  setshade.c:
 *
 *     moves a smooth-shaded polygon in and out of the graphics port.
 *     press LEFTMOUSE to exit.
 *     as of 3.2, the setshade(3G) man page describes this routine as 
 *     obsolete and that replacement by color(3G) produces identical 
 *     results.
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <stdio.h>

main() {

    Device dev;
    short val;
    unsigned short i;
    long xorig, yorig, xsize, ysize;
    Coord x;
    int   do_sngl_buf = 0;

/* Test for ability to use 256 colors in double buffer.
** If the machine has fewer than 256 colors available 
** in double buffered colormap mode, then leave it in
** single buffered mode. To offset some of the side affects
** of single buffered animation, we use gsync() to help
** even out the frame updates. It's not perfect. Try commenting
** out the gsync lines for a contrast.
*/
    if(getgdesc(GD_BITS_NORM_DBL_CMODE) < 8) do_sngl_buf = 1;
    if(do_sngl_buf) {
    fprintf(stdout, "\nWarning: this machine does not have enough bits\n");
    fprintf(stdout, "to have a range of 256 colors AND do double\n");
    fprintf(stdout, "buffer animation. Expect some flickering.\n");
    fflush(stdout);
    }
    foreground();
    winopen("setshade/clip test");
    if(!do_sngl_buf) doublebuffer();
    gconfig();
    getorigin(&xorig, &yorig);
    getsize(&xsize, &ysize);
    viewport(0,xsize-1,0,ysize-1);
    ortho2(-0.5, xsize-0.5, -0.5, ysize-0.5);
    qdevice(ESCKEY);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    get_cmap();

    for (i=0; i<128; i++)
        mapcolor(128+i, 2*i, 0, 2*i);

    makeobj(1);
        color(128+127);
        pmv2i(100,-100);
        color(128+0);
        pdr2i(100,100);
        color(128+127);
        pdr2i(-100,100);
        color(128+0);
        pdr2i(-100,-100);
        spclos();
    closeobj();

    while (TRUE) {
        for (x = -150.0; x < 150.0; x++) {
            color(CYAN);
            clear();
            pushmatrix();
            translate(x,150.0,0.0);
            rotate(300,'z');
            callobj(1);
            popmatrix();
	    if(do_sngl_buf) gsync();
            else swapbuffers();
	    if (qtest()) {
		dev = qread(&val);
		switch (dev) {

                                        /* Exit when key is going up, not */
                                        /* down.  This avoids the scenario */
                    case ESCKEY:        /* where a window underneath this */
                        if (val) break; /* program's window--say a wsh--would */
                    case WINQUIT:       /* otherwise "eat up" the up-event 
                                         * of the Esc key being released. 
                                 * Also, regarding the usage of WINQUIT,
                                 * which will be put into the queue upon
                                 * "boinking" on the lighting bolt or by
                                 * choosing the "Quit" NeWS menu item:
                                 * notice how it DOES NOT test to see if
                                 * the WINQUIT val is true because val
                                 * contains the graphics ID of the window
                                 * being "quitted" and thus will always
                                 * be non-zero.
                                 */
			restore_cmap();
			gexit();
			exit(0);
			break;
		    case REDRAW:
			reshapeviewport();
			getorigin(&xorig, &yorig);
			getsize(&xsize, &ysize);
			viewport(0,xsize-1,0,ysize-1);
			ortho2(-0.5, xsize-0.5, -0.5, ysize-0.5);
			break;
		}
	    }
        } 
        for (x=150.0; x>-150.0; x--) {
            color(CYAN);
            clear();
            pushmatrix();
            translate(x,150.0,0.0);
            rotate(300,'z');
            callobj(1);
            popmatrix();
	    if(do_sngl_buf) gsync();
            else swapbuffers();
	    if (qtest()) {
		dev = qread(&val);
		switch (dev) {
		    case ESCKEY:
			restore_cmap();
			gexit();
			exit(0);
			break;
		    case REDRAW:
			reshapeviewport();
			getorigin(&xorig, &yorig);
			getsize(&xsize, &ysize);
			viewport(0,xsize-1,0,ysize-1);
			ortho2(-0.5, xsize-0.5, -0.5, ysize-0.5);
			break;
		}
	    }
        }
    }
}

#define lo_end 128
#define hi_end 255
static short CarrayR[hi_end+1], CarrayG[hi_end+1], CarrayB[hi_end+1];
unsigned short cmindex;

get_cmap() 
{
    short rcomp, gcomp, bcomp;

    for (cmindex=lo_end; cmindex<=hi_end; cmindex++) {
        getmcolor(cmindex,&rcomp, &gcomp, &bcomp);
        CarrayR[cmindex] = rcomp;
        CarrayG[cmindex] = gcomp;
        CarrayB[cmindex] = bcomp;
    }
}

restore_cmap() 
{
    for (cmindex=lo_end; cmindex<=hi_end; cmindex++) 
        mapcolor(cmindex,CarrayR[cmindex], CarrayG[cmindex], CarrayB[cmindex]);
}
