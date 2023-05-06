/*
 *   mapw:
 *
 *    Bare-bones example demonstrates use of MAPW(3G) for determining a line
 *  in 3-space given a 2-D, screen space point.  Note that the "line" drawn
 *  after the call to MAPW(3G) simply looks like a green point.  This is
 *  exactly as it should be--i.e. drawing a line from the first to the
 *  second point returned by mapw *should* appear to the viewer's eye as if
 *  it is simply a point in space.
 *
 *                                      ratman - 1987
 */

#include <gl/gl.h>
#include <gl/device.h>

#define VOBJ 6

main()
{
    Coord wx1, wy1, wz1, wx2, wy2, wz2;
    long xorig, yorig;
    Screencoord scrx, scry;
    Device dev;
    short val;


    prefsize(512,512);
    winopen("mapw example");
    qdevice(LEFTMOUSE);
    qdevice(ESCKEY);
    color(BLACK);
    clear();
    perspective(440,1.0,1.0,1500.0);

    makeobj(VOBJ);
        perspective(440,1.0,1.0,1500.0);
    closeobj();

    getorigin(&xorig, &yorig);

    while (TRUE) {
        dev = qread(&val);
        switch(dev) {
            case LEFTMOUSE:
                if (val) {
                    scrx = (short) getvaluator(MOUSEX) - xorig;   
                    scry = (short) getvaluator(MOUSEY) - yorig;  
                    callobj(VOBJ);
                    mapw(VOBJ,scrx,scry,&wx1,&wy1,&wz1,&wx2,&wy2,&wz2);

                    /* only get values +/- 1.0 for wz1 */
                    printf("wx1=%.2f  wy1=%.2f  wz1=%.2f  ", wx1, wy1, wz1);
                    printf("wx2=%.2f  wy2=%.2f  wz2=%.2f\n", wx2, wy2, wz2);
		    color(BLACK);
		    clear();
		    color(GREEN);
		    move(wx1, wy1, wz1);
		    draw(wx2, wy2, wz2);
                }
                break;
            case REDRAW:
                getorigin(&xorig, &yorig);
		reshapeviewport();
                callobj(VOBJ);
		color(BLACK);
		clear();
                break;
            case ESCKEY:
                gexit();
                exit(0);
                break;

        }
    }
}
