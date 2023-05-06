/* 
 *  iobounce.c:
 *
 *           "pool" ball that "bounces" around a 2-d "surface". 
 *                RIGHTMOUSE stops ball
 *                MIDDLEMOUSE increases y velocity
 *                LEFTMOUSE increases x velocity
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

long xmaxscrn, ymaxscrn;         /* maximum size of screen in x and y       */

#define XMIN 100
#define YMIN 100
#define XMAX 900
#define YMAX 700

long xvelocity = 0, yvelocity = 0;

main()
{
    Device dev;
    short val;
    long sizex, sizey;

    initialize();

    while (TRUE) {
       while (qtest()) {
          dev = qread(&val);
          switch (dev) {
                case REDRAW:     /* redraw window re: move/resize/push/pop */
                    reshapeviewport();
                    ortho2(XMIN - 0.5, XMAX + 0.5, YMIN - 0.5, YMAX + 0.5);
                    drawball();
                    break;
                case LEFTMOUSE:                /* increase xvelocity */
                    if (xvelocity >= 0)
                        xvelocity++;
                    else
                        xvelocity--;
                    break;
                case MIDDLEMOUSE:        /* increase yvelocity */
                    if (yvelocity >= 0)
                        yvelocity++;
                    else
                        yvelocity--;
                    break;
                case RIGHTMOUSE:         /* stop ball */
                    xvelocity = yvelocity = 0;
                    break;
                case ESCKEY:
                    gexit();
                    exit(0);
            }
        }
        drawball();
    }
}

initialize() {

    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    prefposition(xmaxscrn/4,xmaxscrn*3/4,ymaxscrn/4,ymaxscrn*3/4);
    winopen("iobounce");
    winconstraints();

    doublebuffer();
    gconfig();
    shademodel(FLAT);

    ortho2(XMIN - 0.5, XMAX + 0.5, YMIN - 0.5, YMAX + 0.5);

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);
}

drawball() {
    static xpos = 500,ypos = 500;
    long radius = 10;

    color(BLACK);
    clear();
    xpos += xvelocity;
    ypos += yvelocity;
    if (xpos > XMAX - radius ||
        xpos < XMIN + radius) {
        xpos -= xvelocity;
        xvelocity = -xvelocity;
    }
    if (ypos > YMAX - radius ||
        ypos < YMIN + radius) {
        ypos -= yvelocity;
        yvelocity = -yvelocity;
    }
    color(YELLOW);
    circfi(xpos, ypos, radius);
    swapbuffers();
}
