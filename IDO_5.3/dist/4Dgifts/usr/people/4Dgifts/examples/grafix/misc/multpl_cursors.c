/*
 *  multpl_cursors.c:
 *
 * shows how to define and run a program that has more than one cursor defined.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

static Cursor fly = {0x0000, 0x1818, 0x2574, 0x2244, 
                     0x2184, 0xA185, 0xA185, 0xA185, 
                     0x518A, 0x2994, 0x1FF8, 0x15A8, 
                     0x2A54, 0x4BD2, 0x4422, 0x4002};

static unsigned short hourglass[16] = {0x7FFE, 0x43E2, 0x21C4, 0x308C,
                                       0x2814, 0x2424, 0x2244, 0x2244,
                                       0x23C4, 0x23C4, 0x27E4, 0x2FF4,
                                       0x33CC, 0x2004, 0x4002, 0x7FFE};

main() {

    short val;
    unsigned short cursor[128];
    long dev;
    long sx, sy;                 /* x/y size of current window   */
    long ox, oy;                 /* x/y origin of current window */
    long xmaxscrn, ymaxscrn;     /* maximum size of screen in x and y       */
    Colorindex dummy;            /* for strict ANSI C prototyping */


    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    winopen("multiple cursor test");
    getsize(&sx, &sy);
    getorigin(&ox, &oy);
    qdevice(ESCKEY);
    qdevice(ONEKEY);
    qdevice(TWOKEY);
    qdevice(THREEKEY);

    curstype (C16X1);
    defcursor(1, fly);
    curorigin(1,8,8);
    curstype (C16X1);
    defcursor(2, hourglass);
    curorigin(2,8,8);
    curstype (CCROSS);
    defcursor(3, cursor);
    curorigin(3,0,0);
    setcursor(1, dummy, dummy);

    color(CYAN);
    clear();
    drawmode(CURSORDRAW);
    mapcolor(1,1,1,1);
    drawmode(NORMALDRAW);

    while(1) {
        while(qtest()) {
            dev = qread(&val);
            switch(dev) {
                case ESCKEY:
                    setvaluator(MOUSEX, (short)(sx/2), 0, xmaxscrn);
                    setvaluator(MOUSEY, (short)(sy/2), 0, ymaxscrn);
                    gexit();
                    exit(1);
                    break;
                case REDRAW:
                    reshapeviewport();
                    getsize(&sx, &sy);
                    getorigin(&ox, &oy);
                    color(CYAN);
                    clear();
                    break;
                case ONEKEY:
                    setcursor(1, dummy, dummy);
                    setvaluator(MOUSEX, (short)(ox+(0.3125*sx)), 0, xmaxscrn);
                    setvaluator(MOUSEY, (short)(oy+(0.6225*sy)), 0, ymaxscrn);
                    break;
                case TWOKEY:
                    setcursor(2, dummy, dummy);
                    setvaluator(MOUSEX, (short)(ox+(0.0825*sx)), 0, xmaxscrn);
                    setvaluator(MOUSEY, (short)(oy+(0.0975*sy)), 0, ymaxscrn);
                    break;
                case THREEKEY:
                    setcursor(3, dummy, dummy);
                    setvaluator(MOUSEX, (short)(ox+(0.7825*sx)), 0, xmaxscrn);
                    setvaluator(MOUSEY, (short)(oy+(0.2275*sy)), 0, ymaxscrn);
                    break;
                default:
                    break;
            }
        }
    }
}
