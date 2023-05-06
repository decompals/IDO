/*
 *  pick1.c: 
 *
 *    sample picking demo code.  use LEFTMOUSE to "pick" the background, 
 *  a circle, or the square.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define PICKS 1

long xmaxscrn, ymaxscrn;         /* maximum size of screen in x and y       */

main()
{
    short namebuffer[50];
    long numpicked;
    short val, i, j, k;
    Device dev;

    initialize();

    makeobj(PICKS);
	translate(500.0, 250.0, 0.0);
        color(RED);
        loadname(1);
        rectfi(20,20,100,100);
        loadname(2);
        pushname(3);
        circi(50,500,50);
        loadname(4);
        circi(50,530,60);
        loadname(5);
        move2i(30,30);
        draw2i(32,32);
    closeobj();

    while (TRUE) {
        dev = qread(&val);
        if (val == 0)
            continue;
        switch (dev) {
            case ESCKEY:
                gexit();
                exit(0);
            case REDRAW:
		reshapeviewport();
                ortho2(-0.5, xmaxscrn + 0.5, -0.5, ymaxscrn + 0.5);
                color(BLACK);
                clear();
                callobj(PICKS);
                break;
            case LEFTMOUSE:
                pick(namebuffer, 50);
                ortho2(-0.5, xmaxscrn + 0.5, -0.5, ymaxscrn + 0.5);
                initnames();
                callobj(PICKS);
                numpicked = endpick(namebuffer);
                printf("hits: %d; ",numpicked);
                j = 0;
                for (i = 0; i < numpicked; i++) {
                    printf(" ");
                    k = namebuffer[j++];
                    printf("%d ", k);
                    for (;k; k--)
                        printf("%d ", namebuffer[j++]);
                    printf("|");
                }
                printf("\n");
                break;
            default:
                break;
        }
    }
}

initialize()
{
    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    prefposition(xmaxscrn/4,xmaxscrn*3/4,ymaxscrn/4,ymaxscrn*3/4);
    winopen("pick1");

    ortho2(-0.5, xmaxscrn + 0.5, -0.5, ymaxscrn + 0.5);

    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qenter(REDRAW, 1);
}
