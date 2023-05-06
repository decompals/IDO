/*
 * dialbuttons.c:
 *
 * this program is expanded version of dialtest.c.  
 *  - it draws colored lines in the x and y directions based on which dial 
 *    gets turned:  
 *      x is DIAL0, DIAL2, DIAL4, & DIAL6, and
 *      y is DIAL1, DIAL3, DIAL5, & DIAL7.
 *
 *  - the switches (0-31) control the noise level for the eight dials.  
 *    the initial noise delta is set to 2. 
 *
 *  - everytime a switch is depressed or a dial is turned, the LED display on
 *    the switches box (if you have that style--some switch box models do not
 *    include an LED display) will specify which device is being accessed.
 * 
 *  - there is a variable called count--initially set to 5--used to determine
 *    the frequency of echoing to stderr the screen values being returned from
 *    getvaluator and their equivalent "window values".  (no matter what size/
 *    shape of window one initially opens or manually resizes, this window is
 *    mapped to a simulated "full console screen" in that x always runs from 
 *    0 to 1279, and y always runs from 0 to 1023.  (0 to 1023 and 0 to 767 
 *    respectively for the 14-inch monitor 4D machines.))  if for some reason 
 *    you wish to change the frequency, push the numeric keys 1-9 to increase
 *    or decrease the freqency of text being sent to stderr.
 *
 *              dave "who can do? ratmandu!" ratcliffe - 1990
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define round(n)       ((int) ((n) + 0.5))

long sx, sy;                   /* size of window in x and y */
float xratio, yratio;          /* maps window size to full console */
short xmaxscrn, ymaxscrn;      /* maximum simulated size of screen in x & y */

main() {

    static int curdial = 0, count = 0, modfactor = 5;
    Icoord xval, yval;
    float xpos, ypos, startx, starty;
    Device dev, i;
    short val;
    char str[9];
    long xscrnsize;       /* size of actual screen in x used to set globals  */

    xscrnsize = getgdesc(GD_XPMAX);                   /* get/set screen size */
    if (xscrnsize == 1280) {
        xmaxscrn = 1279;
        ymaxscrn = 1023;
        xval = 640;
        yval = 512;
    } else if (xscrnsize ==1024) {
        xmaxscrn = 1023;
        ymaxscrn = 767;
        xval = 512;
        yval = 384;
    } else {
        fprintf(stderr, "Something's EXTREMELY wrong:  ");
        fprintf(stderr, "xscrnsize=%d\n", xscrnsize);
        exit(-1) ;
    }


    foreground();
    winopen("");
    wintitle("dialbuttons program");
    linewidth(3);
    init_win();
    startx = ((float) sx)/2;
    starty = ((float) sy)/2;
    move2(startx, starty);
    draw2(startx+1, starty-1);
    draw2(startx-1, starty+1);

    /* Put Mousebuttons, switches, valuators, and buttons in the event ueue */
    qdevice(ESCKEY);                    
    qdevice(ONEKEY);                    
    qdevice(TWOKEY);                    
    qdevice(THREEKEY);                    
    qdevice(FOURKEY);                    
    qdevice(FIVEKEY);                    
    qdevice(SIXKEY);                    
    qdevice(SEVENKEY);                    
    qdevice(EIGHTKEY);                    
    qdevice(NINEKEY);                    
    for(i=DIAL0; i<=DIAL7; i++) {
        qdevice(i);
    }
    for(i=SW0; i<=SW31; i++) {
        qdevice(i);
    }

    /* set noise so Dials must move more than two before value is queued */
    for(i=DIAL0; i<=DIAL7; i++) {
        noise(i, 2);
    }

    /* Set initial, min, and max values for dials */
    setvaluator(DIAL0, (short) xval, 0, xmaxscrn);
    setvaluator(DIAL1, (short) yval, 0, ymaxscrn);
    setvaluator(DIAL2, (short) xval, 0, xmaxscrn);
    setvaluator(DIAL3, (short) yval, 0, ymaxscrn);
    setvaluator(DIAL4, (short) xval, 0, xmaxscrn);
    setvaluator(DIAL5, (short) yval, 0, ymaxscrn);
    setvaluator(DIAL6, (short) xval, 0, xmaxscrn);
    setvaluator(DIAL7, (short) yval, 0, ymaxscrn);

    color(4);

    qreset();
    while (1) {
        dev = qread(&val);
	if (SW0 <= dev && dev <= SW31) {
	    sprintf(str,"BUTTON%2d",dev-SW0);
            dbtext(str);
            if (val) {
                for(i=DIAL0; i<=DIAL7; i++) noise(dev-SW0, 0);
                fprintf(stderr, "\n noise delta now set to %d\n",dev-SW0);
	    }
	} else if (ONEKEY <= dev && dev <= NINEKEY) {
	    modfactor = dev - ONEKEY + 1;
            count = 0;
	} else switch (dev) {
            case ESCKEY:                            /* exits program       */
                gexit();
                exit(0);
                break;
            case INPUTCHANGE:
                qreset();
                break;
            case REDRAW:
                init_win();
                break;
            case DIAL0:
                if (curdial == 0) {
                    color(0);
                    xval = getvaluator(DIAL0);
                    yval = getvaluator(DIAL1);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0) {
                        fprintf(stderr,"DIAL0-1: xvalue=%d  yvalue=%d ",
                                                        xval,       yval);
                        fprintf(stderr,"     xwinpos=%d  ywinpos=%d\n",
                                                  round(xpos), round(ypos));
                    }
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL0, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL1, (short) yval, 0, ymaxscrn);
                    curdial = 0;
                    dbtext("DIAL 0");
                }
                break;
            case DIAL1:
                if (curdial == 1) {
                    color(1);
                    xval = getvaluator(DIAL0);
                    yval = getvaluator(DIAL1);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0) {
                        fprintf(stderr," DIAL1-0: yvalue=%d  xvalue=%d ",
                                                        yval,       xval);
                        fprintf(stderr,"     ywinpos=%d  xwinpos=%d\n",
                                                  round(ypos), round(xpos));
                    }
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL0, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL1, (short) yval, 0, ymaxscrn);
                    curdial = 1;
                    dbtext("DIAL 1");
                }
                break;
            case DIAL2:
                if (curdial == 2) {
                    color(2);
                    xval = getvaluator(DIAL2);
                    yval = getvaluator(DIAL3);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0) 
                        fprintf(stderr,"  DIAL2:  xvalue=%d  xwinpos=%d\n",
                                                      xval,    round(xpos));
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL2, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL3, (short) yval, 0, ymaxscrn);
                    curdial = 2;
                    dbtext("DIAL 2");
                }
                break;
            case DIAL3:
                if (curdial == 3) {
                    color(3);
                    xval = getvaluator(DIAL2);
                    yval = getvaluator(DIAL3);
                    ypos = yval*yratio;
                    xpos = xval*xratio;
                    if ((count++ % modfactor) == 0)
                        fprintf(stderr,"   DIAL3:    yvalue=%d    ywinpos=%d\n",
                                                    yval,           round(ypos));
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL2, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL3, (short) yval, 0, ymaxscrn);
                    curdial = 3;
                    dbtext("DIAL 3");
                }
                break;
            case DIAL4:
                if (curdial == 4) {
                    color(4);
                    xval = getvaluator(DIAL4);
                    yval = getvaluator(DIAL5);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0) 
                        fprintf(stderr,"    DIAL4:  xvalue=%d  xwinpos=%d\n",
                                                    xval,        round(xpos));
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL4, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL5, (short) yval, 0, ymaxscrn);
                    curdial = 4;
                    dbtext("DIAL 4");
                }
                break;
            case DIAL5:
                if (curdial == 5) {
                    color(5);
                    xval = getvaluator(DIAL4);
                    yval = getvaluator(DIAL5);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0) 
                        fprintf(stderr,"     DIAL5:    yvalue=%d    ywinpos=%d\n",
                                                    yval,             round(ypos));
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL4, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL5, (short) yval, 0, ymaxscrn);
                    curdial = 5;
                    dbtext("DIAL 5");
                }
                break;
            case DIAL6:
                if (curdial == 6) {
                    color(6);
                    xval = getvaluator(DIAL6);
                    yval = getvaluator(DIAL7);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0) 
                        fprintf(stderr,"      DIAL6:  xvalue=%d  xwinpos=%d\n",
                                                    xval,          round(xpos));
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL6, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL7, (short) yval, 0, ymaxscrn);
                    curdial = 6;
                    dbtext("DIAL 6");
                }
                break;
            case DIAL7:
                if (curdial == 7) {
                    color(7);
                    xval = getvaluator(DIAL6);
                    yval = getvaluator(DIAL7);
                    xpos = xval*xratio;
                    ypos = yval*yratio;
                    if ((count++ % modfactor) == 0)
                        fprintf(stderr,"       DIAL7:    yvalue=%d    ywinpos=%d\n",
                                                    yval,               round(ypos));
                    draw2(xpos,ypos);
                } else {
                    setvaluator(DIAL6, (short) xval, 0, xmaxscrn);
                    setvaluator(DIAL7, (short) yval, 0, ymaxscrn);
                    curdial = 7;
                    dbtext("DIAL 7");
                }
                break;
        }
    }
}
 
 
init_win() {

    reshapeviewport();
    getsize(&sx,&sy);
    viewport(0,sx-1,0,sy-1);
    ortho2(-0.5,sx-0.5,-0.5,sy-0.5);
    color(44);
    clear();
    xratio = ((float) sx-1)/xmaxscrn;
    yratio = ((float) sy-1)/ymaxscrn;
}
