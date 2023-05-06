/*
 * Etch-a-sketch simulator used to test the dial box for GL2
 *
 * >>>>> Note that SWITCH 31 must be pressed before the main <<<<<
 * >>>>> body of the program will begin to be executed.      <<<<<
 *
 * Written by: Tom Davis
 * Updated by: Rick McLeod
 */

#include <gl/gl.h>
#include <gl/device.h>

main()
{
    Icoord xval, yval, pair, pairled, colorled, counter, dialnumber;
    Colorindex line_dr_color;
    Device type, i;
    short val;
    char buf[100];
    long win;

    foreground();
    prefposition(0,getgdesc(GD_XPMAX)-1,0,getgdesc(GD_YPMAX)-1);
    win = winopen("knobs_test");
    shademodel(FLAT);
    qdevice(KEYBD);
    cursoff();

    /* Set color map */
    mapcolor(0, 120, 120, 120);
    mapcolor(1, 0, 0, 0);
    mapcolor(2, 255, 0, 0);
    mapcolor(3, 0, 255, 0);
    mapcolor(4, 0, 0, 255);
    mapcolor(5, 255, 255, 0);
    mapcolor(6, 255, 0, 255);
    mapcolor(7, 0, 255, 255);

    linewidth(2);
    color(0);
    clear();
    move2i(0, 0);
    gflush();


    for(i=SW0; i<=SW31; i++) 
        qdevice(i);    
        
    for(i=DIAL0; i<=DIAL7; i++) 
        qdevice(i);

    /* Dials must move more than two before value is queued */
    for(i=DIAL0; i<=DIAL7; i++) {
        noise(i, 2);
    }

    /* Set max and min values for dials */
    setvaluator(DIAL0,512,100,getgdesc(GD_XPMAX)-100);
    setvaluator(DIAL1,512,100,getgdesc(GD_YPMAX)-100);
    setvaluator(DIAL2,512,100,getgdesc(GD_XPMAX)-100);
    setvaluator(DIAL3,512,100,getgdesc(GD_YPMAX)-100);
    setvaluator(DIAL4,512,100,getgdesc(GD_XPMAX)-100);
    setvaluator(DIAL5,512,100,getgdesc(GD_YPMAX)-100);
    setvaluator(DIAL6,512,100,getgdesc(GD_XPMAX)-100);
    setvaluator(DIAL7,512,100,getgdesc(GD_YPMAX)-100);

    xval = getvaluator(DIAL0);
    yval = getvaluator(DIAL1);
    move2i(xval,yval);

    dbtext("PRESS 31");
    /* Cycle through switch led's until switch 31 is pressed. */
    i=0;
    while (type != SW31) {
	    setdblights(1<<i);
	    i++;
	    sginap(10); /* Give device a moment to respond */
	    if(i>31) i=0;
            if (qtest()) {
                if (qread(&val) == SW31)
                    break;
                }
    }
    setdblights(0); /* clear led's */

    line_dr_color = 1;
    pair = 4;
    color(1);

    while (1) {
        type = qread(&val);
        dialnumber = 1;
        color(line_dr_color);
        if (type == KEYBD) {
	    setdblights(0);
	    greset();
            gexit();
            exit(0);
        }
        else

        /* Let switches 4, 10, 16, 22 chose dial pairs */    
        if (type == SW4) {
            pair = 4;
            pairled = 0x00000010;
	    setdblights(pairled);
        }
        else
        if (type == SW10) {
            pair = 3;
            pairled = 0x00000400;
	    setdblights(pairled);
        }
        else
        if (type == SW16) {
            pair = 2;
            pairled = 0x00010000;
	    setdblights(pairled);
        }
        else
        if (type == SW22) {
            pair = 1;
            pairled = 0x00400000;
	    setdblights(pairled);
        }

        /* Take x and y values from one of the pairs
           of valuators depending on value of 'pair' */
        
        if (type == DIAL0 && pair == 1)
            xval = val;
        else
        if (type == DIAL1 && pair == 1)
            yval = val;
        else
        if (type == DIAL2 && pair == 2)
            xval = val;
        else
        if (type == DIAL3 && pair == 2)
            yval = val;
        else
        if (type == DIAL4 && pair == 3)
            xval = val;
        else
        if (type == DIAL5 && pair == 3)
            yval = val;
        else
        if (type == DIAL6 && pair == 4)
            xval = val;
        else
        if (type == DIAL7 && pair == 4)
            yval = val;
        else
        /* Use switches 0 - 3 & 28 - 30 to select 
           line drawing color (line_dr_color)  */
        if (type == SW0) { 
            line_dr_color = 1;
            colorled = 0x00000001;
        }
        else
        if (type == SW1) { 
            line_dr_color = 2;
            colorled = 0x00000002;
        }
        else
        if (type == SW2) { 
            line_dr_color = 3;
            colorled = 0x00000004;
        }
        else
        if (type == SW3) { 
            line_dr_color = 4;
            colorled = 0x00000008;
        }
        else
        if (type == SW28) { 
            line_dr_color = 5;
            colorled = 0x10000000;
        }
        else
        if (type == SW29) { 
            line_dr_color = 6;
            colorled = 0x20000000;
        }
        else
        if (type == SW30) { 
            line_dr_color = 7;
            colorled = 0x40000000;
        }

        color(line_dr_color);
        draw2i(xval, yval);
        dbtext("LED TEST");

        /* The following switches perform these functions:
           switches 5 - 9 ----- draws circles of various radii
           switches 11 - 15 --- draws solid circles of various radii
           switches 17 - 21 --- draws rectangles of various sizes
           switches 23 - 27 --- draws solid rect's of various sizes */
        if (type == SW5) {
            color(2);
            circi(xval,yval, 100);
            dbtext("CIRCLE");
        }
        else
        if (type == SW6) {
            color(3);
            circi(xval,yval, 75);
            dbtext("CIRCLE");
        }
        else
        if (type == SW7) {
            color(4);
            circi(xval,yval, 50);
            dbtext("CIRCLE");
        }
        else
        if (type == SW8) {
            color(5);
            circi(xval,yval, 25);
            dbtext("CIRCLE");
        }
        else
        if (type == SW9) {
            color(6);
            circi(xval,yval, 10);
            dbtext("CIRCLE");
        }
        else
        if (type == SW11) {
            color(4);
            circfi(xval, yval, 100);
            dbtext("CIRCLE");
        }
        else
        if (type == SW12) {
            color(5);
            circfi(xval, yval, 75);
            dbtext("CIRCLE");
        }
        else
        if (type == SW13) {
            color(6);
            circfi(xval, yval, 50);
            dbtext("CIRCLE");
        }
        else
        if (type == SW14) {
            color(7);
            circfi(xval, yval, 25);
            dbtext("CIRCLE");
        }
        else
        if (type == SW15) {
            color(3);
            circfi(xval, yval, 10);
            dbtext("CIRCLE");
        }
        else
        if (type == SW17) {
            color(3);
            recti(xval+100,yval-100,xval-100,yval+100);
            dbtext("SQUARE");
        }
        else
        if (type == SW18) {
            color(4);
            recti(xval+75,yval-75,xval-75,yval+75);
            dbtext("SQUARE");
        }
        else
        if (type == SW19) {
            color(5);
            recti(xval+50,yval-50,xval-50,yval+50);
            dbtext("SQUARE");
        }
        else
        if (type == SW20) {
            color(6);
            recti(xval+25,yval-25,xval-25,yval+25);
            dbtext("SQUARE");
        }
        else
        if (type == SW21) {
            color(7);
            recti(xval+10,yval-10,xval-10,yval+10);
            dbtext("SQUARE");
        }
        else
        if (type == SW23) {
            color(5);
            rectfi(xval+100,yval-100,xval-100,yval+100);
            dbtext("SQUARE");
        }
        else
        if (type == SW24) {
            color(6);
            rectfi(xval+75,yval-75,xval-75,yval+75);
            dbtext("SQUARE");
        }
        else
        if (type == SW25) {
            color(7);
            rectfi(xval+50,yval-50,xval-50,yval+50);
            dbtext("SQUARE");
        }
        else
        if (type == SW26) {
            color(3);
            rectfi(xval+25,yval-25,xval-25,yval+25);
            dbtext("SQUARE");
        }
        else
        if (type == SW27) {
            color(4);
            rectfi(xval+10,yval-10,xval-10,yval+10);
            dbtext("SQUARE");
        }
        else

        /* Switch 31 clears the screen   */
        if (type == SW31) {
            dbtext(" CLEAR ");
            color(0);
            clear();
            color(1);
            move2i(xval,yval);
        }
        /* Print X, Y, and Color information in bottom left corner
           of screen.  */
        color(4);
        rectfi(0, 0, 200, 30);
        color(2);
        sprintf(buf, "X=%d Y=%d Color=%d", xval, yval, line_dr_color);
        cmov2i(0, 0);
        charstr(buf);
        move2i(xval, yval);
        gflush();
    }
}

