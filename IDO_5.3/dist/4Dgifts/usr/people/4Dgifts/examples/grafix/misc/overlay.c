/*   
 * overlay.c:   another overlay bitplane usage demo program... 
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

unsigned short mycolortbl[83];
Boolean MACHMODE;

main () {
    Colorindex heat;
    int rampup();
    long sx, sy;
    short val = 0;
    int i,j, xpos = .9, ypos = .9;
    float xspd = 0.0, yspd = 0.0, 
          yaccel = -1.0, yacc = -.4, yreflect = -0.6;


    keepaspect (1,1);
    winopen ("overlay prog");
    getsize (&sx, &sy);
    viewport (0,sx-1,0,sy-1);
    ortho2 (0.0, 1.0, 0.0, 1.0);
    doublebuffer ();
    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2)   /* test for overlay planes */
        MACHMODE = PUPDRAW;
    else {
        MACHMODE = OVERDRAW;
        overlay(2);                               /* define/setup overlays */
    }
    gconfig();
    qdevice(ESCKEY);

    drawovers();

    mapcolor (0, 0, 0, 0);
    mapcolor (1, 255, 255, 0);
    rampup (12, 82, 255, 255, 0, 255, 0, 0);
    setbell (1);

    while (1) {

	if (qtest()) {

	    switch(qread(&val)) {

		case REDRAW:
		    reshapeviewport();
		    getsize (&sx, &sy);
		    viewport (0,sx-1,0,sy-1);
		    ortho2 (0.0, 1.0, 0.0, 1.0);
		    drawovers();
		    break;

		case ESCKEY:
		    drawmode (MACHMODE); /* clean up the overlay bitplanes */
		    color (0);
		    clear();
		    drawmode(NORMALDRAW);
		    gexit();
		    exit(0);
		    break;
	    }
	}

        for (i = j = 0.05; i < 990 && j < 990 && !(qtest()); 
	     i = i + 6, j = j + 6) {   
            color (BLUE);            /* roll the ball up and to the right */
            clear (); 
            if ((i==210) || (i==510) || (i==810)) 
                 ringbell();                   /* ring the bell everytime */
            if ((i>250) && (i<550))            /* the ball gets to the    */
                color (9);                     /* next rectangle, as well */
            else if ((i>550) && (i<850))       /* change the ball's color */
                color (5);
            else if (i>850)
                color (7);
            else
                color (3);
            circf (i/990.0, j/990.0, .035);
            swapbuffers ();
        }

        yspd = 0.0;
        for (heat=82, ypos=990; ypos>=5 && !(qtest()); ypos+=yspd) {
            color (BLUE);             /* drop the ball back to the bottom */
            clear ();                 
            color (mycolortbl[heat--]);        /* change the ball's color */
            yspd += yacc;                      /* as it falls             */
            circf (.95, ypos/990.0, .035);
            swapbuffers ();
        }
        yspd = -60.0;
        for (xpos=990, ypos=10; xpos >= 0 && !(qtest()); xpos -= 5) {
            if (ypos <= 10)          /* roll the ball back to the beginning */
                 yspd *= yreflect;             /* and keep updating its'   */
            color (BLUE);                      /* bounce-ability per frame */
            clear (); 
            color (1);
            ypos += yspd;
            yspd += yaccel;
            circf (xpos/990.0, ypos/790.0, .035);
            swapbuffers ();
        }
    }

}


drawovers() {

    drawmode (MACHMODE);                 /* get into the overlay bitplanes */
    color(0);
    clear();
    mapcolor (1, 255, 0, 0);           
    mapcolor (2, 0, 255, 0);
    mapcolor (3, 0, 255, 255);
    color (1);
    rectf (.215, .215, .285, .285);       /* and draw some rectangles for   */
    color (2);                            /* ball to roll underneath        */
    rectf (.515, .515, .585, .585);
    color (3);
    rectf (.815, .815, .885, .885);
    color (2);
    rectf (.877, .527, .947, .597);
    rectf (.733, .527, .805, .597);
    rectf (.949, .455,1.019, .525);
    rectf (.805, .455, .875, .525);
    rectf (.661, .455, .731, .525);
    rectf (.877, .383, .947, .453);
    rectf (.733, .383, .805, .453);   
    rectf (.589, .383, .659, .453);
    rectf (.949, .311,1.019, .381);
    rectf (.877, .239, .947, .309);
    rectf (.805, .311, .875, .381);
    rectf (.733, .239, .803, .309);
    rectf (.661, .311, .731, .381);
    rectf (.589, .239, .659, .309);
    rectf (.519, .311, .587, .381);
    rectf (.447, .239, .517, .309);
    rectf (.661, .167, .731, .237);
    rectf (.519, .167, .587, .237);
    rectf (.375, .167, .445, .237);
    rectf (.447, .095, .517, .165);
    rectf (.589, .095, .659, .165);
    rectf (.661, .023, .731, .093);
    rectf (.519, .023, .587, .093);
    rectf (.375, .023, .445, .093);
    drawmode (NORMALDRAW);
}


/*
 * make a color ramp                             
 * makes an interpolated ramp from the 1st arguement's index to the 2nd. 
 * 3rd and 4th are red's low and hi indices (5&6 green's, 7&8 are blue's) 
 */

rampup(first_lutv,last_lutv,minR,maxR,minG,maxG,minB,maxB)
unsigned short first_lutv, last_lutv,          /* start & end ramp values */
               minR, maxR, minG, maxG, minB, maxB;      /* lo/hi rgb vals */
{
    unsigned short len_red, len_green, len_blue,  /* length of each color */
                   i;                      /* counter for number of steps */

    float red, gre, blu;                                    /* lut values */
    float rdx, gdx, bdx,                       /* sizes of rgb increments */
          r, g, b,                              /* a position on the ramp */
          steps;                     /* # of steps along the ramp @ which */
                                    /* intensity assignments will be made */

    steps = (float) (last_lutv-first_lutv + 1); /*determine length of ramp*/

    len_red   = (maxR - minR);                 /* determine length of red */
    len_green = (maxG - minG);               /* determine length of green */
    len_blue  = (maxB - minB);                /* determine length of blue */

    rdx = (float) len_red   / steps;                      /* compuke step */
    gdx = (float) len_green / steps;                      /* sizes of r g */
    bdx = (float) len_blue  / steps;                      /* and b values */
    r = minR;                                          /* assign starting */
    g = minG;                                         /* indices for each */
    b = minB;                                         /* color value      */

    for (i = first_lutv; i <= last_lutv; i++) {
        red = r/255.0;                              /* round off */
        gre = g/255.0;                              /* given r g */
        blu = b/255.0;                              /* b value   */
                                          /* assign next color into mytbl */
        mycolortbl[i] = (unsigned short) rgb(red,gre,blu);
        /*
        printf("mycolortbl[%d] = %d\n", i, mycolortbl[i]);
        */
        r += rdx;                                            /* increment */
        g += gdx;                                            /* color in- */
        b += bdx;                                            /* dices     */
    }    
}
