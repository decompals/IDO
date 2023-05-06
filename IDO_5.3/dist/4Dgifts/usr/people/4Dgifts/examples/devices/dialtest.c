/*
 *  dialtest.c:
 *
 *  primitive program to test basic communication setup for the Dials box 
 *  (and by implication, the Buttons as well) functionality.  
 *  note the ranges used for setvaluator are completely arbitrary.
 *
 *  OPERATION OF:  this program was written to simply provide the most
 *                 *minimalist* test to determine whether or not the
 *                 dial and buttons box pair are correctly configured
 *                 to communicate with the IRIS.  it is for this reason
 *                 that the grafix window you are prompted to initially
 *                 open up does nothing more than provide a screen 
 *                 location for the input focus to be applied to--there
 *                 is no "graphics" (beside clearing to color GREEN) that 
 *                 happens in the GL window you open when this program 
 *                 runs.  the only output/feedback you will ever see is-
 *                 -with the mouse sitting *inside* the perimeter of the 
 *                 GL window--DIAL/X-Y window coordinates sent back to 
 *                 stdout into the shell you invoked dialtest from in the
 *                 first place.  This kind of "output" is sufficient to 
 *                 confirm that the dial and button box pair are 
 *                 correctly hooked up to the IRIS.  (if you wish to see 
 *                 graphics in the GL window (like an "etch-a-sketch"), 
 *                 look at/compile-run the dialbuttons program in this 
 *                 same directory.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

main()
{
    Icoord xval, yval;
    Device type, i;
    short val;

    foreground();
    winopen("dial test program");
    color(GREEN);
    clear();

    /* Put Mousebuttons, switches, and valuators in queue */    
    qdevice(KEYBD);                    
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    for(i=DIAL0; i<=DIAL7; i++) 
        qdevice(i);

    /* set noise so Dials must move more than two before value is queued */
    for(i=DIAL0; i<=DIAL7; i++) {
        noise(i, 2);
    }

    /* Set initial, min, and max values for dials */
    setvaluator(DIAL0,512,100,924);
    setvaluator(DIAL1,512,100,678);
    setvaluator(DIAL2,512,100,924);
    setvaluator(DIAL3,512,100,678);
    setvaluator(DIAL4,512,100,924);
    setvaluator(DIAL5,512,100,678);
    setvaluator(DIAL6,512,100,924);
    setvaluator(DIAL7,512,100,678);

    color(4);

    while (1) {
        type = qread(&val);
        switch (type) {
            case KEYBD:                            /* exits program       */
                gexit();
                exit(0);
                break;
            case REDRAW:            
		reshapeviewport();
	        color(GREEN);
    	        clear();
                break;         
            case INPUTCHANGE:  /* catch/trap inputchange events occurring */
                break;   /* when mouse crosses the window border boundary */
            case MIDDLEMOUSE:                      /* MIDDLEMOUSE is used */
                setvaluator(DIAL0,512,100,924);    /* to reset Dials 0-1  */
                setvaluator(DIAL1,512,100,678);    /* back to the center  */
                xval = getvaluator(DIAL0);         /* of their range      */
                yval = getvaluator(DIAL1);
                break;
            case DIAL0:
                xval = getvaluator(DIAL0);
                yval = getvaluator(DIAL1);
                fprintf(stderr,"DIAL0-1: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL1:
                xval = getvaluator(DIAL0);
                yval = getvaluator(DIAL1);
                fprintf(stderr," DIAL0-1: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL2:
                xval = getvaluator(DIAL2);
                yval = getvaluator(DIAL3);
                fprintf(stderr,"  DIAL2-3: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL3:
                xval = getvaluator(DIAL2);
                yval = getvaluator(DIAL3);
                fprintf(stderr,"   DIAL2-3: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL4:
                xval = getvaluator(DIAL4);
                yval = getvaluator(DIAL5);
                fprintf(stderr,"    DIAL4-5: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL5:
                xval = getvaluator(DIAL4);
                yval = getvaluator(DIAL5);
                fprintf(stderr,"     DIAL4-5: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL6:
                xval = getvaluator(DIAL6);
                yval = getvaluator(DIAL7);
                fprintf(stderr,"      DIAL6-7: x=%d  y=%d\n",xval, yval);
                break;
            case DIAL7:
                xval = getvaluator(DIAL6);
                yval = getvaluator(DIAL7);
                fprintf(stderr,"       DIAL6-7: x=%d  y=%d\n",xval, yval);
                break;
            default: fprintf(stderr,"device # %d was entered\n",type);
        }
    }
}
