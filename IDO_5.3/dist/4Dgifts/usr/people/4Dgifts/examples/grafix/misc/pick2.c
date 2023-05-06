/*
 *    pick2.c:
 *
 *      Another sample pick program showing a different set of objects
 *    that can be picked.  Notice that with the text string "CYAN", in 
 *    order to be able to register a hit, it is necessary to be have the
 *    cursor be close to its origin (bottom left corner of the 'C' 
 *    character).
 *      press any key on the KEYBD to exit program.
 */

#include <gl/gl.h>
#include <gl/device.h>

#define BUFSIZ  300

main () {
    Colorindex wmask;
    Device     dev;
    short      pickbuff[BUFSIZ], val;
    int        i, j, numpicked, k, winid;
    int        ysize = 700, xsize = 540;

    prefsize(xsize,ysize);
    winid = winopen("picktest");

    color (BLACK);
    clear ();
    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(KEYBD);

       /*	make all objects		*/
    makeobj (110);		/* 110 object			*/
    pushname (110);		/* push 110 onto stack		*/
    color (CYAN);
    recti (164, 33, 364, 600);  /* large hollow rectangle	*/
    popname ();			/* remove 110 from stack	*/
    closeobj ();

    makeobj (120);		/* 120 object -- various things	*/
    pushname (119);		/* push 119 			*/
    pushname (120);             /* push 120 (119 and 120)       */
    color (YELLOW);
    cmov2i (400, 400);
    charstr ("Yellow");		/* draw charstr CYAN		*/
    popname ();			/* remove 120 (119)		*/
    pushname (121);		/* push 121 (119 and 121)	*/
    color (WHITE);		
    rectfi (400, 100, 500, 300);/* draw filled rect-lower right*/
    popname ();			/* remove 121 (119)		*/
    pushname (122);		/* push 122 (119 and 122)	*/
    color (CYAN);
    cmov2i (420, 200);
    charstr ("Cyan");		/* draw charstr CYAN		*/
    popname ();			/* remove 122 (119)		*/
    pushname (123);		/* push 123 (119 and 123)	*/
    color (WHITE);
    rectfi (100, 100, 200, 200);/* draw filled rect-lower left	*/
    popname ();			/* remove 123 (119)		*/
    popname ();			/* remove 119			*/
    closeobj ();

    makeobj (100);		/* 100 -- THE MAIN OBJECT	*/
    initnames ();		/* clear name stack		*/
    loadname (100);		/* push 100 onto stack		*/
    callobj (110);
    color (WHITE);
    callobj (120);
    closeobj ();

    ortho2 (-0.5, (float)(xsize+0.5), -0.5, (float)(ysize+0.5));
    color (BLACK);
    clear ();
    callobj (100);

    swapbuffers ();

    while (1) {
	if (qtest()) {
	    dev = qread(&val);
	    switch(dev) {
		case KEYBD:
		    gexit();
		    exit(0);
		    break;
		case REDRAW:
		    reshapeviewport();
                    ortho2 (-0.5, (float)(xsize+0.5), -0.5, (float)(ysize+0.5));
		    color(BLACK);
		    clear();
		    callobj(100);
		    break;
                case LEFTMOUSE:
		    if (val) {
			pushmatrix ();
			pick (pickbuff, BUFSIZ);
			initnames ();
			ortho2 (-0.5, (float) (xsize+0.5),
				-0.5, (float) (ysize+0.5));
			callobj (100);	/* reset proj. matrix & draw objects */
			numpicked = endpick (pickbuff);  /* get # of entries */
			popmatrix ();	             /* restore the universe */

			printf ("numpicked = %d |", numpicked);
			j = 0;
			for (i = 0; i < numpicked; i++) {  /* for each entry */
			    printf (" ");
			    k = pickbuff[j++];      /* get number of objects */
			    printf ("%d ", k);
			    for (; k; k--)	          /* for each object */
				printf ("%d ", pickbuff[j++]);   /* print it */
			    printf ("|");
			}
			printf ("\n");

			callobj (100);
			break;
		    }
	    }
	}
    }
}
