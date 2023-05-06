/*    
 * simple program to test the bitpad's functionalility.
 * press RIGHTMOUSE or ESCKEY to exit.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define CLEAR      1
#define STDERRON   2
#define STDERROFF  3
#define EXIT       4

main() {

    int winid, menu, menuval;               /* vars for window/popup menu */
    int stderron = TRUE;                    /* standard error output flag */
    long x, y, xprev, yprev;            /* current/previous puck position */
    Device dev;
    short val;


    foreground();
    prefposition(0, getgdesc(GD_XPMAX)-1, 0, getgdesc(GD_YPMAX)-1);
    winid = winopen("test");          
    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
    menu = defpup(" bitpad example %t|clear|stderr ON|stderr OFF|exit");

    color(4);                                 /* clear background to blue */
    clear(); 
    attachcursor(BPADX, BPADY);             /* hook cursor to bitpad puck */

    xprev = x = getvaluator(BPADX);      /* get initial x/y puck position */
    yprev = y = getvaluator(BPADY); 
    move2i(x,y);

    while(1) {
        while(qtest()) {

            /* for MOUSEX/Y or EXIT action,  we must re-attach the cursor 
               and then re-setvaluators to their standard default ranges */

            attachcursor(MOUSEX, MOUSEY);            
            setvaluator(MOUSEX, getgdesc(GD_XPMAX)/2, 0, getgdesc(GD_XPMAX));
            setvaluator(MOUSEY, getgdesc(GD_YPMAX)/2, 0, getgdesc(GD_YPMAX));

            dev = qread(&val);
            switch(dev) {
                case ESCKEY:
                    gexit();
                    exit(0); /* this MUST BE called, else prog still runs */
                    break;
                case RIGHTMOUSE:
                    if (val) {
                        menuval = dopup(menu);
                        switch(menuval) {
                            case CLEAR:
                                color(BLUE);
                                clear();
                                break;
                            case STDERRON:
                                stderron = TRUE;
                                break;
                            case STDERROFF:
                                stderron = FALSE;
                                break;
                            case EXIT:
                                gexit();
                                exit(0);
                                break;
                            default:
                                break;
                        }
                    }
                default:
                break;
            }
            attachcursor(BPADX, BPADY);    /* remember to reset to bitpad */
        }

        x = getvaluator(CURSORX);                 /* get next puck position */
        y = getvaluator(CURSORY);
        if (stderron)
            if ((x != xprev) || (y != yprev))    /* has position changed? */
                fprintf(stderr, "x=%d  y=%d\n",x,y);
        color(7);
        draw2i(x, y);
        xprev = x;                              /* save previous position */
        yprev = y;
    }
}
