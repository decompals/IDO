/*
 *    cycol - 
 *        Display a pallete of colors.  A new palette may be selected 
 *        by pointing with the mouse and clicking the left mouse button.
 *
 *				David Ratcliffe - 1987
 */
#include "stdio.h"
#include "gl.h"
#include "device.h"
#include "port.h"

#define STANDSTILL  0
#define LEFT        1
#define RIGHT       2
#define MAXCOL   4096

short Start, End;                          /* indices of color range       */
short ro[MAXCOL], go[MAXCOL], bo[MAXCOL];  /* original color vals          */
short ri[MAXCOL], gi[MAXCOL], bi[MAXCOL];  /* initial "new" color vals     */
short rc[MAXCOL], gc[MAXCOL], bc[MAXCOL];  /* current color vals           */
long sx, sy;                         /* x and y lengths of sides of window */
long ox, oy;                         /* x and y screen origin of window    */
long curscrnx, curscrny;             /* current x/y screen position        */
long prescrnx, prescrny;             /* previous x/y screen position       */
int deltax;                          /* change of x delimits "speed" of LM */
int inwindow;                        /* flag for LEFTMOUSE decision making */
int justleftwindow;                  /* flag for LEFTMOUSE decision making */
int ncolors;                         /* number of colors in current range  */
int menu;			     /* the pop-up menu 		   */

main(argc,argv)
int argc;
char **argv;
{
    short val;
    static short pos1, pos2;
    int dorot, totalrot, currentrot;
    float downxpos, curxpos, fgetmousex();

    if (argc==3) {
        Start = (short) atoi(argv[1]);
        pos2 = End = (short) atoi(argv[2]);
    } else {
        Start = 128;
        pos2 = End = 255;
    }
    ncolors = End - Start + 1;
    justleftwindow = FALSE;
    winopen("cycol");
    glcompat(GLC_SOFTATTACH,TRUE);
    menu = defpup("cycol %t|restore range|restore map|clean exit");
    getorigin(&ox, &oy);
    getsize(&sx, &sy);
    qdevice(LEFTMOUSE);
    qdevice(MENUBUTTON);
    get_cmap();
    readmap();
    displaymap();
    while (1) {
        switch(qread(&val)) {
            case REDRAW:
		reshapeviewport();
                getorigin(&ox, &oy);
                getsize(&sx, &sy);
                displaymap();
                break;
            case MENUBUTTON:
		if(val) {
		    switch(dopup(menu)) {
			case 1:
                	    reset_colors();
			    break;
			case 2:
			    restore_cmap();
			    break;
			case 3:
			    restore_cmap();
			    gexit();
			    exit(0);
			    break;
		    }
		}
                break;
            case LEFTMOUSE:
                inwindow = findcurspos();
                if (inwindow) {
                    downxpos = fgetmousex();
                    currentrot = 0;
                    while (getbutton(LEFTMOUSE)) {
                        curxpos = fgetmousex();
                        totalrot = ((int)((ncolors*(curxpos-downxpos)) +10000.5)) -10000;
                        dorot = totalrot - currentrot;
                        if (dorot != 0)
			    shiftmap(dorot);
			currentrot = totalrot;
                    }
                    justleftwindow = TRUE;
		} else if ((!val) && (!justleftwindow)) {
                    pos1 = getapixel(getvaluator(MOUSEX), getvaluator(MOUSEY));
                    maprange(pos1, pos2);
                    pos2 = pos1;
                } else 
                    justleftwindow = FALSE; /* just left the cycol window so 
                                               make the next go round correct-
                                               ly check for new color values */
                displaymap();
                break;
            default:
                break;
        }
    }
}

maprange(pos1,pos2)
short pos1, pos2;
{
    Start = MIN(pos1, pos2);
    End   = MAX(pos1, pos2);
    ncolors = End - Start + 1;
    displaymap();
    readmap();
}

displaymap()
{
    register int i;

    ortho2((float)Start,(float)End+1,0.0,1.0);
    for (i=Start; i<=End; i++) {
        color(i);
        rectfi(i,0,i+1,1);
    }
}

findcurspos()
{
    long x, y;

    inwindow = FALSE;
    x = getvaluator(MOUSEX);
    y = getvaluator(MOUSEY);
    if ((ox < x) && (x < (ox + sx)) 
     && (oy < y) && (y < (oy + sy)))
        inwindow = TRUE;
    return(inwindow);
}


shiftmap(n)
int n;
{
    int i;

    if (n>0) {
        for (i=0; i<n; i++)
            shiftone(RIGHT);
    } 
    else if (n<0) {
        n = -n;
        for (i=0; i<n; i++)
            shiftone(LEFT);
    } 
    writemap();
}

shiftone(direction) 
int direction;
{
    short cind;
    short tmpr, tmpg, tmpb;                      /* temp rgb components */
          
    if (direction == RIGHT) {
        tmpr = rc[End];
        tmpg = gc[End];
        tmpb = bc[End];
        for (cind=End-1; cind>=Start; cind--) {
            rc[cind+1] = rc[cind];
            gc[cind+1] = gc[cind];
            bc[cind+1] = bc[cind];
        }
        rc[Start] = tmpr;
        gc[Start] = tmpg;
        bc[Start] = tmpb;
    } else if (direction == LEFT) {
        tmpr = rc[Start];
        tmpg = gc[Start];
        tmpb = bc[Start];
        for (cind=Start+1; cind<=End; cind++) {
            rc[cind-1] = rc[cind];
            gc[cind-1] = gc[cind];
            bc[cind-1] = bc[cind];
        }
        rc[End] = tmpr;
        gc[End] = tmpg;
        bc[End] = tmpb;
    }
}

writemap()                       /* update current range to current colors */
{
    short cind;

    for (cind=Start; cind<=End; cind++)
        mapcolor(cind,rc[cind],gc[cind],bc[cind]); 
}

readmap()
{
    short cind;

    for (cind=Start; cind<=End; cind++) {
        getmcolor(cind,&rc[cind],&gc[cind],&bc[cind]);/* fill current range */
	ri[cind] = rc[cind];
	gi[cind] = gc[cind];
	bi[cind] = bc[cind];
    }
}

reset_colors() 
{
    short cind;

    for (cind=Start; cind<=End; cind++) 
        mapcolor(cind, ri[cind], gi[cind], bi[cind]);
    displaymap();
}

get_cmap()      /* save original colormap--this will never get overwritten */
{
    short cind;
    int mapcolors;

    mapcolors = (1<<getplanes());
    for (cind=0; cind<mapcolors; cind++) 
        getmcolor(cind,&ro[cind],&go[cind],&bo[cind]); 
}

restore_cmap() 
{
    short cind;
    int mapcolors;

    mapcolors = (1<<getplanes());
    for (cind=0; cind<mapcolors; cind++) 
        mapcolor(cind, ro[cind], go[cind], bo[cind]);
}

float fgetmousex() 
{
    long x;
    float fx;

    x = getvaluator(MOUSEX);
    if (x < ox) {
        fx = 0.0;
    }
    else if (x > (ox+sx)) {
        fx = 1.0;
    } else {
        fx = (float) (x-ox) / (float) sx;
        if (fx>1.0)
            fx = 1.0;
        else if (fx < 0.0)
            fx = 0.0;
    }
    return (fx);
}
