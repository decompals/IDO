/*
 *	clock - 
 *		A simple desk clock.
 *
 *				Paul Haeberli - 1984
 *
 *	Modified for IndigoVideo to show underlay mode.
 */

#include <stdio.h>
#include <time.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <math.h>
#include <svideo.h>

typedef struct XY {
    float x;
    float y;
} XY;

int hours, minutes, seconds;
int ahour, aminute, alarmflag;
XY sechand[4]; 
XY minhand[4]; 
XY hourhand[4]; 
XY alrmhand[4];
int menu;
long width, height;
long xoffset, yoffset;

Matrix ident = {1, 0, 0, 0,
		 0, 1, 0, 0,
		 0, 0, 1, 0,
		 0, 0, 0, 1
};

#define greyindex(x) (32 + (int)(25*x))
#define setgrey(x) setcolor(greyindex(x))
#define ANGCONV 1.90985931

void setalarmangle(long mousex,long mousey);
int shouldbeep();
void makeface();
void showhands(int fillcolor,int bordercolor,int shadow);
void gettime(int * h,int * m,int * s);
void setcolor(int c);
void drawpoly2(long verts,XY* v);
void drawpolf2(long verts,XY* v);
int showsechand = 1;
int gotvideo = 1;

/* Show video for pixels with this color */
#define VIDEO_UNDERLAY_COLOR	BLACK

XY
xy(double x, double y)
{
     XY a;
     a.x = x;
     a.y = y;
     return a;
}


main(int argc,char **argv)
{
    short val;
    int leftdown;
    int draw_needed;
    int mousex,mousey;
    long win;
    SVhandle V;
    long param[2];

    alarmflag=0;
    if (argc > 1 && !strcmp(argv[1], "-s"))   /* -s == don't show second hand */
	showsechand = 0;

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Determine maximum window size based on broadcast standard */
    param[0] = SV_BROADCAST;
    svGetParam(V, param, 2);
    if (param[1] == SV_PAL) {
	maxsize(SV_PAL_XMAX, SV_PAL_YMAX);
    } else {
	maxsize(SV_NTSC_XMAX, SV_NTSC_YMAX);
    }
    keepaspect(4,3);
    stepunit(8,6);	/* minimum step sizes to match video scaling */
    win = winopen("vclock");

    /* Associate video input with this window */
    if (svBindGLWindow(V, win, SV_IN_UNDER) < 0) {
        svPerror("bindwindow");
        exit(1);
    }

    subpixel(TRUE);
    mmode(MVIEWING);
    menu = defpup("vclock %t|calendar|alarm");
    setpup(menu, 2, PUP_BOX);
    noise(TIMER0, (showsechand ? 1 : 60 ) * getgdesc(GD_TIMERHZ));
    qdevice(TIMER0);
    qdevice(MENUBUTTON);
    qdevice(LEFTMOUSE);
    qdevice(ESCKEY);
    qdevice(WINFREEZE);
    qdevice(WINTHAW);
    qdevice(VIDEO);

    /* make the hands */
    hourhand[0] = minhand[0] = xy(-0.5,0.0);
    hourhand[1] = minhand[1] = xy(0.0,-1.5);
    hourhand[2] = minhand[2] = xy( 0.5,0.0);

    minhand[3] = xy(0.0,11.5);
    hourhand[3] = xy(0.0,7.0);

    sechand[0] = xy(-0.05,0.0);
    sechand[1] = xy(0.0,-2.0);
    sechand[2] = xy( 0.05,0.0);
    sechand[3] = xy(0.0,11.5);

    alrmhand[0] = xy(-0.2,0.0);
    alrmhand[1] = xy(0.0,-1.0);
    alrmhand[2] = xy( 0.2,0.0);
    alrmhand[3] = xy(0.0,9.0);

    scale(.75,1.0,1.0);		/* adjust for TV's 4:3 aspect ratio */

    /* draw the clock */
    qenter(REDRAW,1);
    while (1) {
	/* always empty queue before drawing, then draw only when required */
	while (qtest() || !draw_needed) {
	    switch (qread(&val)) {
		case REDRAW:
		    getsize(&width,&height);
		    if (svSetSize(V, width, height) < 0) {
			svPerror("set size");
			exit(1);
		    }
		    if (svBindGLWindow(V, win, SV_IN_UNDER) < 0) {
			svPerror("bind gl window");
			exit(1);
		    }
		    getorigin(&xoffset,&yoffset);
		    viewport(0,width-1,0,height-1);
		    draw_needed = TRUE;
		    break;
		case ESCKEY:
		    if (!val)
			exit(0);
		    break;
		case TIMER0:
		    draw_needed = TRUE;
		    break;
		case LEFTMOUSE:
		    leftdown = val;
		    if (alarmflag) {
			if (leftdown) {
			    qdevice(MOUSEX);
			    qdevice(MOUSEY);
			    mousex = getvaluator(MOUSEX);
			    mousey = getvaluator(MOUSEY);
			} else {
			    unqdevice(MOUSEX);
			    unqdevice(MOUSEY);
			}
			draw_needed = TRUE;
		    }
		    break;
		case MOUSEX:
		    mousex = val;
		    draw_needed = TRUE;
		    break;
		case MOUSEY:
		    mousey = val;
		    draw_needed = TRUE;
		    break;
		case MENUBUTTON:
		    if (val) {
			unqdevice(TIMER0);
			switch (dopup(menu)) {
			    case 1:
				system("ical");
				break;
			    case 2:
				alarmflag = !alarmflag;
				setpup(menu, 2,
				     alarmflag ? PUP_CHECK : PUP_BOX);
				if (alarmflag) {
				    ahour = 12;
				    aminute = 0;
				}
				break;
			}
			qdevice(TIMER0);
			draw_needed = TRUE;
		    }
		    break;
		case VIDEO:
		    if (val == SvActiveAttribute) {
			getsize(&width,&height);
			svSetSize(V, 0, height);
			if (svBindGLWindow(V, win, SV_IN_UNDER) < 0) {
			    svPerror("bindwindow");
			    exit(1);
			}
			gotvideo = 1;
			draw_needed = TRUE;
		    } else if (val == SvVideoPreempted) {
			gotvideo = 0;
			draw_needed = TRUE;
		    }
		    break;
	    }
	}
	if (leftdown && alarmflag)
	    setalarmangle(mousex,mousey);
	makeface();
	draw_needed = FALSE;
    }
}

void
setalarmangle(long mousex,long mousey)
{
    float ratio,angle;
    long mx,my;
    mx = mousex - width/2 - xoffset;
    my = mousey - height/2 - yoffset;
    if (mx==0 && my==0)
	return;
    if (abs(mx)>abs(my)) {
	ratio = (float)my / (float)mx;
	angle = (float)atan(ratio) * ANGCONV;
	if (mx>0) {
	    angle = 3.0 - angle;
	} else {
	    angle = 9.0 - angle;
	}
    } else {
	ratio = (float)mx/(float)my;
	angle = (float)atan(ratio) * ANGCONV;
	if (my>0) ;
	else angle = 6.0 + angle;
    }
    ahour = (int)angle;
    aminute = (int)(0.5 + 60*(angle - ahour));
    while (aminute<0) {
	ahour --;
	aminute += 60;
    }
    while (aminute>59) {
	ahour ++;
	aminute -= 60;
    }
    while (ahour<1) {
	ahour += 12;
    }
    while (ahour>12) {
	ahour -= 12;
    }
}

int
shouldbeep()
{
	long t1,t2;
	if (!alarmflag)
	    return 0;
	t1 = (hours%12)*60 + minutes;
	t2 = (ahour%12)*60 + aminute;
	return (t1-t2>=0 && t1-t2<5);
}

void setOrtho()
{
    ortho2(-100.0,100.0,-100.0,100.0);
}

showhour(Coord x1, Coord y1, Coord x2, Coord y2)
{
    setgrey(0.1);
    rectf(x1-0.2, y1-0.2, x2-0.2, y2-0.2);
    setgrey(0.8);
    rectf(x1, y1, x2, y2);
}

void
makeface()
{
    register int i;

    setOrtho();
    gettime(&hours,&minutes,&seconds);
    if (shouldbeep() && !seconds)
	ringbell();

    pushmatrix();
	if (shouldbeep()) 
	    setcolor(RED);
	else if (gotvideo)
	    setcolor(VIDEO_UNDERLAY_COLOR);
	else
	    setgrey(0.2);
	clear();
	scale(8.0,8.0,1.0);
	pushmatrix();
	    for (i=0; i<12; i++) {
		if (i==0)
		    showhour(-0.5,9.0,0.5,11.0);
		else if (i==3 || i==6 || i== 9)
		    showhour(-0.5,9.5,0.5,10.5);
		else
		    showhour(-0.25,9.5,0.25,10.5);
		rotate(-300,'z');
	    }
	    pushmatrix();
		translate(0.60,-0.60,0.0);
		showhands(greyindex(0.1),greyindex(0.1),TRUE);
	    popmatrix();
	    showhands(greyindex(0.4),greyindex(0.9),FALSE);
	popmatrix();
    popmatrix();
}

void
showhands(int fillcolor,int bordercolor,int shadow)
{
    /* draw the alarm hand and time */
    if (alarmflag) {
	static char timestring[20];
	/* draw the alarm hand */
	pushmatrix();
	    rotate(-(ahour*3600/12)-(aminute*5),'z');
	    if (shadow) 
		setgrey(0.1);
	    else 
		setcolor(RED);
	    drawpolf2(4,alrmhand);
	    drawpoly2(4,alrmhand);
	popmatrix();
	/* draw the alarm time */
	ortho2(0.,(float)width,0.,(float)height);
	pushmatrix();
	    loadmatrix(ident);
	    setcolor(YELLOW);
	    sprintf(timestring,"%2d:%2d",ahour,aminute);
	    if (aminute<10)
		timestring[3] = '0';
	    cmov2i(width/2 - strwidth(timestring)/2,getheight()/2);
	    charstr(timestring);
	popmatrix();
	setOrtho();
    }

    /* draw the hour hand */
    pushmatrix();
	rotate(-(hours*3600/12)-(minutes*5),'z');
	setcolor(fillcolor); 
	drawpolf2(4,hourhand);
	setcolor(bordercolor);
	drawpoly2(4,hourhand);
    popmatrix();

    /* draw the minute hand */
    pushmatrix();
	rotate(-(minutes*3600/60)-(seconds),'z');	
	setcolor(fillcolor); 
	drawpolf2(4,minhand);
	setcolor(bordercolor);
	drawpoly2(4,minhand);
    popmatrix();

    if (showsechand) {
	/* draw the second hand */
	pushmatrix();
	    rotate(-seconds*3600/60,'z');
	    setcolor(fillcolor); 
	    drawpolf2(4,sechand);
	    setcolor(bordercolor);
	    drawpoly2(4,sechand);
	popmatrix();
    }

    /* draw a point at the center */
    pnt(0.0,0.0,0.0);
}

void
gettime(int * h,int * m,int * s )
{
    long clock;
    struct tm *timeofday;

    time(&clock);
    timeofday = (struct tm *)localtime(&clock);
    *h = timeofday->tm_hour;
    *m = timeofday->tm_min;
    *s = timeofday->tm_sec;
}

void
setcolor(int c)
{
    color(c);
}

void
drawpoly2(long verts, XY* v)
{
    register i;
    bgnclosedline();
    for (i=0; i<verts; i++, v++) {
	v2f(&v->x);
    }
    endclosedline();
}

void
drawpolf2(long verts, XY* v)
{
    register i;
    bgnpolygon();
    for (i=0; i<verts; i++, v++) {
	v2f(&v->x);
    }
    endpolygon();
}
