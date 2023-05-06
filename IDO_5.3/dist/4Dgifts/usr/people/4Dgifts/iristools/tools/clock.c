/*
 *	clock - 
 *		A simple desk clock.
 *
 *				Paul Haeberli - 1984
 *
 *		Antialiased version for VGX and beyond
 *
 *				Kurt Akeley - 1991
 */

#include <stdio.h>
#include <time.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <math.h>

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
int dodot;
int doaa;
int iconic;
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
void drawcircle(float radius);

void drawcircle(float radius);

XY
xy(double x, double y)
{
     XY a;
     a.x = x;
     a.y = y;
     return a;
}


main(int argc,char **argv,char **envp)
{
    int i;
    short t, val;
    int leftdown;
    int draw_needed = TRUE;
    int mousex,mousey;

    /* check if resources are available */
    if (getgdesc(GD_POLYSMOOTH) && argc < 3)
	doaa = TRUE;
    else
	doaa = FALSE;

    alarmflag=0;
    if (argc>1)
	dodot = 1;

    /* get a window */ 
    iconsize(85, 66);
    keepaspect(1,1);
    winopen("clock");
    if (doaa) {
	RGBmode();
	doublebuffer();
	gconfig();
    }
    glcompat(GLC_OLDPOLYGON,FALSE);	/* just for the circle */
    subpixel(TRUE);
    mmode(MVIEWING);
    menu = defpup("clock %t|calendar|alarm");
    setpup(menu, 2, PUP_BOX);
    noise(TIMER0,getgdesc(GD_TIMERHZ));
    qdevice(TIMER0);
    qdevice(MENUBUTTON);
    qdevice(LEFTMOUSE);
    qdevice(ESCKEY);
    qdevice(REDRAWICONIC);
    qdevice(WINFREEZE);
    qdevice(WINTHAW);

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

    /* draw the clock */
    while (1) {
	/* always empty queue before drawing, then draw only when required */
	while (qtest() || !draw_needed) {
	    switch (t = qread(&val)) {
		case REDRAW:
		case REDRAWICONIC:
		    iconic = t == REDRAWICONIC;
		    getsize(&width,&height);
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
    float sc;

    if (iconic) {
	if (width > height) {
	    sc = 100.0 / height;
	    ortho2(-width * sc , width * sc, -100.0, 100.0);
	} else {
	    sc = 100.0 / width;
	    ortho2(-100.0, 100.0, -height * sc, height * sc);
	}
    } else {
	ortho2(-100.0,100.0,-100.0,100.0);
    }
}

void
makeface()
{
    register int i;

    setOrtho();
    gettime(&hours,&minutes,&seconds);
    if (shouldbeep() && !seconds)
	ringbell();

    if (doaa) {
	/* clear to a black background */
	cpack(0);
	clear();

	/* enable antialiasing of polygons */
	polysmooth(PYSM_ON);
	blendfunction(BF_MIN_SA_MDA,BF_ONE);

	pushmatrix();
	    scale(8.0,8.0,1.0);
	    pushmatrix();

		/* draw dot */
		if (dodot) {
		    setcolor(GREEN);
		    drawcircle(2.0);
		}

		/* draw the hands */
		showhands(greyindex(0.4),greyindex(0.9),FALSE);

		/* draw the shadows */
		pushmatrix();
		    translate(0.60,-0.60,0.0);
		    if (dodot) {
			setgrey(0.0);
			drawcircle(2.0);
		    }
		    showhands(greyindex(0.1),greyindex(0.1),TRUE);
		popmatrix();

		/* draw the markers */
		setgrey(0.8);
		for (i=0; i<12; i++) {
		    if (i==0)
			rectf(-0.5,9.0,0.5,11.0);
		    else if (i==3 || i==6 || i== 9)
			rectf(-0.5,9.5,0.5,10.5);
		    else
			rectf(-0.25,9.5,0.25,10.5);
		    rotate(-300,'z');
		}
	    popmatrix();
	popmatrix();

	/* clear the background */
	if (shouldbeep())
	    setcolor(RED);
	else
	    setgrey(0.2);
	rectf(-100.0,-100.0,100.0,100.0);

	polysmooth(PYSM_OFF);
	blendfunction(BF_ONE,BF_ZERO);

	swapbuffers();
    }
    else {
	pushmatrix();
	    if (shouldbeep()) 
		setcolor(RED);
	    else 
		setgrey(0.2);
	    clear();
	    scale(8.0,8.0,1.0);
	    pushmatrix();
		setgrey(0.8);
		for (i=0; i<12; i++) {
		    if (i==0)
			rectf(-0.5,9.0,0.5,11.0);
		    else if (i==3 || i==6 || i== 9)
			rectf(-0.5,9.5,0.5,10.5);
		    else
			rectf(-0.25,9.5,0.25,10.5);
		    rotate(-300,'z');
		}
		pushmatrix();
		    translate(0.60,-0.60,0.0);
		    showhands(greyindex(0.1),greyindex(0.1),TRUE);
		    if (dodot) {
			setgrey(0.0);
			drawcircle(2.0);
		    }
		popmatrix();
		showhands(greyindex(0.4),greyindex(0.9),FALSE);
		if (dodot) {
		    setcolor(GREEN);
		    drawcircle(2.0);
		}
	    popmatrix();
	popmatrix();
    }
}

void
showhands(int fillcolor,int bordercolor,int shadow)
{
    if (doaa) {
	/* draw the second hand */
	pushmatrix();
	    rotate(-seconds*3600/60,'z');
	    setcolor(bordercolor); 
	    drawpolf2(4,sechand);
	popmatrix();

	/* draw the minute hand */
	pushmatrix();
	    rotate(-(minutes*3600/60)-(seconds),'z');	
	    pushmatrix();
		scale(0.75,0.75,0.75);
		setcolor(fillcolor); 
		drawpolf2(4,minhand);
	    popmatrix();
	    setcolor(bordercolor);
	    drawpolf2(4,minhand);
	popmatrix();

	/* draw the hour hand */
	pushmatrix();
	    rotate(-(hours*3600/12)-(minutes*5),'z');
	    pushmatrix();
		scale(0.75,0.75,0.75);
		setcolor(fillcolor); 
		drawpolf2(4,hourhand);
	    popmatrix();
	    setcolor(bordercolor);
	    drawpolf2(4,hourhand);
	popmatrix();

	/* draw the alarm hand and time */
	if (alarmflag) {
	    static char timestring[20];
	    /* draw the alarm hand */
	    pushmatrix();
		rotate(-(ahour*3600/12)-(aminute*5),'z');
		if (shadow)
		    setgrey(0.1);
		else setcolor(RED);
		drawpolf2(4,alrmhand);
	    popmatrix();
	    if (!iconic) {
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
	}
    } else {
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
	    if (!iconic) {
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

	/* draw the second hand */
	pushmatrix();
	    rotate(-seconds*3600/60,'z');
	    setcolor(fillcolor); 
	    drawpolf2(4,sechand);
	    setcolor(bordercolor);
	    drawpoly2(4,sechand);
	popmatrix();

	/* draw a point at the center */
	pnt(0.0,0.0,0.0);
    }
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
    if (!doaa)
	color(c);
    else {
	switch (c) {
	case BLACK:
	    cpack(0xff000000);
	    break;
	case RED:
	    cpack(0xff0000ff);
	    break;
	case GREEN:
	    cpack(0xff00ff00);
	    break;
	case YELLOW:
	    cpack(0xff00ffff);
	    break;
	default:
	    if ((c >= 32) && (c <= 56)) {
		int val;
		val = (c-32)*10;
		cpack(val + (val<<8) + (val<<16) + 0xff000000);
	    } else {
		fprintf(stderr,"setcolor: unexpected value %d\n",c);
		exit(1);
	    }
	    break;
	}
    }
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

#define CIRCVERTS 80

void
drawcircle(float radius)
{
    static n = 0;
    static float *data;
    register i;
    register float *p;

    /* create data structure if not already computed */
    if (n != CIRCVERTS) {
	if (n)
	    free(data);
	data = (float*)malloc(sizeof(float)*2*(CIRCVERTS+1));
	n = CIRCVERTS;
	data[0] = data[1] = 0;
	for (i=0; i<CIRCVERTS; i++) {
	    data[2*(i+1)] = radius * fcos(i*2*M_PI / CIRCVERTS);
	    data[2*(i+1)+1] = radius * fsin(i*2*M_PI / CIRCVERTS);
	}
    }

    /* draw the circle as a single triangle mesh */
    bgntmesh();
    p = data;
    v2f(p);
    p += 2;
    for (i=0; i<CIRCVERTS; i++) {
	v2f(p);
	p += 2;
	swaptmesh();
    }
    v2f(data+2);
    endtmesh();
}
