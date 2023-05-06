/*
 *	weave -
 *		A weaving simulator for IRIS workstations.  This simulates
 *	a loom and the pattern created with different settings of the headles
 *	and various treadling patterns.  The tieups are also programable.
 *	Click the left mouse over the left area to change the treadling
 *	pattern, Click the left mouse over the top area to change the 
 *	threading of the headles.  Click the left mouse over the box in
 *	the upper left corner to alter the tieups.
 *
 *			Paul Haeberli and Amie Slate - 1991
 */
#include "stdio.h"
#include "gl.h"
#include "device.h"

/* feel free to change the number of treadles and headles */

#define NTREADLES	4
#define NHEADLES	4

#define BOXSIZE		8

#define TOTX		(800/BOXSIZE)
#define TOTY		(600/BOXSIZE)
#define NX		(TOTX-NTREADLES)
#define NY		(TOTY-NHEADLES)

int up[NX];
int headles[NX];
int treadles[NY];
int tieup[NHEADLES][NTREADLES];

main()
{
    short val;
    int x, y, menu;

    prefsize(TOTX*BOXSIZE,TOTY*BOXSIZE);
    winopen("weave");
    qmouse();
    subpixel(1);
    pseudorgb();
    initstate();
    makeframe();
    menu = defpup("weave %t|init state|print help");
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		makeframe();
		break;
	    case LEFTMOUSE:
		if(val) {
		    x = getmousex()/BOXSIZE;
		    y = getmousey()/BOXSIZE;
		    if(x<NTREADLES) {
			if(y>=NY) {
			    y = y-NY;
			    tieup[y][x] = 1-tieup[y][x];
			    makeframe();
			} else {
			    treadles[y] = x;
			    updaterow(y);
			}
		    } else if(y>=NY) {
			x = x-NTREADLES;
			headles[x] = y-NY;
			updatecol(x);
		    }
		}
		break;
	    case MENUBUTTON:
		if(val) {
		    switch(dopup(menu)) {
			case 1:
			    initstate();
			    makeframe();
			    break;
			case 2:
			    printhelp();
			    break;
		    }
		}
	}
    }
}

initstate()
{
    int i, j;

    for(i=0; i<NX; i++)
	headles[i] = i%NHEADLES;
    for(i=0; i<NY; i++)
	treadles[i] = i%NTREADLES;
    for(i=0; i<NHEADLES; i++) {
	for(j=0; j<NTREADLES; j++) {
	    if(i == j || i == ((j+1)%NTREADLES))
		tieup[i][j] = 1;
	    else
		tieup[i][j] = 0;
	}
    }
}

#define DEL (1*1.0/BOXSIZE)

makeframe()
{
    int x, y, t, ty;

    reshapeviewport();
    ortho2(0.0,(float)TOTX,0.0,(float)TOTY);
    panelcolor();
    clear();

/* draw the contents of the headles */
    pushmatrix();
    translate((float)NTREADLES,(float)NY,0.0);
    markcolor();
    for(x=0; x<NX; x++) {
	fillrecti(x,headles[x],x+1,headles[x]+1);
    }
    popmatrix();

/* draw the contents of the treadles */
    markcolor();
    for(y=0; y<NY; y++) {
	fillrecti(treadles[y],y,treadles[y]+1,y+1);
    }

/* draw the contents of the tieups */
    pushmatrix();
    translate(0.0,(float)NY,0.0);
    markcolor();
    for(y=0; y<NHEADLES; y++) {
	for(x=0; x<NTREADLES; x++) {
	    if(tieup[y][x])
		fillrecti(x,y,x+1,y+1);
	}
    }
    popmatrix();

/* draw the pattern */
    pushmatrix();
    translate(((float) NTREADLES),0.0,0.0);
    backcolor();
    fillrecti(0,0,NX,NY);
    darkthreadcolor();
    for(x=0; x<NX; x++)
	fillrect(x+DEL,0.0,x+1-DEL,(float)NY);
    lightthreadcolor();
    for(y=0; y<NY; y++)
	fillrect(0.0,y+DEL,(float)NX,y+1-DEL);
    for(y=0; y<NY; y++) {
	for(x=0; x<NX; x++) 
	    up[x] = 0;
	t = treadles[y];
	for(ty=0; ty<NHEADLES; ty++) {
	    if(tieup[ty][t]) {
		for(x=0; x<NX; x++) {
		    if(headles[x] == ty)
			up[x] = 1;
		}
	    }
	}
	darkthreadcolor();
	for(x=0; x<NX; x++) {
	    if(up[x]) 
		fillrect(x+DEL,y+DEL,x+1-DEL,y+1-DEL);
	}
    }
    popmatrix();
    drawgrid();
}

drawgrid()
{
    int x, y;

/* draw grid */
    gridcolor();
    for(x=0; x<TOTX; x++) {
	if(x<NTREADLES) {
	    move2i(x,0);
	    draw2i(x,TOTY);
	} else {
	    move2i(x,NY);
	    draw2i(x,TOTY);
	}
    }
    for(y=0; y<TOTY; y++) {
	if(y<NY) {
	    move2i(0,y);
	    draw2i(NTREADLES,y);
	} else {
	    move2i(0,y);
	    draw2i(TOTX,y);
	}
    }

/* draw dark lines */
    blackcolor();
    move2i(NTREADLES,0);
    draw2i(NTREADLES,TOTY);
    move2i(0,NY);
    draw2i(TOTX,NY);
}

updaterow(y)
int y;
{
    int x, t, ty;

    frontbuffer(1);
    pushmatrix();
    translate(((float) NTREADLES),0.0,0.0);
    lightthreadcolor();
    fillrect(0.0,y+DEL,(float)NX,y+1-DEL);
    for(x=0; x<NX; x++) 
	up[x] = 0;
    t = treadles[y];
    for(ty=0; ty<NHEADLES; ty++) {
	if(tieup[ty][t]) {
	    for(x=0; x<NX; x++) {
		if(headles[x] == ty)
		    up[x] = 1;
	    }
	}
    }
    darkthreadcolor();
    for(x=0; x<NX; x++) {
	if(up[x]) 
	    fillrect(x+DEL,y+DEL,x+1-DEL,y+1-DEL);
    }
    popmatrix();
    panelcolor();
    fillrecti(0,y,NTREADLES,y+1);
    markcolor();
    fillrecti(treadles[y],y,treadles[y]+1,y+1);
    drawgrid();
    frontbuffer(0);
}

updatecol(x)
int x;
{
    int y, t, ty;

    frontbuffer(1);
    pushmatrix();
    translate(((float) NTREADLES),0.0,0.0);
    darkthreadcolor();
    fillrect(x+DEL,0.0,x+1-DEL,(float)NY);
    for(y=0; y<NY; y++) {
	t = treadles[y];
	up[0] = 0;
	for(ty=0; ty<NHEADLES; ty++) {
	    if(tieup[ty][t]) {
		if(headles[x] == ty)
		    up[0] = 1;
	    }
	}
	lightthreadcolor();
	if(!up[0]) 
	    fillrect(x+DEL,y+DEL,x+1-DEL,y+1-DEL);
    }
    panelcolor();
    fillrecti(x,NY,x+1,NY+NHEADLES);
    markcolor();
    fillrecti(x,NY+headles[x],x+1,NY+headles[x]+1);
    popmatrix();
    drawgrid();
    frontbuffer(0);
}


panelcolor()
{
    rgb(0.8,0.8,0.8);
}

backcolor()
{
    rgb(0.25,0.25,0.25);
}

lightthreadcolor()
{
    rgb(0.9,0.9,0.9);
}

darkthreadcolor()
{
    rgb(0.0,0.0,0.0);
}

blackcolor()
{
    rgb(0.0,0.0,0.0);
}

markcolor()
{
    rgb(0.5,0.0,0.0);
}

gridcolor()
{
    rgb(0.4,0.4,0.4);
}

printhelp()
{
    printf("\n\nPaul Haeberli and Amie Slate's weaving simulator:\n\n");
    printf("The state of the headles is shown across the top.  This can be\n");
    printf("changed by clicking with left mouse button to select which\n");
    printf("headle the threads go through.\n\n");
    printf("The treadling pattern can be changed by clicking the left\n");
    printf("mouse over the area on the left hand part of the window\n\n");
    printf("The state of the tieup can be changed by clicking the left\n");
    printf("mouse on the area in the upper left corner\n\n");
}
