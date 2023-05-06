/*
 *	colorbars - 
 *		A colorbar generator based on "Illumination and Color in
 *	Compute Generated Imagery" by Roy Hall.
 *
 *				Paul Haeberli - 1989
 */
#include "stdio.h"
#include "math.h"
#include "device.h"
#include "gl.h"

#define GAMMA		(2.2)

#define NBARS		3

#define BARS_EIA	0
#define BARS_SMPTE	1
#define BARS_FULL	2

#define SDELTA		1.0
#define BDELTA		1.25
#define EIAYBASE	0.25
#define SMPTEYBASE	0.33
#define SMPTEYLOW	0.25

ColorBars(type)
int	type;
{
    switch(type){
	case BARS_EIA:
	    EIAbars();
	    wintitle("EIA colorbars");
	    break;
	case BARS_SMPTE:
	    SMPTEbars();
	    wintitle("SMPTE colorbars");
	    break;
	case BARS_FULL:
	    FULLbars();
	    wintitle("Full colorbars");
	    break;
    }
}

EIAbars()
{
    gamRGBcolor(191,191,191);
    rectf(0*SDELTA,EIAYBASE,1*SDELTA,1.0);
    gamRGBcolor(191,191,0);
    rectf(1*SDELTA,EIAYBASE,2*SDELTA,1.0);
    gamRGBcolor(0,191,191);
    rectf(2*SDELTA,EIAYBASE,3*SDELTA,1.0);
    gamRGBcolor(0,191,0);
    rectf(3*SDELTA,EIAYBASE,4*SDELTA,1.0);
    gamRGBcolor(191,0,191);
    rectf(4*SDELTA,EIAYBASE,5*SDELTA,1.0);
    gamRGBcolor(191,0,0);
    rectf(5*SDELTA,EIAYBASE,6*SDELTA,1.0);
    gamRGBcolor(0,0,191);
    rectf(6*SDELTA,EIAYBASE,7*SDELTA,1.0);

    gamRGBcolor(0,76,127);
    rectf(0*BDELTA,0.0,1*BDELTA,EIAYBASE);	/* -I */
    gamRGBcolor(255,255,255);
    rectf(1*BDELTA,0.0,2*BDELTA,EIAYBASE);	/* W */
    gamRGBcolor(75,0,139);
    rectf(2*BDELTA,0.0,3*BDELTA,EIAYBASE);	/* +Q */

    gamRGBcolor(0,0,0);
    rectf(3*BDELTA,0.0,7*SDELTA,EIAYBASE);

    gamRGBcolor(0,0,0);
    rectf(7*SDELTA,0.0,8*SDELTA,1.0);
}

SMPTEbars()
{
    gamRGBcolor(191,191,191);
    rectf(0*SDELTA,SMPTEYBASE,1*SDELTA,1.0);
    gamRGBcolor(191,191,0);
    rectf(1*SDELTA,SMPTEYBASE,2*SDELTA,1.0);
    gamRGBcolor(0,191,191);
    rectf(2*SDELTA,SMPTEYBASE,3*SDELTA,1.0);
    gamRGBcolor(0,191,0);
    rectf(3*SDELTA,SMPTEYBASE,4*SDELTA,1.0);
    gamRGBcolor(191,0,191);
    rectf(4*SDELTA,SMPTEYBASE,5*SDELTA,1.0);
    gamRGBcolor(191,0,0);
    rectf(5*SDELTA,SMPTEYBASE,6*SDELTA,1.0);
    gamRGBcolor(0,0,191);
    rectf(6*SDELTA,SMPTEYBASE,7*SDELTA,1.0);

    gamRGBcolor(0,0,191);
    rectf(0*SDELTA,SMPTEYLOW,1*SDELTA,SMPTEYBASE);
    gamRGBcolor(0,0,0);
    rectf(1*SDELTA,SMPTEYLOW,2*SDELTA,SMPTEYBASE);
    gamRGBcolor(191,0,191);
    rectf(2*SDELTA,SMPTEYLOW,3*SDELTA,SMPTEYBASE);
    gamRGBcolor(0,0,0);
    rectf(3*SDELTA,SMPTEYLOW,4*SDELTA,SMPTEYBASE);
    gamRGBcolor(0,191,191);
    rectf(4*SDELTA,SMPTEYLOW,5*SDELTA,SMPTEYBASE);
    gamRGBcolor(0,0,0);
    rectf(5*SDELTA,SMPTEYLOW,6*SDELTA,SMPTEYBASE);
    gamRGBcolor(191,191,191);
    rectf(6*SDELTA,SMPTEYLOW,7*SDELTA,SMPTEYBASE);

    gamRGBcolor(0,76,127);
    rectf(0*BDELTA,0.0,1*BDELTA,EIAYBASE);	/* -I */
    gamRGBcolor(255,255,255);
    rectf(1*BDELTA,0.0,2*BDELTA,EIAYBASE);	/* W */
    gamRGBcolor(75,0,139);
    rectf(2*BDELTA,0.0,3*BDELTA,EIAYBASE);	/* +Q */
    gamRGBcolor(0,0,0);
    rectf(3*BDELTA,0.0,7*SDELTA,SMPTEYLOW);

    gamRGBcolor(11,11,11);
    rectf(5*SDELTA,0.0,5.3333*SDELTA,SMPTEYLOW);

    gamRGBcolor(0,0,0);
    rectf(7*SDELTA,0.0,8*SDELTA,1.0);
}

FULLbars()
{
    gamRGBcolor(191,191,191);
    rectf(0*SDELTA,0.0,1*SDELTA,1.0);
    gamRGBcolor(191,191,0);
    rectf(1*SDELTA,0.0,2*SDELTA,1.0);
    gamRGBcolor(0,191,191);
    rectf(2*SDELTA,0.0,3*SDELTA,1.0);
    gamRGBcolor(0,191,0);
    rectf(3*SDELTA,0.0,4*SDELTA,1.0);
    gamRGBcolor(191,0,191);
    rectf(4*SDELTA,0.0,5*SDELTA,1.0);
    gamRGBcolor(191,0,0);
    rectf(5*SDELTA,0.0,6*SDELTA,1.0);
    gamRGBcolor(0,0,191);
    rectf(6*SDELTA,0.0,7*SDELTA,1.0);

    gamRGBcolor(0,0,0);
    rectf(7*SDELTA,0.0,8*SDELTA,1.0);
}

drawit(type)
int type;
{
    reshapeviewport();
    ortho2(0.0,8.0,0.0,1.0);
    ColorBars(type%NBARS);
}

main()
{
    short val;
    int type;

    type = BARS_EIA;
    prefsize(644,483);
    winopen("colorbars");
    RGBmode();
    gconfig();
    drawit(type);
    qdevice(LEFTMOUSE);
    while(1) {
	switch(qread(&val)) {
	    case LEFTMOUSE:
		if(val) 
		    drawit(++type);
		break;
	    case REDRAW:
		drawit(type);
	}
    }
}

gamRGBcolor(r,g,b)
int r, g, b;
{
    r = gammap(r);
    g = gammap(g);
    b = gammap(b);
    RGBcolor(r,g,b);
}

gammap(v)
int v;
{
    return (int)(255.0*pow(v/255.0,GAMMA)+0.5);
}
