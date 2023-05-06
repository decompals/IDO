/*
 *	cedit - 
 *		A simple color editor. Use the left mouse button to 
 *	pick the color on the screen to edit, then move the sliders to 
 *	change the color.
 *
 *				Paul Haeberli - 1984
 *
 */
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include "port.h"

#define DISPSIZE	150

float or = -1.0;
float og = -1.0;
float ob = -1.0;
int cc = -1;
float cr, cg, cb;
int m1, m2, m3;
long xorg, yorg;
int x, y;
long xsize, ysize;
float wx, wy;
int menu;
int togglehex = 0;
unsigned short mask = 0x0fff;
int sngl =1;

main(argc,argv)
int argc;
char **argv;
{
    int i, j, bits;

    if (argc>1)
	setcolorsys(atoi(argv[1]));
    bits = getgdesc(GD_BITS_NORM_SNG_CMODE);
    glcompat(GLC_SOFTATTACH, TRUE);
    minsize(135,135);
    keepaspect(1,1);
    winopen("cedit");
    keepaspect(1,1);
    winconstraints();
    menu = defpup("colorsys %t|rgb|cmy|hsv|hls|toggle numbers");
    if(bits <= 8) {
      addtopup(menu, "doublebuffer");
      mask = 0x00ff;
    }
    initmouse();
    if(getplanes()<=9)	/* greybase hardcoded to 512, so no good if less */
    	cc = 64;
    else
	cc = 576;	/* what it used to be */
    makeframe();
    while (1) {
	checkmouse();
	if (m3) {
	    x = getvaluator(MOUSEX);
	    y = getvaluator(MOUSEY);
	    wx = 10.0*(x-xorg)/xsize;
	    wy = 10.0*(y-yorg)/ysize;
	    if (wy>0.0 && wy<10.0) {
		if (wx>1.0 && wx<2.0) {
		     cr = (wy-2.0)/7.0;
		     drawsliders(cc,cr,cg,cb);
		} else if (wx>3.0 && wx<4.0) {
		     cg = (wy-2.0)/7.0;
		     drawsliders(cc,cr,cg,cb);
		} else if (wx>5.0 && wx<6.0) {
		     cb = (wy-2.0)/7.0;
		     drawsliders(cc,cr,cg,cb);
		}
	    } 
	}
    }
}

newcolor(c)
int c;
{
    int savetoggle;

    cc = c;
    color(c);
    rectf(7.0,2.0,9.0,9.0);
    grey(0.0);
    rect(7.0,2.0,9.0,9.0);
    modgetmcolor(c,&cr,&cg,&cb);
    or = og = ob = -1.0;
    drawback();
    drawsliders(cc,cr,cg,cb);
    if(xsize>DISPSIZE) {
	savetoggle = togglehex;
	togglehex = 0;
	centernum(cc,8.0,0.3);
	togglehex = savetoggle;
    }
}

drawsliders(c,r,g,b)
int c;
float r, g, b;
{
    int changed;

    changed = 0;
    if (r<0.0) r = 0.0;
    if (r>1.0) r = 1.0;
    if (r != or) {
	drawknob(0.0,or,r,rgb(1.0,0.0,0.0));
        or = r;
	if(xsize>DISPSIZE) 
	    centernum((int)(r*255),1.5,0.3);
	changed++;
    }
    if (g<0.0) g = 0.0;
    if (g>1.0) g = 1.0;
    if (g != og) {
	drawknob(2.0,og,g,rgb(0.0,1.0,0.0));
        og = g;
	if(xsize>DISPSIZE) 
	    centernum((int)(g*255),3.5,0.3);
	changed++;
    }
    if (b<0.0) b = 0.0;
    if (b>1.0) b = 1.0;
    if (b != ob) {
	drawknob(4.0,ob,b,rgb(0.0,0.0,1.0));
        ob = b;
	if(xsize>DISPSIZE) 
	    centernum((int)(b*255),5.5,0.3);
	changed++;
    }
    if (changed) 
	modmapcolor(c,r,g,b);
}

drawknob(x,old,new,c)
float x, old, new;
int c;
{
     pushmatrix();
	 translate(x,2.0+(7.0*old),0.0);
	 grey(0.9);
	 rectf(1.2,-0.10,1.8,0.10);	
     popmatrix();
     pushmatrix();
	 translate(x,2.0+(7.0*new),0.0);
	 color(c);
	 rectf(1.2,-0.10,1.8,0.10);	
     popmatrix();
}

makeback(x)
float x;
{
    pushmatrix();
        translate(x,0.0,0.0);
	grey(0.9);
	rectf(1.0,1.6,2.0,9.4);
	grey(0.0);
	rect(1.0,1.6,2.0,9.4);
    popmatrix();
}

drawback()
{
    makeback(0.0);
    makeback(2.0);
    makeback(4.0);
}

initmouse()
{
    qdevice(MENUBUTTON);
    qdevice(MIDDLEMOUSE);
    qdevice(LEFTMOUSE);
}

checkmouse()
{
    short dev, val;
    int sel;

    while (1) {
	if (m1 != 0 || m2 != 0 || m3 != 0) {
	    if (!qtest()) 
		return;
	} 
	dev = qread(&val);
	switch (dev) {
		case MENUBUTTON: 
			sel = dopup(menu);	
			if (sel>0) {
			    if (sel == 5)
				togglehex = (togglehex == 1) ? 0 : 1;
			    else if (sel == 6)
				togglemenu();
			    else 
				setcolorsys(sel);
			    newcolor(cc);
			}
			break;
		case MIDDLEMOUSE: 
			m2 = val;
			if (m2 == 0) {
			    x = getvaluator(MOUSEX);
			    y = getvaluator(MOUSEY);
			    wx = 10.0*(x-xorg)/xsize;
			    wy = 10.0*(y-yorg)/ysize;
			    if (wx<-0.5 || wx>10.5 || wy<-0.5 || wy>10.5)
				modmapcolor(mask&getapixel(x,y),cr,cg,cb);
			}
			break;
		case LEFTMOUSE: 
			m3 = val;
			if (m3 == 0) {
			    x = getvaluator(MOUSEX);
			    y = getvaluator(MOUSEY);
			    wx = 10.0*(x-xorg)/xsize;
			    wy = 10.0*(y-yorg)/ysize;
			    if (wx<-0.5 || wx>10.5 || wy<-0.5 || wy>10.5)
				newcolor(mask&getapixel(x,y));
			}
			break;
		case REDRAW: 
			makeframe();
			break;
	}
    }
}

makeframe()
{
    reshapeviewport();
    getorigin(&xorg,&yorg);
    getsize(&xsize,&ysize);
    grey(0.8);
    clear();
    ortho2(0.0,10.0,0.0,10.0);
    grey(0.0);
    newcolor(cc);
}

modmapcolor(c,r,g,b)
int c;
float r, g, b;
{
    float fr, fg, fb;
    int ir, ig, ib;

    torgb(r,g,b,&fr,&fg,&fb);
    rgb_to_irgb(fr,fg,fb,&ir,&ig,&ib);
    gammapcolor(c,ir,ig,ib);
}

modgetmcolor(c,r,g,b)
int c;
float *r, *g, *b;
{
    unsigned short cr, cg, cb;
    float fr, fg, fb;

    gamgetmcolor(c,&cr,&cg,&cb);
    irgb_to_rgb(cr,cg,cb,&fr,&fg,&fb);
    fromrgb(fr,fg,fb,r,g,b);
}

centernum(val,x,y)
int val;
float x, y;
{
    char buf[128];
    int pixlen;
    float flen;

    grey(0.8);
    rectf(x-1.0,y-0.3,x+1.0,y+1.2);
    grey(0.0);
    if (togglehex)
        sprintf(buf,"0x%02x",val);
    else
        sprintf(buf,"%d",val);
    pixlen = strwidth(buf);
    flen = (pixlen*10.0)/xsize;
    cmov2(x-flen/2.0,y);
    charstr(buf);
}

togglemenu()
{
	freepup(menu);

	menu = defpup("colorsys %t|rgb|cmy|hsv|hls|toggle numbers");
        if(!sngl) {
	  addtopup(menu, "doublebuffer");
	  mask = 0x00ff;
	}
	else {
	  addtopup(menu, "singlebuffer");
	  mask = 0x000f;
	}
	sngl = 1 - sngl;
}
