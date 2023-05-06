/*
 *   line.c:
 *    
 *    a module of swirl which contains the drawing part.  The drawing 
 *  algorithm is taken from the xclock program.  initline() initializes 
 *  the variables of the drawing structure.  drawline() first checks the 
 *  boundary of a new line, removes the first line in the current queue 
 *  (the last line drawn in the GL image), then creates the new line with 
 *  the next color in the color structure, and places this in the last 
 *  item of the line queue.
 *               
*/

#include "swirl.h"

typedef long point[2];

typedef struct {
    int		colornum;
    int		first;
    int		last;
    int		dx1;
    int		dy1;
    int		dx2;
    int		dy2;
    int		x1;
    int 	y1;
    int		x2;
    int		y2;
    int		offset;
    int		delta;
    int		width;
    int		height;
    int		nlines;
    point	*lineq;
}   linestruct;

typedef struct {
    short       red;
    short       green;
    short       blue;
}   color;

static linestruct line;

static color colormap[MAXCOLORS];


void storeRGB()
{
     int h,i,j,k;
     color rgbvector;

     h = 0;
     for (i=0; i<3; i++) {
         rgbvector.red = i*127.5;
	 for (j=0; j<3; j++) {
	     rgbvector.green = j*127.5;
	     for (k=0; k<3; k++) {
		 rgbvector.blue = k*127.5;
		 colormap[h++] = rgbvector;
	     }
	 }
     }
}

int msleep(msec)
unsigned long msec;
{
    poll((struct poll *) 0, (size_t) 0, msec);   /* ms resolution */
    return 0;
}


void initline(display, win)
Display *display;
Window win;
{
    XWindowAttributes wa;
    linestruct *ln = &line;

    srandom(time((long *) 0));
    ln->nlines = (batchcount +1)*2;
    if (!ln->lineq) {
	ln->lineq = (point *)malloc(ln->nlines * sizeof(point));
	memset(ln->lineq, '\0', ln->nlines*sizeof(point));
    }

    XGetWindowAttributes(display,win,&wa);
    ln->width = wa.width;
    ln->height = wa.height;
    ln->delta = 16;
    ln->offset = ln->delta/3;
    ln->last = 0;
    ln->colornum = 0;
    ln->dx1 = random() % ln->delta + ln->offset;
    ln->dy1 = random() % ln->delta + ln->offset;
    ln->dx2 = random() % ln->delta + ln->offset;
    ln->dy2 = random() % ln->delta + ln->offset;
    ln->x1 = random() % ln->width;
    ln->y1 = random() % ln->height;
    ln->x2 = random() % ln->width;
    ln->y2 = random() % ln->height;

    RGBcolor(0,0,0);
    clear();
}

void checkInterval(ln,value,diff,max)
linestruct *ln;
int value;
int *diff;
int max;
{
     if(value < 0)
	*(diff) = (random() % (ln)->delta) + (ln)->offset;
     else if(value > max)
	*(diff) = -(random() % (ln)->delta) - (ln)->offset;
}

void drawline(display, win)
Display* display;
Window   win;
{
     long vector[2];
     linestruct *ln = &line;

     ln->first = (ln->last + 2) % ln->nlines;

     ln->x1 += ln->dx1;
     ln->y1 += ln->dy1;
     ln->x2 += ln->dx2;
     ln->y2 += ln->dy2;
     checkInterval(ln,ln->x1,&ln->dx1,ln->width);
     checkInterval(ln,ln->y1,&ln->dy1,ln->height);
     checkInterval(ln,ln->x2,&ln->dx2,ln->width);
     checkInterval(ln,ln->y2,&ln->dy2,ln->height);

     RGBcolor(0,0,0);
     bgnline();
     v2i(ln->lineq[ln->first]);
     v2i(ln->lineq[ln->first+1]);
     endline();


     RGBcolor(colormap[ln->colornum].red,colormap[ln->colornum].green,colormap[ln->colornum].blue);

     if(++ln->colornum >= MAXCOLORS)
	ln->colornum = 0;
     bgnline();
     vector[0] = ln->x1;
     vector[1] = ln->y1;
     v2i(vector);
     vector[0] = ln->x2;
     vector[1] = ln->y2;
     v2i(vector);
     endline();

     ln->lineq[ln->last][0] = ln->x1;
     ln->lineq[ln->last][1] = ln->y1;
     ln->last++;
     if(ln->last >= ln->nlines)
	ln->last = 0;

     ln->lineq[ln->last][0] = ln->x2;
     ln->lineq[ln->last][1] = ln->y2;
     ln->last++;
     if(ln->last >= ln->nlines)
	ln->last = 0;
}
