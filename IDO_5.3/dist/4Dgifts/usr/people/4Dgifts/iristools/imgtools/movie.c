/*
 *	movie - 
 *		Show a series of images as a movie.
 *
 *				Paul Haeberli - 1985	
 */
#include "math.h"
#include "image.h"
#include "gl.h"
#include "device.h"
#include "port.h"
#include "dispimg.h"

float fgetmousex();
DISPIMAGE *myreadimage();

#define XBORDER		8
#define TOPBORDER	8
#define BOTBORDER	40

#define STOPPED		1
#define LOOPING		2
#define MOUSEPOS	3
#define FORBACK		4
#define SWINGING	5

#define FORWARD		0
#define BACK		1

DISPIMAGE *movie[1000];
int xsize, ysize;
int xframe, yframe;
int sxmode = 0;
int nframes;
int curframe;
int menu;
float zoom;
int mode = SWINGING;
int direction = FORWARD;

main(argc,argv)
int argc;
char **argv;
{
    register short *sptr;
    register int i, j = 0;
    short val;
    int inf;

    sxmode = 0;
    zoom = 1.0;
    for(i=1; i<argc; i++) {
	if(argv[i][0] == '-') {
	    if(strcmp(argv[i],"-sx") == 0) 
		sxmode = 1;
	    else if(strcmp(argv[i],"-z") == 0) {
		i++;
		zoom = atof(argv[i]);
	    } 
	}
    }
    curframe = 0;
    for(i=1; i<argc; i++) {
	if(argv[i][0] == '-') {
	    if(strcmp(argv[i],"-sx") == 0)
		;
	    else if(strcmp(argv[i],"-z") == 0)
		i++;
	} else
	    addframe(argv[i]);
   	if(nframes>0) 
	    percentdone(100.0*i/argc);
    }
    if(nframes == 0) {
	fprintf(stderr,"usage: movie images . . . [-sx] [-z zoomfactor]\n");
	exit(1);
    }
    percentdone(100.0);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(MENUBUTTON);
    qdevice(INPUTCHANGE);
    qdevice(LEFTARROWKEY);
    qdevice(RIGHTARROWKEY);
    makeframe(0);
    menu = defpup("movie %t|stop|loop|swing|mousepos|for/back");
    while(1) {
	if(mode == LOOPING) {
	    while(!qtest()) 
		step();
	}
	if(mode == SWINGING) {
	    while(!qtest()) 
		swing();
	}
	switch(qread(&val)) {
   	    case LEFTARROWKEY:
		if(val) {
		    direction = BACK;
		    step();
		}
		break;
   	    case RIGHTARROWKEY:
		if(val) {
		    direction = FORWARD;
		    step();
		}
		break;
   	    case MIDDLEMOUSE:
		if(val) {
		    direction = FORWARD;
		    while(getbutton(MIDDLEMOUSE))
		        step();
		}
		break;
   	    case LEFTMOUSE:
		if(val) {
		    if(mode == MOUSEPOS) {
		        while(getbutton(LEFTMOUSE))
			    gotoframe((int)(nframes*fgetmousex()));
		    } else {
		        direction = BACK;
		        while(getbutton(LEFTMOUSE))
		            step();
		    }
		}
		break;
   	    case MENUBUTTON:
		if(val) {
		    switch(dopup(menu)) {
			case 1:
			    mode = STOPPED; 
			    break;
			case 2:
			    mode = LOOPING; 
			    break;
			case 3:
			    mode = SWINGING; 
			    break;
			case 4:
			    mode = MOUSEPOS; 
			    break;
			case 5:
			    mode = FORBACK; 
			    break;
		    }
		}
		break;
   	    case REDRAW:
		reshapeviewport();
		frontbuffer(1);
		makeframe(curframe);
		frontbuffer(0);
		break;
	}
    }
}

back()
{
    curframe--;
    if(curframe<0)
	curframe = nframes-1;
    makeframe(curframe);
    swapbuffers();
}

forward()
{
    curframe++;
    if(curframe >= nframes)
	curframe = 0;
    makeframe(curframe);
    swapbuffers();
}

step()
{
    if(direction == FORWARD)
	forward();
    else
	back();
}

swing()
{
    int dir;

    if(curframe == 0)
	direction = FORWARD;
    if(curframe == nframes-1)
	direction = BACK;
    step();
}

gotoframe(n)
int n;
{
    if(n >= nframes)
	n = nframes-1;
    if(n<0)
	n = 0;
    if(n!=curframe) {
	curframe = n;
	makeframe(curframe);
	swapbuffers();
    }
}

addframe(filename)
char *filename;
{

    movie[nframes] = myreadimage(filename);
    makeframe(nframes++);
    swapbuffers();
}

makeframe(frameno)
int frameno;
{
    int xoff, yoff;

    curframe = frameno;
    viewport(0,xframe-1,0,yframe-1);
    ortho2(-0.5,xframe-0.5,-0.5,yframe-0.5);
    if(sxmode) {
	grey(1.0);
	rectfi(0,0,xframe-1,BOTBORDER-1);
	rectfi(0,0,XBORDER-1,yframe-1);
	rectfi(xframe-XBORDER,0,xframe-1,yframe-1);
	rectfi(0,yframe-TOPBORDER,xframe-1,yframe-1);
	color(0);
	recti(XBORDER-1,BOTBORDER-1,xframe-XBORDER,yframe-TOPBORDER);
    }
    if(sxmode) {
	xoff = XBORDER;
	yoff = BOTBORDER;
    } else {
	xoff = 0;
	yoff = 0;
    }
    drawimage(movie[frameno],xoff,yoff);
}

DISPIMAGE *myreadimage(name)
char *name;
{
    char fullname[100];
    register IMAGE *image;
    DISPIMAGE *di;
    static int firsted = 0;

    findname(name,fullname,"GFXPATH");
    image = iopen(fullname,"r");
    if(!image) {
	printf("readimage: can't find image %s along GFXPATH\n",name);
	exit(0);
    }
    if(!firsted) {
        xsize = image->xsize;
        ysize = image->ysize;
	if(sxmode) {
	    xframe = zoom*xsize+XBORDER+XBORDER;
	    yframe = zoom*ysize+BOTBORDER+TOPBORDER;
	} else {
	    xframe = zoom*xsize;
	    yframe = zoom*ysize;
	}
	firsted++;
	prefsize(xframe,yframe);
	winopen("movie");
	rectzoom(zoom,zoom);
	doublebuffer();
	gconfig();
        setimagemode(image);
	grey(0.5);
	clear();
	swapbuffers();
    }
    di = makedisp(image);
    iclose(image);
    return di;
}
