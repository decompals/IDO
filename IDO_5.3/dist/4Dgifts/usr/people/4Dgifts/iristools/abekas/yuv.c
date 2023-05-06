/* yuv.c                     (c) 1991 Abekas Video Systems  */
/*                                                          */
/* This program puts an Abekas YUV frame in a window on the */
/* Iris screen.  The YUV file must be free of the syncs and */
/* line numbers found in SCSI transfers.  Engoop may be     */
/* used for this function.                                  */
/*                                                          */
/*       cc -g yuv.c abklib.c -lgl_s -lds  -o yuv           */

#include <stdio.h>
#include <string.h>
#include <device.h>
#include <gl.h>

#define LINES_525

#define IMG_PIXELS 720

#ifdef LINES_525
#define SCR_PIXELS 640
#define LINES 486
#else
#define SCR_PIXELS 768
#define LINES 576
#endif

#define FIELD1 1
#define FIELD2 2
#define FRAME 3

int menu, zoomMenu;
int mainWin, zoomWin, valueWin;
int zoomX, zoomY;
int dispMode, Aspect, fieldFile, Pixels, Lines;
short *piccy, *tmppiccy;
FILE *imagefile;
short blackvec[3] = {0, 0, 0};

drawImage()
{
long *ptr;
int i;

winset(mainWin);
if(dispMode == FRAME)
    {
    if(fieldFile)rectzoom(1.0, 2.0);
    else rectzoom(1.0, 1.0);
    lrectwrite(0, 0, Pixels-1, Lines-1, piccy);
    }
else {
    ptr = (long *)piccy;
    rectzoom(1.0, 2.0);
    if(dispMode == FIELD1)ptr += Pixels;
    for(i=0; i<LINES; i+=2)
	{
	lrectwrite(0, i, Pixels-1, i, ptr);
	ptr += Pixels*2;
	}
    }
}

drawZoom()
{
long mainXorg, mainYorg;
long zoomXorg, zoomYorg;

winset(mainWin);
getorigin(&mainXorg, &mainYorg);
winset(zoomWin);
getorigin(&zoomXorg, &zoomYorg);
rectzoom(8.0, 8.0);
rectcopy(zoomX-zoomXorg+mainXorg-8, zoomY+mainYorg-zoomYorg-8, 
    zoomX+mainXorg-zoomXorg+8, zoomY+mainYorg-zoomYorg+8, 0, 0);
}

setDispMode(mode)
int mode;
{
dispMode = mode;
if(menu) freepup(menu);
menu = defpup("display %t");
if(!fieldFile)
    {
    if(mode == FRAME)addtopup(menu, "->frame %x3");
    else addtopup(menu, "   frame %x3");
    if(mode == FIELD1)addtopup(menu, "->field 1 %x1");
    else addtopup(menu, "   field 1 %x1");
    if(mode == FIELD2)addtopup(menu, "->field 2 %x2");
    else addtopup(menu, "   field 2 %x2");
    }
if(Aspect) addtopup(menu, "   zoom %x4");
}

zoomImage()
{
long mainXorg, mainYorg, mainXsize, mainYsize;
long zoomXorg, zoomYorg, x, y;
short val;

getorigin(&mainXorg, &mainYorg); /* origin of parent window */
getsize(&mainXsize, &mainYsize);

/* open up the window */
if(!zoomWin)
    {
    prefposition(mainXorg-100, mainXorg+28, mainYorg-100, mainYorg+28);
    zoomWin = winopen("zoom");
    RGBmode();
    gconfig();
    }
else winset(zoomWin);
getorigin(&zoomXorg, &zoomYorg); /* origin of parent window */
screenspace(); /* allow rectcopy from screen */
qdevice(LEFTMOUSE);
while(1)
    {
    if(qtest())
	if(qread(&val) == LEFTMOUSE) break;
    x = getvaluator(MOUSEX)-mainXorg;
    y = getvaluator(MOUSEY)-mainYorg;
    if((x >= 8) && (x < (mainXsize-7))
	&& (y >= 8) && (y < (mainYsize-7)))
	{
	zoomX = x;
	zoomY = y;
	drawZoom();
	}
    }
winset(mainWin);
}

getValue()
{
long zoomXorg, zoomYorg;
char str[80];
long *ptr;
short *sptr;
int r, g, b, X, Y, x, y, u, v, offset;

/* open up the window */
if(!valueWin)
    {
    getorigin(&zoomXorg, &zoomYorg); /* origin of parent window */
    prefposition(zoomXorg-160, zoomXorg+28, zoomYorg-55, zoomYorg+14);
    valueWin = winopen("value");
    color(BLACK);
    clear();
    }
else {
    winset(valueWin);
    X = (getvaluator(MOUSEX)-zoomXorg)>>3;
    Y = (getvaluator(MOUSEY)-zoomYorg)>>3;
    ptr = (long *)piccy;
    offset = (Pixels * (Y+zoomY+4)) +X+zoomX+4;
    ptr += offset;
    r = *ptr & 0xFF;
    g = (*ptr>>8) & 0xFF;
    b = (*ptr>>16) & 0xFF;
    /*
    fseek(imagefile, ((offset<<1) & 0xFFFC), 0);
    fread(&x, 1, 4, imagefile);
    */
    sptr = tmppiccy;
    sptr += (offset & 0xFFFFFE);;
    x = *(long *)sptr;
    if(1 & offset) y = x & 0xFF;
    else y = (x>>16) & 0xFF;
    u = (x>>24) & 0xFF;
    v = (x>>8) & 0xFF;
    color(BLACK);
    clear();
    color(WHITE);
    sprintf(str, "x %d y %d", X+zoomX+4, LINES - (Y+zoomY+4+1)); 
    cmov2i(10, 55);
    charstr(str);
    sprintf(str, "y %02X u %02X v %02X", y, u, v);
    cmov2i(10, 30);
    charstr(str);
    sprintf(str, "r %02X g %02X b %02X", r, g, b);
    cmov2i(10, 5);
    charstr(str);
    }
}

int whichWindow()
{
long mainXorg, mainYorg, mainXsize, mainYsize;
long x, y;
/* which window */
winset(mainWin);
getorigin(&mainXorg, &mainYorg);
getsize(&mainXsize, &mainYsize);
x = getvaluator(MOUSEX);
y = getvaluator(MOUSEY);
if((x >= mainXorg) && (x < (mainXsize+mainXorg))
  && (y >= mainYorg) && (y < (mainXsize+mainYorg)))
    return mainWin;
else return zoomWin;
}

main(argc, argv)
int argc;
char *argv[];
{
short dev, val, sel;
short *from;
long *to;
int i;
char *name;


if((argc < 2) | (argc > 3))
    {
    printf("Usage: yuv [-af] <filename>\n");
    printf("-a turns off aspect ratio correction\n");
    printf("-f says file contains a field\n");
    exit(0);
    }
name = argv[1];
if(*name == '-')
    {
    name++;
    while(*name)
	{
	if(*name == 'a')
	    Aspect = 1;
	else if(*name == 'f')
	    fieldFile = 1;
	else {
	    printf("Unknown option -%c\n", *name);
	    exit();
	    }
	name++;
	}
    name = argv[2];
    }
if(!(imagefile = fopen(name, "r")))
    {
    printf("yuv: unable to open: %s\n", name);
    exit(0);
    }

/* strip off path from filename */
while(strrchr(name, '/')) name = strrchr(name, '/')+1;
if(Aspect) Pixels = IMG_PIXELS;
else Pixels = SCR_PIXELS;
if(fieldFile) Lines = LINES/2;
else Lines = LINES;

/* open up the window */
prefsize(Pixels-1, LINES-1);
mainWin = winopen(name);
RGBmode();
gconfig();
c3s(blackvec);
clear();

/* initialise the pop up menus */
setDispMode(FRAME);

zoomMenu = defpup("value");

/* read the image into temp memory */
tmppiccy = (short *)malloc(Pixels*Lines*2);
piccy = (short *)malloc(IMG_PIXELS*Lines*4);
fread(piccy, 1, IMG_PIXELS*Lines*2, imagefile);

/* now flip top to bottom and correct aspect ratio */
flip(Aspect, Lines, tmppiccy, piccy);
/* .. then convert to RGB */

if(fieldFile) rectzoom(1.0, 2.0);
from = tmppiccy;
to = (long *)piccy;
for(i=0; i<LINES; i+=(fieldFile+1))
    {
    rgbmatrix(Pixels, to, from);
    lrectwrite(0, i, Pixels-1, i, to); /* display as we go */
    from += Pixels;
    to += Pixels;
    }

/*
free(tmppiccy);
*/
/*
drawImage();
*/

/* main event loop */
qdevice(REDRAW);
qdevice(MENUBUTTON);
while(TRUE)
    switch(dev = qread(&val))
	    {
	    case REDRAW:
		if(val == mainWin) drawImage();
		else drawZoom();
		break;
	    case LEFTMOUSE:
		if(whichWindow() != mainWin)
		    getValue();
		break;
	    case MENUBUTTON:
		if(whichWindow() == mainWin)
		    switch(sel = dopup(menu))
			{
			case FIELD1: /* field 1 display */
			case FIELD2: /* field 2 display */
			case FRAME: /* frame display */
			    setDispMode(sel);
			    drawImage();
			    break;
			case 4: /* zoom display */
			    zoomImage();
			default:
			    break;
			}
		 else {	
		    winset(zoomWin);
		    switch(sel = dopup(zoomMenu))
			{
			case 1:
			    getValue();
			    break;
			default:
			    break;
			}
		    }
	    }	
}
