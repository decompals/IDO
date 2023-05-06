/*
 *  fineclip.c:
 *
 *    Print characters using fine clipping.
 *
 *    This program demonstrates fine clipping.  Fine clipping is when part
 *  of a character is outside some boundary and is clipped so only part is 
 *  visible.  (Conversely, gross clipping is when the entire character 
 *  isn't imaged if part of it falls outside the clipping boundary.)  
 *  fineclip draws 5.5 lines of text in the bottom of a window.  The text 
 *  on the last line is clipped to the border of the window, cutting 
 *  through the characters.
 *
 *    In order to get this effect, some fudging is required, because of 
 *  the nature of cmov.  If you position the text position to, say, 
 *  (5,-5), the result is undefined if the viewport has not been expanded.
 *  That is, a call to getcpos() will return an undefined coordinate pair.
 *  fmprstr() calls getcpos() to find out where to start imaging a string.
 *    
 *    To get around this, one can enlarge the viewport and reset the 
 *  scrmask, (the call to viewport automatically resizes the scrmask to 
 *  the viewport dimensions.)  Setting the projection to match the new 
 *  viewport allows us to use our window-based coordinate system (or any 
 *  other scaling you wish to impose--this program assumes an 
 *  orthographic projection where there is a 1:1 mapping between world 
 *  space and screen space coordinates.)
 *
 *    This program enlarges the viewport by twice the width of the 
 *  largest character in the font on each side of the port, ensuring 
 *  enough transformation space to move around in.
 *
 *    For more details about fine clipping, see Section 3.1 of the 
 *  "Graphics Library Programming Guide".
 *
 *				      Glen Williams - 1988
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

int xstart, ystart;
unsigned char *str;
fmfontinfo finfo;

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle fsized;
    char winname[100];
    short val;
    float size;


    if(argc<3) {
	printf("usage: fineclip fontname size [str]\n");
	printf("print a string in 'fontname' employing fine clipping\n");
	exit(1);
    }
    
    size = atof(argv[2]);
    if(argc>3)
	str = (unsigned char *)argv[3];
    else
	str = (unsigned char *)"fine clipping font example program.";

    fminit();
    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font \"%s\"\n", argv[1]);
	exit (-1);
    }
    fsized = fmscalefont(f, size);
    fmsetfont(fsized);
    fmgetfontinfo(fsized, &finfo);

    xstart=ystart=5*finfo.height+finfo.height/2;

    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    wintitle(winname);
    qdevice(ESCKEY);

    drawit();
    while(1) {
	switch(qread(&val)) {
	    case ESCKEY:
		gexit();
	        exit(0);
	        break;
	    case REDRAW:
     	        qreset();
	        drawit();
	        break;
	}
    }
}


drawit() {

    Scoord x,y;
    Scoord left,right,bottom,top;
    long fontwidth, fontheight;
    long win_width, win_height;
    long ox, oy;
    Coord oleft, oright, obottom, otop;
    Scoord vleft, vright, vbottom, vtop;

    reshapeviewport();
    color(7);
    clear();
    color(0);

    pushviewport();
    pushmatrix();
    getscrmask(&left, &right, &bottom, &top);
    getsize(&win_width, &win_height);
    getorigin(&ox, &oy);            /*find the lower left corner of window */

    fontwidth = finfo.xsize*2; /* fudge factor for setting up new viewport */
    fontheight = finfo.ysize*2;

    vleft   =	-fontwidth;
    vright  =	win_width+fontwidth;
    vbottom =	-fontheight;
    vtop    =	win_height+fontheight;
    viewport(vleft, vright, vbottom, vtop);

    oleft   =	-0.5-(Coord)fontwidth;
    oright  =	(Coord)win_width+(Coord)fontwidth -0.5;
    obottom =	-0.5-(Coord)fontheight;
    otop    =    (Coord)win_height+(Coord)fontheight - 0.5;
    ortho2  (oleft, oright, obottom, otop);

    scrmask(left, right, bottom, top);

    for(x=xstart,y=ystart; y>-100; x+=finfo.xsize,y-=finfo.ysize) {

	cmov2i(x,y);    /* can now position outside the original viewport */
	fmprstr(str);
    }
    popviewport();
    popmatrix();
}
