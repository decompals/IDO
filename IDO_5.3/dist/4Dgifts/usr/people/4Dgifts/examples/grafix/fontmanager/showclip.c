/*
 *  showclip.c:
 *	 
 *    Fine clipping example that demonstrates manually calculating the
 *  clipping region's bounds.
 *
 *				Glen Williams - 1989
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

fmfontinfo finfo;
char *str;
int xstart, ystart;

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
	printf("usage: showclip fontname size [str]\n");
	printf("display a string with manually calculated fine clipping\n");
	exit(1);
    }
    size = atof(argv[2]);
    if(argc>3)
	str = argv[3];
    else
	str = "Hello World\!";

    fminit();
    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font \"%s\"\n", argv[1]);
	exit (-1);
    }
    fsized = fmscalefont(f, size);
    fmsetfont(fsized);
    fmgetfontinfo(fsized, &finfo);

    xstart=ystart=5*finfo.height+finfo.height;

    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    wintitle(winname);
    qdevice(KEYBD);

    drawit();
    while(1) {
	switch(qread(&val)) {
	case KEYBD:
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
    Scoord vleft, vright, vbottom, vtop;
    Coord oleft, oright, obottom, otop;

    reshapeviewport();
    color(7);
    clear();
    color(0);

    pushviewport();
    pushmatrix();
    getscrmask(&left, &right, &bottom, &top);
    getsize(&win_width, &win_height);
    getorigin(&ox, &oy); /*find the lower left corner of window */

    fontwidth = finfo.xsize*2; /* fudge factor for setting up new viewport */
    fontheight = finfo.ysize*2;

    vleft   =	fontwidth;
    vright  =	win_width-fontwidth;
    vbottom =	fontheight;
    vtop    =	win_height-fontheight;
    viewport(vleft, vright, vbottom, vtop);

    oleft   =	(Coord)fontwidth-0.5;
    oright  =	(Coord)win_width-(Coord)fontwidth +0.5;
    obottom =	(Coord)fontheight-0.5;
    otop    =    (Coord)win_height-(Coord)fontheight + 0.5;
    ortho2  (oleft, oright, obottom, otop);
    color(1);
    recti(vleft, vbottom, vright, vtop);

    scrmask(vleft+10, vright-10, vbottom+10, vtop-10);

    color(2);
    clear();
    color(0);
    recti(vleft+10, vbottom+10, vright-10, vtop-10);
    for(x=xstart,y=ystart; y>-100; x+=finfo.xsize,y-=finfo.ysize) {
	cmov2i(x,y);    /* can now position outside the original viewport */
	fmprstr(str);
    }
    popviewport();
    popmatrix();
}
