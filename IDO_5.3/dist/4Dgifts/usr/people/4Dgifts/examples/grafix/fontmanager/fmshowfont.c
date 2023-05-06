/*
 *  showfont.c
 *
 *    Graphically display all the characters in the font specified 
 *  associating each with its ascii numeric index.
 *
 *				Glen Williams - 1987
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

fmfonthandle fsized;
fmfontinfo finfo;
fmfontinfo times_info;
fmfonthandle times_sized;
char fname[200];
int x, y,ystart, rows, columns, nglyphs;
int column_width, xstart;
int noscroll = 0;
int char_start, char_end;

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle tf;
    char winname[100];
    short val;
    int wwidth, wheight;
    float size;


    if(argc<3) {
	printf("usage: showfont fontname size\n");
        printf("Print all characters in a font, using height for leading\n");
	exit(2);
    }
    size = atof(argv[2]);

    fminit();
    f = fmfindfont(argv[1]);
    tf = fmfindfont("Times-Roman");
    if (!f) {
	printf("can't find font \"%s\"\n", argv[1]);
	exit (-1);
    }
    if (!tf) {
	printf("can't find Times-Roman\n");
	exit (-1);
    }
    times_sized = fmscalefont(tf, 12.0);
    fsized = fmscalefont(f, size);
    fmsetfont(times_sized);
    fmgetfontinfo(times_sized, &times_info);
    fmgetfontinfo(fsized, &finfo);

    nglyphs = finfo.nglyphs;
    ystart = 10000;
    columns = 7;

    /* 37 is for window border, need a full font height for the characters,
    ** and then an additional 1/2 height for space at the top.
    */
    while (ystart+37+3*finfo.height/2 > getgdesc(GD_YPMAX)) {
	columns++;
	rows = nglyphs / columns;
	if(nglyphs > columns && nglyphs%columns != 0)
	    rows++;
	ystart = y = finfo.height*(rows)-(finfo.height/2);
    }

    if (ystart <= 0)	    /* if rows == 0, ystart ends up negative */
	ystart = finfo.yorig;

    column_width = fmgetstrwidth(times_sized, "99999: ") + finfo.xsize;

    fmgetfontname(fsized, 200, fname);
    wwidth = columns*column_width;
    /* window height needs to be n+1 * font height for n rows of
    ** characters plus one half font height spacing on the top
    ** and bottom.
    */
    wheight = (rows+1)*finfo.height;

    if((wwidth < getgdesc(GD_XPMAX)) && (wheight < getgdesc(GD_YPMAX))) {
	prefsize(wwidth, wheight);
	noscroll = 1;
    }
    if(wwidth<=0 || wheight <=0) {
	printf("zero-height or zero-width font\n");
	exit(-1);
    }
    /* initial window size if font is really wide. */
    else minsize(getgdesc(GD_XPMAX)/2, wheight);

    char_start = 0;

    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    winconstraints();
    wintitle(winname);
    qdevice(KEYBD);
    qdevice(ESCKEY);
    qdevice(LKEY);
    qdevice(RKEY);
    qdevice(RIGHTARROWKEY);
    qdevice(LEFTARROWKEY);

    drawit(REDRAW, 0);
    while(1) {
	switch(qread(&val)) {
 	    case ESCKEY:
		gexit();
	        exit(0);
                break;
	    case REDRAW:
     	        qreset();
	        drawit(REDRAW,val);
	        break;
	    case LKEY:
     	        qreset();
	        if(val)
		    drawit(LKEY,val);
	        break;
	    case LEFTARROWKEY:
     	        qreset();
	        if(val)
		    drawit(LKEY,val);
	        break;
	    case RKEY:
     	        qreset();
	        if(val)
		    drawit(RKEY,val);
	        break;
	    case RIGHTARROWKEY:
     	        qreset();
	        if(val)
	    	    drawit(RKEY,val);
	        break;
	    case KEYBD:
#if 0
     	        qreset();
#endif
	        if(val)
		    drawit(KEYBD,val);
	        break;
	}
    }
}


drawit(direction,val)
int	direction;
short	val;
{
    unsigned int i;
    int vis_columns, vis_chars;
    long win_width, win_height;
    static inited;

    if(direction==KEYBD)
	switch(val) {
	    case 'l':
	    case 'L':
	    case 'r':
	    case 'R':
	    case 27 :	    /* ESC */
		return(0);
	}	

    reshapeviewport();
    color(7);
    clear();
    color(0);
    
    xstart = x = 0;
    y = ystart;
    cmov2i(x,ystart);

    getsize(&win_width, &win_height);
    ortho2(-.5, win_width -.5, -.5, win_height -.5);

    vis_columns = (win_width) / column_width;
    vis_chars = vis_columns * rows;

    switch(direction) {
	case REDRAW:
	    char_end = char_start + vis_chars;
	    break;
	case LKEY:
	    char_start += vis_chars;
	    char_end = char_start + vis_chars;
	    break;
	case RKEY:
	    char_start -= vis_chars;
	    if(char_start < 0)
		char_start = 0;
	    char_end = char_start + vis_chars;
	    break;
    }

    if(char_end > nglyphs)
	char_end = nglyphs;


    y = ystart;
    for(i=char_start; i<char_end; i++) {
	if(i % rows == 0 && i != char_start) {
	    y = ystart;
	    x += column_width;
	} else {
	    if(i != char_start)
		y-= finfo.height;
	}
	sprintf(fname, "%5d: ", i);
	cmov2i(x,y);
	fmprstr(fname);
        fmoutchar(fsized, i);
    }

/* put up a help message */
    if(!inited && !noscroll)
    {
    int hlen, inset;
    char * hstring;
    Scoord vleft, vright, vbottom,vtop;
    hstring ="  Hit Left-Arrow to scroll left, Right-Arrow to scroll right, ESC to exit  ";

    hlen=fmgetstrwidth(times_sized, hstring);
    inset = (win_width-hlen)/2;
    if(inset<0)
	inset = 0;
    vleft   =	inset;
    vright  =	win_width - inset;
    vbottom =	((win_height/2)-times_info.height);
    vtop    =	(win_height/2)+times_info.height;

    /* draw a rectangle for the text */
    color(7);
    rectfi(vleft, vbottom, vright, vtop);
    color(0);
    linewidth(3);
    recti(vleft+2, vbottom+2, vright-2, vtop-2);
    cmov2i(inset, vbottom+((vtop-vbottom)/3));
    fmprstr(hstring);
    inited = 1;
    }
}
