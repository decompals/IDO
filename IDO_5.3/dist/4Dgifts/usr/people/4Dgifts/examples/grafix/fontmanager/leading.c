/*
 *  leading.c:
 *
 *    Show the leading of a given font.  "Leading" is the spacing between
 *  lines.   This is demonstrated by writing out three consecutive lines 
 *  of text in a column.
 *
 *				Glen Williams - 1989
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

int height;

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle fsized;
    fmfontinfo finfo;
    char winname[100];
    char *str;
    short val;
    float size;


    if(argc<3) {
	printf("usage: leading fontname size [str]\n");
	printf("Display the leading of a string using 'fontname' ");
	printf("in a size closest to 'size'\n");
	exit(1);
    }

    size = atof(argv[2]);
    if(argc>3)
	str = argv[3];
    else
	str = "leading.c shows the leading of a given font.";

    fminit();
    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font \"%s\"\n", argv[1]);
	exit (-1);
    }

    fsized = fmscalefont(f, size);
    fmsetfont(fsized);
    fmgetfontinfo(fsized, &finfo);
    height = finfo.height;

    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    wintitle(winname);
    qdevice(KEYBD);

    drawit(str);
    while(1) {
	switch(qread(&val)) {
	case KEYBD:
	    gexit();
	    exit(0);
	    break;
	case REDRAW:
     	    qreset();
	    drawit(str);
	    break;
	}
    }
}


drawit(str)
char *str;
{
    int yloc = 150;
    long w, h;

    reshapeviewport();
    getsize(&w, &h);
    ortho2(-.5,w-.5,-.5,h-.5);
    color(7);
    clear();
    color(0);
    cmov2i(50,yloc);
    fmprstr(str);
    yloc -= height;
    cmov2i(50,yloc);
    fmprstr(str);
    yloc -= height;
    cmov2i(50,yloc);
    fmprstr(str);
}
