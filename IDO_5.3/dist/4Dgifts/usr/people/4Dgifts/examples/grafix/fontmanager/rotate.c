/*
 *  rotate.c:
 *
 *    Write a rotated string using the fmrotatepagematrix Font Manager 
 *  routine.  This routine post-concatenates a rotation to the page matrix.
 *  Rotation is measured in a counter-clockwise direction in degrees.
 *	
 *				Glen Williams - 1989
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

char *str = "Silicon Graphics";

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f, f2;
    char winname[100];
    char *fontname;
    char c;
    short val;
    int x, y, i, slen;
    float size;
    float angle;


    if(argc<4) {
	printf("usage: rotate fontname size angle [string]\n");
	printf("employs fmrotatepagematrix to draw a rotated string.\n");
	exit(1);
    }
    --argc;
    fontname = argv[1]; --argc;
    size = atof(argv[2]); --argc;
    angle = atof(argv[3]); --argc;
    if (argc > 0)
	str = argv[4];

    fminit();
    f = fmfindfont(fontname);
    if (!f) {
	printf("can't find font \"%s\"\n", fontname);
	exit (-1);
    }
    f2 = fmscalefont(f, size);
    fmsetfont(f2);
    fmrotatepagematrix(angle);

    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    wintitle(winname);
    color(7);
    clear();
    color(0);
    qdevice(KEYBD);

    drawit(str);
    while(1) {
	switch(qread(&val)) {
	case KEYBD:
	    exit(0);
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
    long w, h;

    reshapeviewport();
    getsize(&w, &h);
    ortho2(-.5,w-.5,-.5,h-.5);
    color(7);
    clear();
    color(0);
    cmov2i(250,250);
    fmprstr(str);
}
