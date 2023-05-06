/*
 *  rosetta.c:
 *
 *    Draws rotated strings of characters with base lines 
 *    which form a "rosetta" symbol.
 *
 *				Glen Williams - 1989
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

#define BORDER 30
#define RADIUS 50

double atof();

char *str = "Silicon Graphics";
int stwidth;
double m[3][2];

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


    if(argc<3) {
	printf("usage: rosetta fontname size [string]\n");
	printf("Draws rotated strings of characters with base lines.\n");
	exit(1);
    }
    --argc;

    fontname = argv[1]; --argc;
    size = atof(argv[2]); --argc;
    if (argc > 0)
	str = argv[3];

    fminit();
    f = fmfindfont(fontname);
    if (!f) {
	printf("can't find font \"%s\"\n", fontname);
	exit (-1);
    }
    f2 = fmscalefont(f, size);
    fmsetfont(f2);
    fmgetpagematrix(m);

    stwidth = fmgetstrwidth(f2, str);
    if(stwidth <=0) {
	printf("string width is zero\n");
	exit(-1);
    }

    prefsize(2*(stwidth+BORDER+RADIUS), 2*(stwidth+BORDER+RADIUS));
    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    wintitle(winname);
    color(WHITE);
    clear();
    color(BLACK);
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
    double angle;
    int incr = 40;
    int i,j, basis;

    reshapeviewport();
    color(WHITE);
    clear();
    color(BLUE);	/* blue base lines */
    for(basis=incr, j=0;j<2; basis=incr/2, j++) {
        for(i=0; i*incr<=360; i++) {
	    fmsetpagematrix(m);	/* reset to 0 rot */
            angle = (double)(basis+i*incr);
            fmrotatepagematrix(angle);
            pushmatrix();
            translate(stwidth+BORDER+RADIUS,stwidth+BORDER+RADIUS,0);
            angle = (double)(basis+i*incr);
            rotate((int)(angle*10.0), 'z');
            move2i(0,0);
            draw2i(500,0);
            color(BLACK); 
            cmov2i(RADIUS,0);
            fmprstr(str);
            color(YELLOW);	    /* put a yellow dot at (0,0) */
            pnt2i(0,0);
            popmatrix();
	    color(BLUE);
        }
    }
}
