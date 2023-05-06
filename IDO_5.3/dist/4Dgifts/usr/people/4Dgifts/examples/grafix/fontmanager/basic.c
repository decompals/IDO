/*
 *  basic.c:
 *
 *    Print a string using the specified font in the closest size available 
 *  to the "size" specified.
 *
 *				Glen Williams - 1987
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle fsized;
    char winname[100];
    short val;
    char *str;
    float size;


    if(argc<3) {
	printf("usage: basic fontname size [str]\n");
	printf("Print a string in 'fontname' in the size closest to 'size'\n");
	exit(1);
    }

    size = atof(argv[2]);
    if(argc>3)
	str = argv[3];
    else
	str = "basic.c:  A string of sample text.";

    fminit();
    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font \"%s\"\n", argv[1]);
	exit (-1);
    }
    fsized = fmscalefont(f, size);
    fmsetfont(fsized);

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
    long w, h;

    reshapeviewport();
    getsize(&w, &h);
    ortho2(-.5,w-.5,-.5,h-.5);
    color(7);
    clear();
    color(0);
    cmov2i(50,50);
    fmprstr(str);
}
