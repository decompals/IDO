/*
 *  mult_rotate.c:
 *
 *    Write a "moving" string cummulatively rotated 15 degrees times the 
 *  number of iterations.
 *
 *				Glen Williams - 1987
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle frot;
    char winname[100];
    char *str, c;
    int x, y, i;
    short val;
    float angle;


    if(argc>3) {
	printf("usage: mult_rotate [str] [fontname]\n");
	printf("uses a Font Manager format font file to draw\n");
	printf("a rotated moving string.\n");
	exit(1);
    }
    str = "mutliple rotating string example program using fmrotatepagematrix";  
	
    fminit();
    f = fmfindfont(argc==3 ? argv[2] : "Times-Roman");
    if (!f) {
	printf("can't find font \"%s\"\n", argv[2]);
	exit (-1);
    }

    if(argc >= 2)
	str = argv[1];
    frot = fmscalefont(f, 20.0);
    fmsetfont(frot);

    x=100; y=100;

    sprintf(winname, "%s %s 20", argv[0], str);
    winopen(" ");
    wintitle(winname);
    qdevice(KEYBD);
    color(7);
    clear();
    color(0);

    for(i=0, angle = 15.0; i<10; i++, angle += 15.0, x+=100) {
	fmrotatepagematrix(angle);
    	cmov2i(x,y);
	fmprstr(argc>1 ? argv[1] : str);
    }

    while(1) {
	switch(qread(&val)) {
	    case KEYBD:
                gexit();
		exit(0);
		break;
        }
    }
}
