/*
 *  matched.c:
 *
 *    Draw a string with the printer matching set.
 *
 *    Many applications render text on the screen to give the user the 
 *  chance to proof the text before printing it on a laser printer.  For a
 *  more realistic simulation, use laser printer character widths to 
 *  represent the text.
 *
 *    fmprintermatch(0) disables printer matching, fmprintermatch(1)
 *  enables printer matching.  When the Font Manager renders images a 
 *  font, it inspects the state of this variable.  If enabled, the Font
 *  Manager searches for a printer widths file that corresponds to the
 *  font.  If the file exists, and the font has not yet been sized, the
 *  Font Manager creates a new font.  The Font Manager also updates the
 *  font handle of the current font so that it has character widths that
 *  correspond to the laser printer's width scheme.
 *	
 *				Glen Williams - 1989
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
    char *str;
    char c;
    short val;
    int x, y, i, slen;
    float size;


    if(argc<3) {
	printf("usage: matched fontname size [str]\n");
	printf("Print a string with fmprintermatch enabled\n");
	exit(1);
    }
    size = atof(argv[2]);
    if(argc>3)
	str = argv[3];
    else {
	sprintf(winname, "Demonstration of printer-width %s, point size %d",argv[1],size);
	str = winname;
    }

    fminit();
    fmprintermatch(1);	    /* use printer's widths */
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
    color(7);
    clear();
    color(0);
    
    x=50; y=50;
    cmov2i(x,y);

    fmprstr(str);
    while(1) {
        switch(qread(&val)) {
            case REDRAW:
		reshapeviewport();
		color(7);
		clear();
		color(0);
		cmov2i(x,y);
		fmprstr(str);
                break;
            case KEYBD:
                gexit();
                exit(0);
                break;
        }
    }
}
