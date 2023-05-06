/*
 *	fntinfo.c
 *		Get overall font information.
 *	
 *				Glen Williams 1987
 */

#include <stdio.h>
#include <gl/gl.h>
#include <fmclient.h>

double atof();
void printname();


main(argc,argv)
int argc;
char *argv[];
{
    int i;
    fmfonthandle f;
    fmfonthandle fsized;
    fmfontinfo finfo;
#define NAMELEN 50
    char fname[NAMELEN];
    float size;

    if(argc!=3) {
	printf("usage: fntinfo fontname size\n");
	printf("Print information about a font\n");
	exit(1);
    }

    size = atof(argv[2]);

    fminit();

    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font\n");
	exit (-1);
    }

    fsized = fmscalefont(f, size);
    fmsetfont(fsized);

    fmgetfontinfo(fsized, &finfo);
    if(fmgetfontname(fsized, NAMELEN, fname) > 0)
	fprintf(stderr, "%s\n", fname);
    if(fmgetcomment(fsized, NAMELEN, fname) > 0)
	fprintf(stderr, "%s\n", fname);

    fprintf(stderr,
	"fmfntinfo:\nprintermatched: %d  type: %d\n", finfo.printermatched,
	 finfo.type);
    fprintf(stderr,
	"Matrix:\t%4.1f\t%4.1f\t%4.1f\t%4.1f\norigin: %d %d\nsize: %d %d\n",
	finfo.matrix00, finfo.matrix01, finfo.matrix10, finfo.matrix11,
	finfo.xorig, finfo.yorig, finfo.xsize, finfo.ysize);
    fprintf(stderr,
	"encoding: %d  fixed_width %d  #glyphs %d  bits deep: %d\n",
	    finfo.encoding, finfo.fixed_width, finfo.nglyphs, finfo.bitsdeep);
    fprintf(stderr,
	"width: %d\n  resolution %d\n  weight %d\n  height %d\n",
	    finfo.width, finfo.resolution, finfo.weight, finfo.height);

}
