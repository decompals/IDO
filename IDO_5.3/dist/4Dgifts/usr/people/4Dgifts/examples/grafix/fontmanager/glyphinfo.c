/*
 *  glyphinfo.c:
 *
 *    Print out all glyph information (x/y -size, origin, etc.) pertaining
 *  to the fontname and pointsize specified.
 *
 *				Glen Williams - 1987
 */

#include <stdio.h>
#include <fmclient.h>

double atof();

main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle fsized;
    fmfontinfo finfo;
    fmglyphinfo *fgs;
    float size;
    int i;


    if(argc<3) {
	printf("usage: glyphinfo fontname size\n");
	printf("Print glyph information about the 'fontname' font with a ");
	printf("pointsize of 'size'.\n");
	exit(1);
    }

    size = atof(argv[2]);
    fminit();
    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font \"%s\"\n",argv[1]);
	exit (-1);
    }
    fsized = fmscalefont(f, size);
    fmsetfont(fsized);
    fmgetfontinfo(fsized, &finfo);
    fprintf(stderr, "fmfntinfo:\nprintermatched: %d  type: %d\n", 
				   finfo.printermatched, finfo.type);
    fprintf(stderr,
	"Matrix:\t%4.1f\t%4.1f\t%4.1f\t%4.1f\norigin: %d %d\nsize: %d %d\n",
	finfo.matrix00, finfo.matrix01, finfo.matrix10, finfo.matrix11,
	finfo.xorig, finfo.yorig, finfo.xsize, finfo.ysize);
    fprintf(stderr,
	"encoding: %d  fixed_width %d  #glyphs %d  bits deep: %d\n",
	    finfo.encoding, finfo.fixed_width, finfo.nglyphs, finfo.bitsdeep);

    fprintf(stderr,"\n\n");

    fgs = (fmglyphinfo *)calloc((int)finfo.nglyphs, sizeof(fmglyphinfo));
    fmgetwholemetrics(fsized, fgs);

    for (i=0; i < finfo.nglyphs; i++, fgs++) {
	fprintf(stderr, "glyph[%d]", i);
	if(i>32)
	    fprintf(stderr, "'%c'", i);
	fprintf(stderr, ":\t");
	fprintf(stderr, "xsize ysize %d %d xorig yorig %d %d xmove ymove %4.2f %4.2f\n\t\tgtype %d bitsdeep %d\n\n",
	fgs->xsize, fgs->ysize, fgs->xorig, fgs->yorig, fgs->xmove,
	fgs->ymove, fgs->gtype, fgs->bitsdeep);
    }
}
