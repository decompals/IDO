/*
 *	savemap - 
 *		Save part or all of the color map in a file.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "gl.h"
#include "image.h"
#include <sys/types.h>

#define IS_WRITTEN (1<<24)	/* indicates that cell has been written to */
#define PACK_RGBF(r,g,b,f) ((r) | ((g) << 8) | ((b) << 16) | (f))
#define UNPACK_R(rgbf) ((rgbf) & 0xff)
#define UNPACK_G(rgbf) (((rgbf) >> 8) & 0xff)
#define UNPACK_B(rgbf) (((rgbf) >> 16) & 0xff)

struct glColorCell {
    short indx;
    u_char red;
    u_char green;
    u_char blue;
};

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    register int i, min, max;
    short r, g, b;
    unsigned int *cells = (unsigned int *) 0;
    struct glColorCell glcells[4096], *gptr = &glcells[0];
    int num = 0;

    if( argc<2 ) {
	fprintf(stderr,"usage: savemap out.map [-r min max]\n");
	exit(1);
    } 
    foreground();
    noport();
    winopen("savemap");
    if((argc == 5) && (strcmp(argv[2],"-r") == 0) ) {
	min = atoi(argv[3]);
	max = atoi(argv[4]);
    } else {
	min = 0;
	max = (1<<(getplanes()))-1;
    }
    getmcolor(0xfade, &r, &g, &b);
    if ((u_short) b == 0xabcd) {
	int *data = (int *) (((r << 16) & 0xffff0000) | (u_short) g);
#define _GL_CDATA_Addr		0
#define _GL_CDATA_ShmCells	1
#define _GL_CDATA_Flags		2
#define _GL_CDATA_Cmap		3
#define _GL_CDATA_Visual	4
#define _GL_CDATA_Root		5
#define _GL_CDATA_Dpy		6
#define _GL_CDATA_Screen	7
	if (data && data[_GL_CDATA_Addr] == (int) &data[_GL_CDATA_Addr]) {
	    cells = (unsigned int *) data[_GL_CDATA_ShmCells];
	}
    }

    if (cells) {
	for(gptr = &glcells[0], i = min; i <= max; i++) {
	    if ((cells[i] & IS_WRITTEN) == 0)
		continue;
	    ++num;
	    gptr->indx = i;
	    gptr->red = UNPACK_R(cells[i]);
	    gptr->green = UNPACK_G(cells[i]);
	    gptr->blue = UNPACK_B(cells[i]);
	    ++gptr;
	}
    } else
	num = max - min + 1;
    if( (image=iopen(argv[1], "w", VERBATIM(2), 2, 4, num)) == NULL ) {
	fprintf(stderr,"savemap: can't open input file %s\n",argv[1]);
	exit(1);
    }
    image->colormap = CM_COLORMAP;
    if (cells) {
	for(gptr = &glcells[0], i = 0; i < num; i++, gptr++) {
	    putpix(image, gptr->indx);
	    putpix(image, gptr->red);
	    putpix(image, gptr->green);
	    putpix(image, gptr->blue);
	}
    } else {
	for(i = min; i <= max; i++) {
	    gamgetmcolor(i,&r,&g,&b);
	    putpix(image,i);
	    putpix(image,r);
	    putpix(image,g);
	    putpix(image,b);
	}
    }
    iclose(image);
    gexit();
    exit(0);
}
