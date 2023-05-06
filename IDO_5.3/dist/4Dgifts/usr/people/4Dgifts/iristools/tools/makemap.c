/*
 * makemap - Reload the colormap
 *
 * Nowadays, GL programs share a common X colormap, which is distinct
 * from the default root colormap.  makemap causes new colors to be
 * stored into that colormap.
 *
 * By default, makemap will initialize all colors from 0-255 except
 * for color cells 16-31.
 *
 * Usage: makemap [-full] [-ignore16to31]
 * Options:
 *	-full		In addition to the default behavior,
 *			write a color cube into 256-512,
 *			and a gray ramp into 513-639.
 *
 *	-ignore16to31	Don't bother trying to fixup cells 16-31;
 *			Just leave these cells alone.
 */
#include <X11/Xlib.h>
#include <gl.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#define NOGREYPATTERNS
#include "colortbl.inc"

flushAndExit(int code)
{
    gflush();
    exit(code);
}

main(argc, argv)
    int argc;
    char **argv;
{
    char *progname = argv[0];
    int i, j, v, r, g, b, w;
    char *cp;
    int ramp, planes;
    int do_fullmap = 0;
    int ignore16to31 = 0;

    int illegal_args = 0;
    int rgb8bit = 0;

    while ((--argc > 0) && (*(cp = *++argv) == '-')) {
	++cp;
	if (strcmp(cp, "full") == 0)
	    do_fullmap = 1;
	else if (strncmp(cp, "ignore", 6) == 0)
	    ignore16to31 = 1;
	else
	    ++illegal_args;
    }

    if (argc > 0 || illegal_args) {
	fprintf(stderr, "%s: usage: %s [-full] [-ignore16to31]\n",
			progname, progname);
	exit(1);
    }

    noport();
    foreground();
    winopen("makemap");

    /*
     * Write to color cells 0-15 and 16-255.
     */

    for(i=0; i<16; i++)
	gammapcolor(i,red_map[i],green_map[i],blue_map[i]);

    /* leave the X part of the map alone */

    for(i=32; i<256; i++)
	gammapcolor(i,red_map[i],green_map[i],blue_map[i]);

    /*
     * Write the minimum #colors to cells 16-31,
     * so as to make 16-31 match the default root X colormap.
     */
    if (!ignore16to31)
	fixup16to31();

    if (do_fullmap &&
	((planes = getgdesc(GD_BITS_NORM_SNG_CMODE)) <= 8))
	flushAndExit(1);

    rgb8bit = (getgdesc(GD_BITS_NORM_SNG_RED) == 3);

    if (!do_fullmap && !rgb8bit)
	flushAndExit(0);		/* we're done */

    if (do_fullmap) {
	/*
	 * Make an ordered color ramp at 256
	 */
	for (i=0; i<256; i++) {
	    r = (i>>0) & 7;
	    g = (i>>3) & 7;
	    b = (i>>6) & 3;
	    r = (255*r)/7;
	    g = (255*g)/7;
	    b = (255*b)/3;
	    gammapcolor(i+256,r,g,b);
	}
	/*
	 * Make a gray ramp of 128 entries
	 */
	ramp = greybase();
	for (i=0; i<128; i++) 
	    gammapcolor(i+ramp,i<<1,i<<1,i<<1);
    }

    /*
     * For 8-bit RGB systems, load 8-bit RGB ramp at top of color map
     */
    if (rgb8bit) {
	w = 0;
	for(i=0; i<4; i++) { 
	    b = i<<6 | i<<4 | i<<2 | i;
	    for(j=0; j<8; j++) {
		g = j<<5 | j<<2 | j>>1;
		for(v=0; v<8; v++) {
		    r = v<<5 | v<<2 | v>>1;
		    gammapcolor(3840+w,r,g,b);
		    w++;
		}
	    }
	}
    }
    flushAndExit(0);
}

#define IS_WRITTEN (1<<24)	/* indicates that cell has been written to */
#define PACK_RGBF(r,g,b,f) ((r) | ((g) << 8) | ((b) << 16) | (f))
#define UNPACK_R(rgbf) ((rgbf) & 0xff)
#define UNPACK_G(rgbf) (((rgbf) >> 8) & 0xff)
#define UNPACK_B(rgbf) (((rgbf) >> 16) & 0xff)
#define WAS_ALLOCED (1<<25)	/* only used for code in fixup16to31 */
/*
 * Fix up color cells 16-31.
 *
 * NOTE:  The following code is very temporary,
 *        and WILL be changed soon.
 */
fixup16to31()
{
    short r, g, b;
    unsigned int *cells = (unsigned int *) 0;
    Display *dpy;
    Colormap cmap;
    int i, depth, screen;
    unsigned long dummycell, tmp_cells[32];
    
    XColor pix[16];
    int cellFlags[16];
    int nGLcellsWrittenTo = 0;
    int n = 0;	    /* number cells we've alloced (& need to free) */
    /*
     * First find which which of the cells 16-31 in the GL colormap
     * have ever been written to.
     */
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
	    dpy = (Display *) data[_GL_CDATA_Dpy];
	    screen = data[_GL_CDATA_Screen];
	}
    }
    if (!cells || !dpy)
	return;
    depth = DefaultDepth(dpy, screen);
    if (depth == 24)
	return;
    cmap = DefaultColormap(dpy, screen);
    for (i = 0; i < 16; ++i) {
	cellFlags[i] = WAS_ALLOCED;	/* initialize */
	pix[i].pixel = i + 16;		/* for X calls which follow */
	 if (cells[i+16] & IS_WRITTEN) {
	    ++nGLcellsWrittenTo;
	    cellFlags[i] |= IS_WRITTEN;
	 }
    }
    if (nGLcellsWrittenTo == 0) {
	/*
	 * None of 16-31 were written to in GL colormap.
	 * There's nothing to do.
	 */
	return;
    }
    /*
     * Now find which cells 16-31 are allocated
     * in the X default root colormap.
     */
    dummycell = 0;
    while (dummycell < 31) {
	XAllocColorCells(dpy, cmap, True, 0, 0,
			(unsigned long *) &dummycell, 1);
	if (dummycell == 0)
	    break;	    /* alloc failed */
	tmp_cells[n++] = dummycell;	/* keep track of what we've done */
	if (15 < dummycell && dummycell < 32)
	    cellFlags[dummycell - 16] &= ~WAS_ALLOCED;
    }
    if (n)
	XFreeColors(dpy, cmap, tmp_cells, n, 0);
    else
	return;
    XQueryColors(dpy, cmap, &pix[0], 16);
    /*
     * For cells 16-31, if the cell has been written to in
     * in the GL colormap (via mapcolor), and if the corresponding
     * cell in the X root colormap is allocated, then take what's
     * in that X cell and put it into the GL cell.
     */
    for (i = 0; i < 16; ++i) {
	if ((cellFlags[i] & (IS_WRITTEN | WAS_ALLOCED))
			== (IS_WRITTEN | WAS_ALLOCED))
	    mapcolor(i+16, pix[i].red >> 8,
			pix[i].green >> 8, pix[i].blue >> 8);
    }
}
