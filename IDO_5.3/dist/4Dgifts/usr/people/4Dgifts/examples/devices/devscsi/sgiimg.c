#ifdef SGI_IMAGE	/* entire file */
#ident "sgiimg.c: $Revision: 1.1 $"
#include <gl/image.h>

extern int iheight, iwidth, bytes;
extern double zoom;

static unsigned short *abuf[3];
static unsigned short *bbuf[3];
static unsigned short *sbuf;
static unsigned short **xmap[3];

static int y;

static IMAGE *image;
IMAGE *fiopen();

endimg()
{
	int x;

	for(x=0; x<image->zsize; x++) {
		if(abuf[x])
			free(abuf[x]);
		if(bbuf[x])
			free(bbuf[x]);
		if(xmap[x])
			free(xmap[x]);
	}
	if(sbuf)
		free(sbuf);
	if(image)
		iclose(image);
}

/* 'open' the image file and get the header */
get_hdr()
{
	register int x;
	int xsize;

	/* get the header from an sgi image file */
	image = fiopen(0, "r");
	if(image == NULL)
		ds_zot("image is not in standard SGI format");
	iheight = (int)(.5 + zoom * image->ysize);
	iwidth = (int)(.5 + zoom * image->xsize);
	/* width to LW has to be even # of bytes... */
	bytes = 2* ((iwidth + 15 ) / 16);
	iwidth = bytes * 8;	/* make sure we allocate enough buffer space! */

	/* be sure we don't use some data from next array */
	xsize = 16 * ((image->xsize + 15)  / 16);
	for(x=0; x<image->zsize; x++) {
		abuf[x] = (unsigned short *)calloc(xsize, sizeof(short));
		bbuf[x] = (unsigned short *)malloc(iwidth*sizeof(short));
		xmap[x] = (unsigned short **)calloc(iwidth, sizeof(short *));
		if(!abuf[x] || !bbuf[x] || !xmap[x])
			ds_zot("Can't get memory for processing image");
		makexmap(abuf[x],xmap[x],xsize,iwidth);
	}
	sbuf = (unsigned short *)malloc(iwidth*sizeof(short));
	if(sbuf == NULL)
		ds_zot("Can't get memory for processing image");

	y = iheight - 1;
	return 1;
}


/* get the next line from the image, convert it to one bit
per pixel, and put it in ibuf.
*/
get_line(ibuf, cnt)
unsigned char *ibuf;
{
	doline(ibuf, y--);	/* cnt not needed due to earlier calcs */
	return cnt;
}


/*	impulse zoom implementation follows.  This same code is
in izoom and many of the printer filters.  Only difference
is this one just puts data in a buffer instead of calling putrow(),
it does dithering, and conversion on each
output row. Also gets the lines in reverse order, and
assumes x dim always scaled.
Handles the case of the image being zoomed to a larger size also.

NOTE: this does no dithering or filtering between
vertical lines...

For simplicities sake, at the cost of some extra CPU time, this
code is used even when the zoom factor is 1.0.
*/

#define GRIDTOFLOAT(pos,n)	(((pos)+0.5)/(n))
#define FLOATTOGRID(pos,n)	((pos)*(n))

doline(ibuf, y)
int y;
unsigned char *ibuf;
{
	int z, zsize, ay;
	static int curay;	/* will be 0 on subsequent images also,
		since we count down towards 0. */
	float fy;

	if(y < 0)
		ds_zot("tried to read more lines than in image");

	zsize = image->zsize;
	fy = GRIDTOFLOAT(y,iheight);
	ay = FLOATTOGRID(fy, image->ysize);
	if(curay != ay) {
		for(z=0; z<zsize; z++) {
			getrow(image,abuf[z],ay,z);
			xscalebuf(xmap[z],bbuf[z],iwidth);
		}
		curay = ay;
	}
	if(zsize<3)
		bw1dither(bbuf[0],ibuf,iwidth);
	else
		rgbrowtobw1(bbuf[0], bbuf[1],
			bbuf[2],ibuf,iwidth);
}

makexmap(a_buf,x_map,anx,bnx)
unsigned short *a_buf;
unsigned short *x_map[];
int anx, bnx;
{
	int x, ax;
	float fx;

	for(x=0; x<bnx; x++) {
		fx = GRIDTOFLOAT(x,bnx);
		ax = FLOATTOGRID(fx,anx);
		x_map[x] = a_buf+ax;
	}
}

xscalebuf(x_map,b_buf,bnx)
register unsigned short *x_map[];
register unsigned short *b_buf;
register int bnx;
{
	while(bnx--) {
		if(bnx >= 7) {
			bnx -= 7;
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
			*b_buf++ = *(*x_map++);
		}
		else
			*b_buf++ = *(*x_map++);
	}
}


/* 	convert image down to 1 bit per pixel, this code is derived from
	the code Paul did for the -b 1 option of tops.c
	Main difference is that sun rasterfiles are inverse of
	PostScript as.
	Olson, 5/89
*/
bw1dither(iptr,o,n)
register unsigned short *iptr;
register unsigned char *o;
register int n;
{
	unsigned char val;
	register x = 0, i;

	n = (n+7)/8;
	while(n-- > 0) {
		val = 0;
		for(i=0; i<8; i++) {
			val <<= 1;
			val |= (iptr[x] & 0x80) >> 7;
			x++;
		}
		*o++ = 0xff - val;
	}
}

/* 'dither' color image down to 1 bit per pixel, again derived
	from code in tops.c 
*/
static
rgbrowtobw1(r_buf,g_buf,b_buf,o_buf,n)
register unsigned short *r_buf, *g_buf, *b_buf;
unsigned char *o_buf;
register int n;
{
	rgbrowtobw(r_buf,g_buf,b_buf,sbuf,n);
	bw1dither(sbuf,o_buf,n);
}


static
rgbrowtobw(r_buf,g_buf,b_buf,o_buf,n)
register unsigned short *r_buf, *g_buf, *b_buf, *o_buf;
register int n;
{
	while(n--)
		*o_buf++ = (77*(*r_buf++) + 150*(*g_buf++) + 28*(*b_buf++))>>8;
}
#endif SGI_IMAGE
