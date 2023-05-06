/*
 * Use video card as a 24 bit RGB framebuffer. ipaste SGI image file to screen,
 * vpaste image file to video
 */

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>
#include <gl/image.h>


static void imgerror(char *);
static long getvideoparam(SVhandle, long);
static void sgiimage_to_buf(IMAGE *, unsigned long *,
			unsigned long, unsigned long);

int main(int argc, char *argv[])
{
    unsigned long   *rgb_buf, x_size, y_size;
    IMAGE*  ip;
    SVhandle V;
    int pal_mode = 0;
    char line[30];

    if (argc != 2) {
	fprintf(stderr, "Usage: %s imagefile\n", argv[0]);
	exit(1);
    }
    i_seterror(imgerror);

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Size image according to broadcast standard */
    pal_mode = (getvideoparam(V, SV_BROADCAST) == SV_PAL);
    if (pal_mode) {
	x_size = SV_PAL_XMAX;
	y_size = SV_PAL_YMAX;
    } else {
	x_size = SV_NTSC_XMAX;
	y_size = SV_NTSC_YMAX;
    }

    /* Open image */
    if ((ip = iopen(argv[1], "r")) <= (IMAGE*)0) {
	fprintf(stderr, "could not open image file %s\n", argv[1]);
	exit(1);
    }

    rgb_buf = (unsigned long *)malloc(x_size * y_size * sizeof(long));

    sgiimage_to_buf(ip, rgb_buf, x_size, y_size);   /* convert to RGB buffer */

    iclose(ip);

    /* Output 24-bit RGB image */
    if (svPutFrame(V, (char *)rgb_buf) < 0) {
	svPerror("putframe");
	svCloseVideo(V);
	exit(1) ;
    }
    printf("Type <enter> to exit:");
    (void) gets(line);
    exit(0);
}

static void
imgerror(char *s)
{
    fputs(s, stderr);
}

/* Center SGI image file in buffer */
static void
sgiimage_to_buf(IMAGE *ip, unsigned long *rgb_buf,
		unsigned long bxsize, unsigned long bysize)
{
    short   *red, *green, *blue, *r, *g, *b;
    int	    bxstart, ixstart, bystart, iystart;
    int	    iy, by, x, nx, ny;
    unsigned long   *rgb;

    red   = malloc(ip->xsize * sizeof(short));
    green = malloc(ip->xsize * sizeof(short));
    blue  = malloc(ip->xsize * sizeof(short));

    bzero(rgb_buf, bxsize*bysize*sizeof(long));

    if (ip->xsize > bxsize) {
	bxstart = 0;
	ixstart = (ip->xsize - bxsize)/2;
	nx = bxsize;
    } else {
	ixstart = 0;
	bxstart = (bxsize - ip->xsize)/2;
	nx = ip->xsize;
    }
    if (ip->ysize > bysize) {
	bystart = 0;
	iystart = (ip->ysize - bysize)/2;
	ny = bysize;
    } else {
	iystart = 0;
	bystart = (bysize - ip->ysize)/2;
	ny = ip->ysize;
    }

    for (iy = iystart, by = bystart; iy < iystart + ny; iy++, by++) {
	getrow(ip, red, iy, 0);
	getrow(ip, green, iy, 1);
	getrow(ip, blue, iy, 2);

	rgb = &rgb_buf[(by*bxsize) + bxstart];
	r = &red[ixstart];
	g = &green[ixstart];
	b = &blue[ixstart];
	for (x = 0; x < nx; x++) {
	    *rgb++ = (*b++ << 16) | (*g++ << 8) | *r++;
	}
    }
    free(red);
    free(green);
    free(blue);
}

static long
getvideoparam(SVhandle V, long arg)
{
    long pvbuf[2];

    pvbuf[0] = arg;
    if (svGetParam(V, pvbuf, 2) < 0)
	svPerror("svGetParam");
    return pvbuf[1];
}
