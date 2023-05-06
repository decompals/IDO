
/* abklib.c       1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This file is a collection of functions used with yuv.c.  */
/* This file is not standalone.                             */
/*                                                          */

#include <fcntl.h>
#include <gl.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <dslib.h>

#define LINES_525

#define BLOCKSIZE 512
#define IMG_PIXELS 720

#ifdef LINES_525
#define SCR_PIXELS 640
#define LINES 486
#define FIELDLEN 0x5D000
#else
#define SCR_PIXELS 768
#define LINES 576
#define FIELDLEN 0x6E000
#endif

char *devpath = "/dev/scsi/sc0d5l0";       /* device for a60 */

long dsblksize;		/* block size for devscsi read/write */

/* the following two routines are extension to dev scssi that allow */
/* reads and writes with a variable blocksize */

/*
|| readextended28 - issue group 1 "Read Extended" command (0x28)
*/

myreadextended28(dsp, data, datalen, lba, vu)
  struct dsreq *dsp;
  caddr_t data;
  long datalen, lba;
  char vu;
{
  fillg1cmd(dsp, CMDBUF(dsp), G1_READ, 0, B4(lba), 0, B2(datalen), B1(vu<<6));
  filldsreq(dsp, data, datalen*dsblksize, DSRQ_READ /* |DSRQ_CTRL2 */ );
  dsp->ds_time = 400;	/* often takes a while */
  return(doscsireq(getfd(dsp), dsp));
}


/*
|| writeextended2a - issue group 1 "Write Extended" command (0x2a)
*/

mywriteextended2a(dsp, data, datalen, lba, vu)
  struct dsreq *dsp;
  caddr_t data;
  long datalen, lba;
  char vu;
{
  fillg1cmd(dsp, CMDBUF(dsp), G1_WRIT, 0, B4(lba), 0, B2(datalen), B1(vu<<6));
  filldsreq(dsp, data, datalen*dsblksize, DSRQ_WRITE /* |DSRQ_CTRL2 */ );
  dsp->ds_time = 400;	/* often takes a while */
  return(doscsireq(getfd(dsp), dsp));
}

/* line interp corrects for aspect ratio differences between the A60
 * and the Iris Frame buffer
 */
lineinterp(to, from) /* quick 'n dirty */
short *from;
long *to;
{
int i, src_pos, inc, odd, tmp;
unsigned long y2, y, u, v;

/* calculate the inc (integer fixed point 1 == 0x10000) */

inc = (IMG_PIXELS << 16) / SCR_PIXELS;
src_pos = 0;
odd = 0;

/* do a pair of pixels each iteration */
for(i=0; i<(SCR_PIXELS>>1); i++)
    {
    /* first luma */
    y = (0xFF & *(from+odd)) * (0x10000 - src_pos)
	+ (0xFF & *(from+odd+1)) * src_pos;
    /*chroma*/
    u = (0xFF & ((*from)>>8)) * (0x10000 - src_pos)
	+ (0xFF & ((*(from+2))>>8)) * src_pos;
    v = (0xFF & ((*(from+1))>>8)) * (0x10000 - src_pos)
	+ (0xFF & ((*(from+3))>>8)) * src_pos;

    /* add the inc */
    if(odd) src_pos += inc + 0x10000;
    else src_pos += inc;
    from += 0xFFFE & (src_pos>>16);
    odd = (0x10000 & src_pos) != 0;
    /* frac part only */
    src_pos &= 0xFFFF;

    /* second luma */
    y2 = (0xFF & *(from+odd)) * (0x10000 - src_pos)
	+ (0xFF & *(from+odd+1)) * src_pos;
    /* add the inc */
    if(odd) src_pos += inc + 0x10000;
    else src_pos += inc;
    from += 0xFFFE & (src_pos>>16);
    odd = (0x10000 & src_pos) != 0;
    /* frac part only */
    src_pos &= 0xFFFF;
    tmp = (y2>>16) | (0xFF0000 & y) | (0xFF000000 & (u<<8)) | (0xFF00 & (v>>8));
    *to++ = tmp;
    }
}

linecpy(to, from) /* simple copy assumes 2 bytes per pixel */
long *from, *to;
{
int i;

for(i=0; i<(IMG_PIXELS>>1); i++)
    *to++ = *from++;
}

/* flip copies from one image to another */
/* performing a top to bottom flip to change coordinate spaces */
/* if the global aspect is true lines are squashed to correct aspect ratio */

flip(aspect, lines, to, from)
int aspect, lines;
short *to, *from;
{
int y, pixels;

if(aspect) pixels = IMG_PIXELS;
else pixels = SCR_PIXELS;
from = from+IMG_PIXELS*(lines-1);
for(y=0; y<lines; y++)
    {
    if(aspect) linecpy(to, from);
    else lineinterp(to, from);
    to+=pixels;
    from-=IMG_PIXELS;
    }
}

/* squish allows YUV images to be displayed on a 12 bit CLUT */

squish(pixels, lines, ptr)
int pixels, lines;
long *ptr;
{
int i; 
register unsigned long tmp, u, v;

/*
 * takes U Y V Y longs and packs them as two YUV shorts
 * with 6 bits Y and 3 bits U and V
 */

for(i=0; i<((pixels*lines)>>1); i++)
    {
    tmp = *ptr;
    u = ((tmp>>10)&0x380000) | ((tmp>>26)&0x38);
    v = ((tmp<<3)&0x70000) | ((tmp>>13)&0x7);
    tmp = ((tmp<<4)&0xFC00FC0) | u | v;
    *ptr++ = tmp;
    }
}

/* RGB matrix converts YUV to RGB */

#define limit(x) { \
    if(x > 0xFFFFFF) x = 0xFFFFFF; \
    if(x <= 0xFFFF) x = 0; \
    x &= 0xFF0000; \
    }

rgbmatrix(pixels, to, from)
int pixels;
long *from, *to;
{
int i, j; 
long tmp, y, u, v, y1, r, g, b;
long *line;

/*
 * takes U Y V Y longs and makes two A B G R longs from each
 */

    for(j=0; j<(pixels>>1); j++)
    {
    tmp = *from++;
    u = (0xFF&(tmp>>24))-128;
    y = (0xFF&(tmp>>16))-16;
    if(y < 0) y = 0;

    v = (0xFF&(tmp>>8))-128;
    y1 = (0xFF&tmp)-16;
    if(y1 < 0) y1 = 0;

    y *= 76310;

    r  =    y +            104635*v;

    g  =    y + -25690*u + -53294*v;

    b  =    y + 132278*u;

    limit(r);
    limit(g);
    limit(b);

    tmp = b | (g>>8) | (r>>16);
    *to++ = tmp;


    y1 *= 76310;

    r  =    y1 +            104635*v;

    g  =    y1 + -25690*u + -53294*v;

    b  =    y1 + 132278*u;

    limit(r);
    limit(g);
    limit(b);

    tmp = b | (g>>8) | (r>>16);
    *to++ = tmp;
    }
}

extern int dsreqflags;
extern int dsdebug;
extern int dsblksize;

writefield(fieldnum, field)
int fieldnum;
char *field;
{
int i, chunk, block, res;
struct dsreq *dsp;
char *ptr;

dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 0;
dsblksize = BLOCKSIZE;

if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
	{
	fprintf(stderr, "unable to open %s\n", devpath);
	exit(1);
	}

/* clear check cond.    */
if (testunitready00(dsp) != 0)
    {
    fprintf(stderr, "device not ready\n");
    exit(1);
    }

block = fieldnum * (FIELDLEN/BLOCKSIZE);
chunk = (FIELDLEN/BLOCKSIZE)/8;
ptr = field;
for(i=0;i<8;i++,ptr+=(chunk*BLOCKSIZE),block+=chunk)
    if(res = writeextended2a(dsp, ptr, chunk, block, 0)) break;

if(res)
    {
    fprintf(stderr, "error from write %d\n", i);
    if (testunitready00(dsp) != 0)
	fprintf(stderr, "device not ready\n");
    exit(1);
    }
}

readfield(fieldnum, field)
int fieldnum;
char *field;
{
int i, chunk, block, res;
struct dsreq *dsp;
char *ptr;

dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 0;
dsblksize = BLOCKSIZE;

if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
	{
	fprintf(stderr, "unable to open %s\n", devpath);
	exit(1);
	}

/* clear check cond.    */
if (testunitready00(dsp) != 0)
    {
    fprintf(stderr, "device not ready\n");
    exit(1);
    }

block = fieldnum * (FIELDLEN/BLOCKSIZE);
chunk = (FIELDLEN/BLOCKSIZE)/8;
ptr = field;
for(i=0;i<8;i++,ptr+=(chunk*BLOCKSIZE),block+=chunk)
    if(res = readextended28(dsp, ptr, chunk, block, 0)) break;

if(res)
    {
    fprintf(stderr, "error from read %d\n", i);
    if (testunitready00(dsp) != 0)
	fprintf(stderr, "device not ready\n");
    exit(1);
    }
}

rgbtoyuv(numpix, rgbptr, yuvptr) 
char *rgbptr, *yuvptr;
{ 
int r, g, b;
int pixel; 
FILE *rgbfile, *yuvfile; 
long y1, y2, u, v, u0, u1, u2, v0, v1, v2;

/* deal with an U Y V Y sequence each time round */ 

for(pixel = numpix / 2; pixel>0; pixel--) 
    { 
    /* first pixel gives Y and 0.5 of chroma */ 

    rgbptr++;		/* skip the alpha */
    b = *rgbptr++;
    g = *rgbptr++;
    r = *rgbptr++;

    y1  =    16829*r +  33039*g +  6416*b + (0xFFFF & y2);

    u1  =    -4853*r +  -9530*g + 14383*b;

    v1  =    14386*r + -12046*g + -2340*b;


    /* second pixel just yields a Y and 0.25 U, 0.25 V */ 

    rgbptr++;		/* skip the alpha */
    b = *rgbptr++;
    g = *rgbptr++;
    r = *rgbptr++;

    y2  =    16829*r +  33039*g +  6416*b + (0xFFFF & y1);

    u2  =    -2426*r +  -4765*g + 7191*b;

    v2  =    7193*r + -6023*g + -1170*b;

    /* Filter the chroma */

    u = u0 + u1 + u2 + (0xFFFF & u);
    v = v0 + v1 + v2 + (0xFFFF & v);

    u0 = u2;
    v0 = v2;

    *yuvptr++ = (u>>16) +128;
    *yuvptr++ = (y1>>16) + 16;
    *yuvptr++ = (v>>16) +128;
    *yuvptr++ = (y2>>16) + 16;
    } 
} 
