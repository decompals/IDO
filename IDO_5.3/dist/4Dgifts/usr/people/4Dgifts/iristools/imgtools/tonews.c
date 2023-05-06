/*
 *	tonews - 
 *		Convert an iris image into news (SUN resterfile) format.
 *
 *			    Paul Haeberli - 1987
 */
#define NOGREYPATTERNS
#include "image.h"
#include "colortbl.inc"
#include "rasterfile.h"
#include "lum.h"

short r[8192];
short g[8192];
short b[8192];
short sbuf[8192];
char outbuf[8192];
struct rasterfile hdr;
IMAGE *iimage, *oimage;
FILE *ofile;
int rowbytes;
int testmode;

colordist(i,r,g,b)
int i, r, g, b;
{
    int dr, dg, db;
    int rm, gm, bm;

    rm = red_map[i];
    gm = blue_map[i];
    bm = green_map[i];
    dr = r-rm;
    if(dr<0)
       dr = -dr;
    dg = g-gm;
    if(dg<0)
       dg = -dg;
    db = b-bm;
    if(db<0)
       db = -db;
    return dr+dg+db;
}

rgbfindindex(or,og,ob)
int or,og,ob;
{
    int bwindex, rgbindex;
    int bwdist, rgbdist;
    int r, g, b;

    r = or;
    g = og;
    b = ob;
    if(r>255) r = 255;
    if(g>255) g = 255;
    if(b>255) b = 255;
    if(r<0) r = 0;
    if(g<0) g = 0;
    if(b<0) b = 0;
#ifdef FANCY
    bwindex = grey_inverse[ILUM(r,g,b)];
    rgbindex = red_inverse[r]+green_inverse[g]+blue_inverse[b];
    bwdist = colordist(bwindex,or,og,ob);
    rgbdist = colordist(rgbindex,or,og,ob);
    if(bwdist<rgbdist)
	return bwindex;
    else
	return rgbindex;
#else
    return red_inverse[r]+green_inverse[g]+blue_inverse[b];
#endif
}

bwfindindex(b)
int b;
{
    if(b>255) 
	b = 255;
    if(b<0) 
        b = 0;
    return grey_inverse[b];
}

main(argc,argv)
int argc;
char **argv;
{
    register int y, z;
    int xsize, ysize, zsize;

    if( argc<3 ) {
	fprintf(stderr,"usage: tonews irisimage [outfile.im8] [outfile.di -t]\n");
	exit(1);
    } 
    if(argc >= 4) {
	testmode = 1;
    } else
	testmode = 0;
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"rle: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    if(xsize&1)
	rowbytes = xsize+1; 
    else
	rowbytes = xsize; 
    if(testmode) {
	oimage = iopen(argv[2],"w",RLE(1),2,xsize,ysize,1);
	oimage->colormap = CM_SCREEN;
    } else {
	hdr.ras_magic = RAS_MAGIC;
	hdr.ras_width = xsize;
	hdr.ras_height = ysize;
	hdr.ras_depth = 8;
	hdr.ras_length = ysize*rowbytes;
	hdr.ras_type = 1;
	hdr.ras_maptype = RMT_EQUAL_RGB;
	hdr.ras_maplength = 3*256;
	ofile = fopen(argv[2],"w");
	fwrite(&hdr,sizeof(hdr),1,ofile);
	fwrite(red_map,256,1,ofile);
	fwrite(green_map,256,1,ofile);
	fwrite(blue_map,256,1,ofile);
    }
    for(y=0; y<ysize; y++) {
	if(zsize<3) {
	    getrow(iimage,r,(ysize-1)-y,0);
	    normbuf(r,xsize,iimage->max);
	    bwdither(r,outbuf,xsize);
	    outrow(outbuf,rowbytes,y);
	} else {
	    getrow(iimage,r,(ysize-1)-y,0);
	    normbuf(r,xsize,iimage->max);
	    getrow(iimage,g,(ysize-1)-y,1);
	    normbuf(g,xsize,iimage->max);
	    getrow(iimage,b,(ysize-1)-y,2);
	    normbuf(b,xsize,iimage->max);
	    rgbdither(r,g,b,outbuf,xsize);
	    outrow(outbuf,rowbytes,y);
	}
    }
    if(testmode) 
	iclose(oimage);
    else
	fclose(ofile);
    exit(0);
}

int rerr[8192];
int gerr[8192];
int berr[8192];

bwdither(iptr,o,n)
register short *iptr;
register unsigned char *o;
register int n;
{
    register short i;
    register int want, error;
    int rerror, rnext;
    int index, temp;

    rerror = 0;
    rnext = rerr[0] = 0;
    for(i=0; i<n; i++ ) {
	    want = *iptr++ + rerror + rnext;
	    *o++ = index = bwfindindex(want);

	    error =  want-red_map[index];
	    rerror = (3*error)/8;
	    rerr[i] += rerror;
	    rnext = rerr[i+1];
	    rerr[i+1] = error-2*rerror;
    }
}

rgbdither(r,g,b,o,n)
register short *r, *g, *b;
register unsigned char *o;
register int n;
{
    register short i;
    register int rwant, error;
    register int gwant;
    register int bwant;
    int rerror, rnext;
    int gerror, gnext;
    int berror, bnext;
    int index, temp;

    rerror = 0;
    rnext = rerr[0] = 0;
    gerror = 0;
    gnext = gerr[0] = 0;
    berror = 0;
    bnext = berr[0] = 0;
    for(i=0; i<n; i++ ) {
	    rwant = *r++ + rerror + rnext;
	    gwant = *g++ + gerror + gnext;
	    bwant = *b++ + berror + bnext;

	    *o++ = index = rgbfindindex(rwant,gwant,bwant);

	    error =  rwant-red_map[index];
	    rerror = (3*error)/8;
	    rerr[i] += rerror;
	    rnext = rerr[i+1];
	    rerr[i+1] = error-2*rerror;

	    error =  gwant-green_map[index];
	    gerror = (3*error)/8;
	    gerr[i] += gerror;
	    gnext = gerr[i+1];
	    gerr[i+1] = error-2*gerror;

	    error =  bwant-blue_map[index];
	    berror = (3*error)/8;
	    berr[i] += berror;
	    bnext = berr[i+1];
	    berr[i+1] = error-2*berror;
    }
}

normbuf(buf,n,max)
register short *buf;
register int n;
register int max;
{
    if(max == 255)
       return;
    while(n--) {
	if(n>8) {
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    *buf = (*buf*255)/max;
	    buf++;
	    n -= 7;
	} else {
	    *buf = (*buf*255)/max;
	    buf++;
	}
    }
}

outrow(cbuf,n,y)
char *cbuf;
int n, y;
{
    if(testmode) {
	ctos(cbuf,sbuf,n);
	putrow(oimage,sbuf,(oimage->ysize-1)-y);
    } else
	fwrite(cbuf,n,1,ofile);
}
