/*
 *	toalias -
 *		Convert an Iris image to an Alias image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

char rbuf[8192];
char gbuf[8192];
char bbuf[8192];
unsigned long lbuf[8192];

typedef struct hdr {
     short xsize;
     short ysize;
     short xorg;
     short yorg;
     short bitsdeep;
} hdr;

hdr ihdr;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    int xsize, ysize;
    int y, z;
    char dname[128];
    FILE *outfile;

    if(argc<3) {
	fprintf(stderr,"usage: toalias inimage.rgb aliasimage\n");
	exit(1);
    }
    if ((image = iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"toalias: can't open %s\n",argv[1]);
	exit(1);
    }
    if ((outfile = fopen(argv[2],"w")) == NULL ) {
	fprintf(stderr,"toalias: can't open %s\n",argv[2]);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    ihdr.xsize = xsize;
    ihdr.ysize = ysize;
    ihdr.xorg = 0;
    ihdr.yorg = ysize-1;
    ihdr.bitsdeep = 24;
    fwrite(&ihdr,sizeof(ihdr),1,outfile);
    for(y=0; y<ysize; y++) {
	getrow(image,rbuf,ysize-1-y,0);
	getrow(image,gbuf,ysize-1-y,1);
	getrow(image,bbuf,ysize-1-y,2);
	putalias(outfile,rbuf,gbuf,bbuf,xsize);
    }
    fclose(outfile);
    exit(0);
}

putalias(outfile,rbuf,gbuf,bbuf,xsize)
FILE *outfile;
register unsigned short *rbuf, *gbuf, *bbuf;
int xsize;
{
    register int n, count;
    unsigned long val;

    for(n=0; n<xsize; n++) {
	val = *bbuf++;
	val = (val<<8)+ *gbuf++;
	val = (val<<8)+ *rbuf++;
	lbuf[n] = val;
    }
    val = lbuf[0];
    count = 0;
    for(n=0; n<xsize; n++) {
	if(lbuf[n] == val) {
	    count++;
	    if(count==255) {
		val |= count<<24;
		fwrite(&val,sizeof(long),1,outfile);
		val &= 0xffffff;
		count = 0;
	    }
	} else {
	    if(count) {
		val |= count<<24;
		fwrite(&val,sizeof(long),1,outfile);
	    }
	    val = lbuf[n];
	    count = 1;
	}
    }
    if(count) {
	val |= count<<24;
	fwrite(&val,sizeof(long),1,outfile);
    }
}
