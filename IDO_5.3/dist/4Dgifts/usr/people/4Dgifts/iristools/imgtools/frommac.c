/*
 *	frommac -
 *		Convert a macpaint image into a IRIS image.
 *
 *				Paul Haeberli - 1989
 */
#include "image.h"

#define MAXXSIZE	576
#define MAXYSIZE	720

short sbuf[4096];
unsigned char cbuf[72+256];

main(argc,argv) 
int argc;
char *argv[];
{ 
    if(argc<3) {
	fprintf(stderr,"usage: frommac image.mac image.bw\n");
	exit(1);
    }
    if(readmac(argv[1],argv[2],512)) 
	exit(0);
    else if(readmac(argv[1],argv[2],512+128))
	exit(0);
    else {
	fprintf(stderr,"frommac: bad macpaint file %s\n",argv[1]);
	exit(1);
    }
}

readmac(iname,oname,offset)
char *iname, *oname;
int offset;
{
    FILE *inf;
    IMAGE *oimage;
    int i, y;

    inf = fopen(iname,"r");
    if(!inf) {
	fprintf(stderr,"frommac: can't open input file %s\n",iname);
	exit(1);
    }
    oimage = iopen(oname,"w",RLE(1),2,MAXXSIZE,MAXYSIZE,1);
    for (i=0; i<offset; i++)
	getc(inf);
    for(y=0; y<MAXYSIZE; y++) {
	if(!readline(inf)) {
	    iclose(oimage);
	    fclose(inf);
	    return 0;
	    break;
	}
	bitstorow(cbuf,sbuf,MAXXSIZE);
	putrow(oimage,sbuf,MAXYSIZE-1-y,0);
    }
    iclose(oimage);
    fclose(inf);
    fprintf(stderr,"head size was %d\n",offset);
    return 1;
}

readline(inf)
FILE *inf;
{
    int pos, cnt, val;

    pos = 0;
    while(pos < 72) {
	cnt = getc(inf);
	if((cnt&0x80)==0) {
	    cnt++;			
	    while(cnt--)
		cbuf[pos++] = getc(inf);
	} else {	
	    cnt = 257-cnt;
	    val = getc(inf); 	
	    while (cnt--)
		cbuf[pos++] = val;
	}
    }
    if(pos==72) 
	return 1;
    else
	return 0;
}
