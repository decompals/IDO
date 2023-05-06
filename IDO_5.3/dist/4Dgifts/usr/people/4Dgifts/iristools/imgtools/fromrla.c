/*
 *	fromrla - 
 *		Convert Wavefront .rla RGB image files to Silicon Graphics 
 * 	format RGB image files. 
 *
 *		fromrla inimage.rla outimage.rgb
 *
 * 				Paul Haeberli - 1988
 */
#include "stdio.h"
#include "image.h"

typedef struct {
    short left, right, bottom, top;
} WINDOW_S;

typedef struct {
    WINDOW_S 	window;
    WINDOW_S 	active_window;
    short frame;
    short storage_type;
    short num_chan;
    short num_matte;
    short num_aux;
    short aux_mask;
} RLA_HEADER;

RLA_HEADER header;

char buf[8192];
short sbuf[8192];

main(argc,argv)
int argc;
char *argv[];
{
    FILE *inf;
    IMAGE *oimage;
    int xsize, ysize;
    int y, offset;
    short len;

    if(argc<3) {
	fprintf(stderr,"usage: fromrla inimage.rla outimage.rgb\n");
	exit(1);
    }
    inf = fopen(argv[1],"r");
    if(!inf) {
	fprintf(stderr,"fromrla: can't open input file\n");
	exit(1);
    }
    if(fread(&header,1,sizeof(header),inf) != sizeof(header)) {
	fprintf(stderr,"fromrla: error on read of headert\n");
	exit(1);
    }
    if(header.num_chan != 3) {
	fprintf(stderr,"fromrla: number of channels must be 3\n");
	exit(1);
    }
    if(header.storage_type!=0) {
	fprintf(stderr,"fromrla: storage type must be 0\n");
	exit(1);
    }

    xsize = header.active_window.right-header.active_window.left+1;
    ysize = header.active_window.top-header.active_window.bottom+1;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
        if(fseek(inf,740+4*y,0))
	    seekerror();
        fread(&offset,sizeof(long),1,inf);
        if(fseek(inf,offset,0))
	    seekerror();

        fread(&len,sizeof(short),1,inf);
	fread(buf,len,1,inf);
	decode(buf,sbuf,len);
	putrow(oimage,sbuf,y,0);

        fread(&len,sizeof(short),1,inf);
	fread(buf,len,1,inf);
	decode(buf,sbuf,len);
	putrow(oimage,sbuf,y,1);

        fread(&len,sizeof(short),1,inf);
	fread(buf,len,1,inf);
	decode(buf,sbuf,len);
	putrow(oimage,sbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

decode(c_in,c_out,ct)
register signed char *c_in;
register short *c_out;
register int ct;
{
    register int count;
    register short val;

    while(ct>0) {
	if(*c_in < 0) {
	    count = - *c_in++;
	    ct -= count+1;
	    while(count--)
		*c_out++ = (*c_in++)&0xff;
	} else {
	    count = *c_in++ + 1;
	    ct -= 2;
	    val = (*c_in++)&0xff;
	    while(count--)
		*c_out++ = val;
	}
    }
}

seekerror()
{
    fprintf(stderr,"fromrla: seek error\n");
    exit(1);
}
