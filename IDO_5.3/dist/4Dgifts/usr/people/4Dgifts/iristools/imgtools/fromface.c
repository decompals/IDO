/*
 *	fromface -
 *		Convert a faceserver image into IRIS image file format
 *
 *		    Hacked from Jef Poskanzer's program fstopgm.
 *
 *				Paul Haeberli - 1990
 */
#include "image.h"

#define STRSIZE 1000
short sbuf[4096];

int gethexit();

main(argc,argv)
int argc;
char *argv[];
{
    FILE *inf;
    IMAGE *image;
    int x, y;
    int ysize, xsize, depth, pxsize, pysize, xdepth;
    char buf[STRSIZE], firstname[STRSIZE], lastname[STRSIZE], email[STRSIZE];
    int pix, pid;
    float xscale, yscale;

    if(argc<3) {
	fprintf(stderr,"fromface in.face out.bw\n");
	exit(1);
    }
    ysize = 0;
    xsize = 0;
    depth = 0;
    pxsize = 0;
    pysize = 0; 
    xdepth = 0;
    xscale = -1.0;
    yscale = -1.0;
    inf = fopen(argv[1],"r");
    while(1) {
	if(fgets(buf,STRSIZE,inf) == 0) {
	    fprintf(stderr,"fromface: read error\n");
	    exit(1);
	}
	if(strlen(buf) == 1)
	    break;
	if(sscanf(buf,"FirstName: %s\n",firstname)==1)
	    ;
	else if(sscanf(buf,"LastName: %s\n",lastname)==1)
	    ;
	else if(sscanf(buf,"E-mail: %s\n",email)==1)
	    ;
	else if(sscanf(buf,"PicData: %d %d %d\n",
			  &xsize,&ysize,&depth)==3) {
	    if(depth!=8) {
		fprintf(stderr,"fromface: depth must be 8\n");
		exit(1);
	    }	
	} else if(sscanf(buf,"Image: %d %d %d\n",
					     &pysize,&pxsize,&xdepth) == 3) {
	    if(xdepth != 8) {
		fprintf(stderr,"fromface: depth must be 8\n");
		exit(1);
	    }
	}
    }
    if(xsize<=0 || ysize<=0) {
	fprintf(stderr,"fromface: bad header\n");
	exit(1);
    }
    if(pysize!=0 && pxsize!=0 && (pysize!=xsize || pxsize!=ysize)) {
	float rowratio, colratio;
	rowratio = pxsize/(float)ysize;
	colratio = pysize/(float)xsize;
	if(rowratio>colratio) {
	    xscale = 1.0;
	    yscale = rowratio/colratio;
	} else {
	    xscale = colratio/rowratio;
	    yscale = 1.0;
	}
	fprintf(stderr, "Warning: non-square pixels\n");
    }

/* Now read the hex bits. */
    image = iopen(argv[2],"w",RLE(1),2,xsize,ysize,1);
    for (y=0; y<ysize; y++) {
	for (x=0; x<xsize; x++) {
	    pix =  gethexit( inf ) << 4;
	    pix += gethexit( inf );
	    sbuf[x] = pix;
	}
	putrow(image,sbuf,y);
    }
    iclose(image);
    pid = getpid();
    if(xscale>0.0) {
	sprintf(buf,"izoom %s /usr/tmp/face%d.bw %f %f",
				   argv[2],pid,xscale,yscale);
	system(buf);
	sprintf(buf,"cp /usr/tmp/face%d.bw %s",pid,argv[2]);
	system(buf);
	sprintf(buf,"rm /usr/tmp/face%d.bw",pid);
	system(buf);
    }
    exit(0);
}

int gethexit(inf)
FILE *inf;
{
    register int i;
    register char c;

    while(1) {
	i = getc(inf);
	if (i == EOF) {
	    fprintf(stderr,"fromface: premature EOF\n");
	    exit(1);
	}
	c = i;
	if(c>='0' && c<='9' )
	    return c-'0';
	else if(c>='A' && c<='F')
	    return c-'A'+10;
	else if (c>='a' && c<='f')
	    return c-'a'+10;
    }
}
