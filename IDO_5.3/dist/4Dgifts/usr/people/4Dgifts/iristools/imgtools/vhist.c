/*
 *	vhist - 
 *		Draw a 3D histogram of pixel values in an image file.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "port.h"
#include "image.h"
#include "gl.h"
#include "device.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];
long xsize, ysize;
long xorg, yorg;

typedef struct vhistogram {
    int *bucket;
    int nbuckets;
    int dim;
    int xmin, xmax;
    int ymin, ymax;
    int zmin, zmax;
} vhistogram;

vhistogram *newvhist();

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    register unsigned int i, index;
    short val;
    int x, y, z;
    int pixdone, totpixels;
    vhistogram *vhist;

    if( argc<2 ) {
	fprintf(stderr,"usage: vhist inimage\n");
	exit(1);
    } 
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"vhist: can't open input file %s\n",argv[1]);
	exit(0);
    }
    if(image->max>255) {
	fprintf(stderr,"vhist: max can't exceed 255\n");
	exit(0);
    }
    keepaspect(1,1);
    winopen("vhist");
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    perspective(450,1.0,0.001,10.0);
    pseudorgb();
    doublebuffer();
    gconfig();
    vhist = newvhist(3,16);
    makeframe();
    totpixels = image->zsize*image->ysize*image->xsize;
    pixdone =0;
    showvhist(vhist);
    for(y=0; y<image->ysize; y++) {
	getrow(image,rbuf,y,0);
	getrow(image,gbuf,y,1);
	getrow(image,bbuf,y,2);
	addtovhist(vhist,image->xsize,rbuf,gbuf,bbuf);
	percentdone((100.0*pixdone)/totpixels);
	pixdone += image->xsize;
	checkredraw();
	if((y%50) == 0)
	    showvhist(vhist);
    }
    percentdone(100.0);
    showvhist(vhist);
    qdevice(LEFTMOUSE);
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		makeframe();
		showvhist(vhist);
		break;
	    case LEFTMOUSE:
		if(val) {
		    trackclick();
		    while(getbutton(LEFTMOUSE)) {
			trackpoll();
		        showvhist(vhist);
		    }
		}
		break;
	}
    }
}

checkredraw()
{
    short val;

    while(qtest()) {
	if(qread(&val) == REDRAW)
	    makeframe();
    }
}

makeframe()
{
    getorigin(&xorg,&yorg);
    getsize(&xsize,&ysize);
    reshapeviewport();
}

/*
 *	vhist - 
 *		Support for histogram creation and display.
 *
 *				Paul Haeberli - 1988
 */
vhistogram *newvhist(dim,nbuckets)
int dim, nbuckets;
{
    vhistogram *vhist;
    int i, *ptr;
    int size;
    
    vhist = (vhistogram *)malloc(sizeof(vhistogram));
    vhist->nbuckets = nbuckets;
    vhist->dim = dim;
    size = 1;
    for(i=0; i<dim; i++)
	size = size*nbuckets;
    vhist->bucket = (int *)malloc(size*sizeof(int));
    clearvhist(vhist);
    return vhist;
}

freevhist(vhist)
vhistogram *vhist;
{
    if(vhist) {
        free(vhist->bucket);
        free(vhist);
    }
}

clearvhist(vhist)
register vhistogram *vhist;
{
    register int i, nbuckets, *ptr, size;

    ptr = vhist->bucket;
    nbuckets = vhist->nbuckets;
    size = 1;
    for(i=0; i<vhist->dim; i++)
	size = size*nbuckets;
    for (i=0; i<size; i++)
	*ptr++ = 0;
}
 
addtovhist(vhist,n,sptr0,sptr1,sptr2)
vhistogram *vhist;
register int n;
register unsigned short *sptr0, *sptr1, *sptr2;
{
    register unsigned int index, nbuckets, ymult, zmult;
    register int *bucket, i;

    nbuckets = vhist->nbuckets;
    bucket = vhist->bucket;
    switch(vhist->dim) {
	case 1:
    	    while (n--) {
		index = (*sptr0++ * nbuckets)/255;
		if(index>=nbuckets) 
		    index = nbuckets-1;
		bucket[index]++;
	    }
	    break;
	case 2:
	    ymult = vhist->nbuckets;
    	    while (n--) {
		i = (*sptr0++ * nbuckets)/255;
		if(i>=nbuckets) 
		    i = nbuckets-1;
		index = i;
		i = (*sptr1++ * nbuckets)/255;
		if(i>=nbuckets) 
		    i = nbuckets-1;
		index += (i*ymult); 
		bucket[index]++;
	    }
	    break;
	case 3:
	    ymult = vhist->nbuckets;
	    zmult = ymult*ymult;
    	    while (n--) {
		i = (*sptr0++ * nbuckets)/255;
		if(i>=nbuckets) 
		    i = nbuckets-1;
		index = i;
		i = (*sptr1++ * nbuckets)/255;
		if(i>=nbuckets) 
		    i = nbuckets-1;
		index += (i*ymult); 
		i = (*sptr2++ * nbuckets)/255;
		if(i>=nbuckets) 
		    i = nbuckets-1;
		index += (i*zmult); 
		bucket[index]++;
	    }
	    break;
    }
}

showvhist(vhist)
register vhistogram *vhist;
{
    int x, y, z, nbuckets;
    float shift;
    register int *ptr;
    int max;
    float val;

    pushmatrix();
    translate(0.0,0.0,-5.0);
    tracktransform();
    nbuckets = vhist->nbuckets;

    rgbi(0,0,0);
    clear();

    pushmatrix();
    shift = -(nbuckets-1.0)/2.0;
    rgbi(128,128,128);
    pushmatrix();
    scale(2.2,2.2,2.2);
    drawcube();
    popmatrix();
    rgbi(255,255,255);
    scale(1.0/shift,1.0/shift,1.0/shift);
    translate(shift,shift,shift);
    max = 0;
    ptr = vhist->bucket;
    *ptr = 0;
    for(z=0; z<nbuckets; z++) {
        for(y=0; y<nbuckets; y++) {
            for(x=0; x<nbuckets; x++) {
		if(*ptr > max)
		    max = *ptr;
		ptr++;
	    }
        }
    }
    ptr = vhist->bucket;
    *ptr = 0;
    for(z=0; z<2; z++) {
        for(y=0; y<2; y++) {
            for(x=0; x<2; x++) {
		pushmatrix();
		rgbi((255*x),(255*y),(255*z));
		translate(x*(nbuckets-1),y*(nbuckets-1),z*(nbuckets-1));
		scale(0.2,0.2,0.2);
		drawcube();
		popmatrix();
	    }
	}
    }
    for(z=0; z<nbuckets; z++) {
        for(y=0; y<nbuckets; y++) {
            for(x=0; x<nbuckets; x++) {
	 	if(*ptr > 0) {
		    val = *ptr;
		    val = val/max;
		    rgbi(50+(205*x)/nbuckets,
			 50+(205*y)/nbuckets,
			 50+(205*z)/nbuckets);
	     	    drawtet((float)x,(float)y,(float)z,val);
		}
		ptr++;
	    }
        }
    }
    popmatrix();
    swapbuffers();
    popmatrix();
}

vhisteqtable(vhist,tab)
vhistogram *vhist;
short *tab;
{
    int i, sum, shade, nbuckets; 
    register int *bucket; 
    float maxshade;

    nbuckets = vhist->nbuckets;
    bucket = vhist->bucket;
    sum = 0;
    for(i=0; i<nbuckets; i++)
	sum += bucket[i];
    if(sum == 0) 
	sum = 1;
    maxshade = 255.0;
    shade = 0;
    for(i=0; i<nbuckets; i++) {
	tab[i] = (shade*255.0)/sum;
	shade += bucket[i];
    }
}

drawcube()
{
    int i;

    pushmatrix();
    for (i=0; i<4; i++) {
	rot(90.0,'y');
	pushmatrix();
	    translate(0.0,0.0,0.5);
	    rect(-0.5,-0.5,0.5,0.5);
	popmatrix();
    }
    popmatrix();
}

drawtet(x,y,z,size)
register float x, y, z, size;
{
    float s;

    s = size/2.0;
    x += s;
    y += s;
    z += s;
    move(x,y,z);
    draw(x-size,y-size,z);
    draw(x-size,y,z-size);
    move(x,y,z);
    draw(x-size,y,z-size);
    draw(x,y-size,z-size);
    move(x,y,z);
    draw(x,y-size,z-size);
    draw(x-size,y-size,z);
}
