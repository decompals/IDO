/*
 *    ipastelrw -
 *
 *         a simple ipaste imitation example using lrectwrite to "paste"
 *    the image onto the screen.  the highlight of this program is its 
 *    use of the powerful longimagedata() function, defined in the 
 *    imgread.c module of libgutil.a.
 *
 *                                            ratmandu - 1991
 */
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/image.h>

unsigned long	*imgbuf;
int		xsize, ysize;

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    short val;
    int wx, wy, preforg;
    register int i;
    extern long *longimagedata();


    if( argc<2 ) {
        fprintf(stderr,"usage: %s inimage [-f] [-n] [-o xorg yorg]\n", argv[0]);
        exit(1);
    }

    for(i=1; i<argc; i++) {
        if(strcmp(argv[i],"-f") == 0)
            foreground();
        if(strcmp(argv[i],"-n") == 0)
            noborder();
        if(strcmp(argv[i],"-o") == 0) {
            i++;
            wx = atoi(argv[i]);
            i++;
            wy = atoi(argv[i]);
            preforg = 1;
        }
    }

    if( (image=iopen(argv[1],"r")) == NULL ) {
        fprintf(stderr,"rpaste: can't open input file %s\n",argv[1]);
        exit(1);
    }

/* calculate the window size */
    sizeofimage(argv[1], &xsize, &ysize);

/* allocate the memory for the pixel data to be then fed to lrectwrite */
    imgbuf = (unsigned long *) malloc(xsize*ysize*sizeof(long));
    imgbuf = (unsigned long *) longimagedata(argv[1]);

/* open the window */
    if(preforg) {
        prefposition(wx,wx+xsize-1,wy,wy+ysize-1);
        prefsize(xsize-1,ysize-1);
        winopen(argv[0]);
        wintitle(argv[1]);
    } else {
        prefsize(xsize-1,ysize-1);
        winopen(argv[0]);
        wintitle(argv[1]);
    }


/* set the display mode for the image */
    RGBmode();
    gconfig();

    drawit();
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		drawit();
		break;
 	}
    }
}


drawit()
{
    cpack(0x00808080);
    clear();
    reshapeviewport();
    lrectwrite(0,0,xsize-1,ysize-1,imgbuf);
}
