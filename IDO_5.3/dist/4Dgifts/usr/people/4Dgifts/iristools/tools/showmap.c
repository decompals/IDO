/*
 *	showmap - 
 *		Display the color map.
 *
 *				Paul Haeberli - 1984
 *
 */
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

main(argc, argv)
int argc;
char **argv;
{
    Device dev;
    short val;
    int shrink = 0;
    int fullconfig = 0;
    int menu, planes;
    int size1, size2, cursize;

    if( argc == 2 ) {
	if(strcmp(argv[1],"-s") != 0) {
            fprintf(stderr,"usage: showmap [-s]\n");
            exit(1);
	}
	shrink = 1;
    }

    minsize(165,165);
    keepaspect(1,1);
    winopen("showmap");
    keepaspect(1,1);
    winconstraints();
    qdevice(LEFTMOUSE);
    qdevice(MENUBUTTON);
    menu = defpup("showmap %t|makemap|cedit|interp|exit");
    planes = getplanes();
    if (planes<8)
	size1 = size2 = 8;
    else if (planes==8)
	size1 = size2 = 16;
    else if (planes==10)
	size1 = size2 = 32;
    else {
	size1 = 64;
	size2 = 32;
	fullconfig = 1;
    }
    if (shrink && fullconfig)
        cursize = size2;
    else
        cursize = size1; 
    showmap(cursize);
    while (1) {
	int needs_redraw = 0;
	do {
	    dev = qread(&val);
	    switch(dev) {
	    case REDRAW: 
		reshapeviewport();
		needs_redraw = 1;
		break;
	    case LEFTMOUSE: 
		if(val) {
		    if(cursize==size1)
			 cursize=size2;
		    else
			 cursize=size1;
		    needs_redraw = 1;
		}
		break;
	    case MENUBUTTON: 
		if (val) {
		    switch (dopup(menu)) {
			case 1: 
			    dosystem("makemap");
			    break;
			case 2: 
			    dosystem("cedit");
			    break;
			case 3: 
			    dosystem("interp");
			    break;
			case 4: 
			    exit(0);
		    }
		}
		break;
	    }
	} while (qtest());
	if (needs_redraw)
	    showmap(cursize);
    }
}

showmap(cursize)
int cursize;
{
    int i, j, planes;
    long  xsize, ysize;

    grey(0.9);
    clear();
    planes = getplanes();
    ortho2(-0.25,cursize+0.25,-0.25,cursize+0.25);
    if(cursize == 8) {
        for (j=0; j<8; j++) {
	    for (i=0; i<8; i++) {
	        color((j*8)+i);
	        rectf(i+0.1,j+0.1,i+0.9,j+0.9);
	    }
        }
    } else {
        for (j=0; j<cursize; j++) {
	    for (i=0; i<cursize; i++) {
	        color((j*cursize)+i);
	        rectfi(i,j,i+1,j+1); 
	    }	
	}
        grey(0.5);
	getsize(&xsize,&ysize);
	if((xsize/cursize)>4) {
            for (j=0; j<=cursize; j++) {
	        move2i(0,j);
	        draw2i(cursize,j);
	        move2i(j,0);
	        draw2i(j,cursize);
            }
	}
    }
}
