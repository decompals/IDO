/*
 *	gamcal -
 *		See if the gamma correction value is correct by comparing
 *	the intensity of full on, full off horizontal lines to a region of
 *	50 percent grey.
 *
 *				Paul Haeberli - 1985
 *
 */
#include "gl.h"
#include "device.h"

unsigned short tex[] = {
    0xffff, 0x0000, 0xffff, 0x0000,
    0xffff, 0x0000, 0xffff, 0x0000,
    0xffff, 0x0000, 0xffff, 0x0000,
    0xffff, 0x0000, 0xffff, 0x0000,
};

main(argc,argv)
int argc;
char **argv;
{
    short val;

    stepunit(1,16);
    winopen("gamcal");
    pseudorgb();
    defpattern(2,16,tex);
    makeit();
    while(1) {
	switch(qread(&val)) {
	   case REDRAW:
		makeit();
		break;
	}
    }
}

makeit()
{
    int i, y;
    int intens;

    reshapeviewport();
    ortho2(0.0,9.0,0.0,8.0);
    greyi(0);
    clear();
    pushmatrix();
    for(y=0; y<4; y++) {
	setpattern(2);
	intens = 256;
	for(i=0; i<8; i++)  {
	    if(intens == 256)
		greyi(252);
	    else
		greyi(intens);
	    rectf((float)i,1.0,i+1.0,2.0);
	    intens >>= 1;
	}
	setpattern(0);
	intens = 256;
	for(i=0; i<8; i++)  {
	    if(intens == 256)
		greyi(252/2);
	    else
		greyi(intens/2);
	    rectf((float)i,0.0,i+1.0,1.0);
	    intens >>= 1;
	}
	translate(0.0,2.0,0.0);
    }
    popmatrix();
}
