/*
 *	gamma - 
 *		Some support for gamma correction when reading and writing
 *  	color map entries.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "math.h"
#include "port.h"
#include "gl.h"
#include "stdio.h"

FILE *configopen();

float gammacorrect();
float ungammacorrect();

static unsigned char rgamtable[256];
static unsigned char ggamtable[256];
static unsigned char bgamtable[256];
static unsigned char rungamtable[256];
static unsigned char gungamtable[256];
static unsigned char bungamtable[256];
static short firsted;

static makegamtables();
static fixup();


gammapcolor(index,r,g,b)
int index, r, g, b;
{
    mapcolor(index,r,g,b);
}

static makegamtables()
{
    register float gamval;
    register float val;
    register short i;
    int rbal, gbal, bbal; 

    gamval = getgamma();
    getcolorbal(&rbal,&gbal,&bbal);
    for (i=0; i<256; i++) {
	rgamtable[i] = 255.0*gammacorrect((rbal*i)/(255.0*255.0),gamval)+0.5;
	ggamtable[i] = 255.0*gammacorrect((gbal*i)/(255.0*255.0),gamval)+0.5;
	bgamtable[i] = 255.0*gammacorrect((bbal*i)/(255.0*255.0),gamval)+0.5;
    }
    bzero(rungamtable,256);
    bzero(gungamtable,256);
    bzero(bungamtable,256);
    for (i=0; i<256; i++) {
	rungamtable[rgamtable[i]] = i;
	gungamtable[ggamtable[i]] = i;
	bungamtable[bgamtable[i]] = i;
    }
    fixup(rungamtable);
    fixup(gungamtable);
    fixup(bungamtable);
}

static fixup(cptr)
register unsigned char *cptr;
{
    register short i, lowval;

    for (i=256; i--; ) {
	if (*cptr == 0) 
	    *cptr = lowval;
	else
	    lowval = *cptr;
    }
}

gamgetmcolor(index,r,g,b)
int index;
short *r, *g, *b;
{
    getmcolor(index,r,g,b);
}

float gammacorrect(i,gamma)
float i, gamma;
{
    return pow(i,1.0/gamma);
}

float ungammacorrect(i,gamma)
float i, gamma;
{
    return pow(i,gamma);
}

newgamma()
{
    firsted = firsted = 0;
}

newgamtables()
{
    FILE *outf;
    register int i;
    short rtab[256], gtab[256], btab[256];

    makegamtables();

    for(i=0; i<256; i++) {
	rtab[i] = rgamtable[i];
	gtab[i] = ggamtable[i];
	btab[i] = bgamtable[i];
    }
    gammaramp(rtab,gtab,btab);
}
