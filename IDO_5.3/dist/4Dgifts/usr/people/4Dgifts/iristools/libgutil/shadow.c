/*
 * 	shadow -
 *		Make it easy to select a halftone gray pattern for shadows.
 *
 *				Paul Haeberli - 1985
 */
#include "gl.h"

static unsigned short shadow[16] = {
	0x5555, 0xaaaa, 0x5555, 0xaaaa, 
	0x5555, 0xaaaa, 0x5555, 0xaaaa, 
	0x5555, 0xaaaa, 0x5555, 0xaaaa, 
	0x5555, 0xaaaa, 0x5555, 0xaaaa
}; 

#define SHADOWPAT	1255
#define DITHERPAT	1256

static int firsted;

shadowpattern()
{
    if(!firsted) {
	defpattern(SHADOWPAT,16,shadow);
	firsted++;
    }
    setpattern(SHADOWPAT);
}


fclear()
{
    nextdither();
    clear();
    setpattern(0);
}

static int shifts[16] = {
    0, 2, 2, 0,
    1, 3, 3, 1,
    0, 2, 2, 0,
    1, 3, 3, 1,
};

static int wheres[16] = {
    0, 2, 0, 2,
    1, 3, 1, 3,
    1, 3, 1, 3,
    0, 2, 0, 2,
};

static int texno;

nextdither()
{
    setdither(texno++);
}

setdither(n)
int n;
{
    unsigned short tex[16];

    ditherpat(n,tex);
    defpattern(DITHERPAT,16,tex);	
    setpattern(DITHERPAT);
}

ditherpat(n,tex)
int n;
unsigned short tex[16];
{
    register int i;
    register int shift, where, pattern;

    n = n&0xf;
    for(i=0; i<16; i++)
	tex[i] = 0;
    shift = shifts[n]; 	
    where = wheres[n]; 	
    pattern = 0x1111<<shift;
    tex[where+0] = pattern;
    tex[where+4] = pattern;
    tex[where+8] = pattern;
    tex[where+12] = pattern;
}

float frand();
static float thresh[16][16];
static unsigned short tex[16];

static short dithmat[4][4] = {
    0, 8, 2, 10,
    12, 4, 14, 6,
    3, 11, 1, 9,
    15, 7, 13, 5,
};

alphapat(alpha)
float alpha;
{
    unsigned short val, bit;
    float min, max;
    int x, y;
    static int firsted;

    if(alpha >0.99) {
	setpattern(0);
	return;
    }
    if(!firsted) {
        for(y=0; y<16; y++)
            for(x=0; x<16; x++) 
		thresh[y][x] = (dithmat[y%4][x%4]+frand())/16.0;
	firsted = 1;
    }
    max = alpha;
    for(y=0; y<16; y++) {
	val = 0;
	bit = 1;
	for(x=0; x<16; x++) {
	    if(thresh[y][x]<max) 
		val |= bit;
	    bit <<= 1;
	}
	tex[y] = val;
    }
    defpattern(23,16,tex);
    setpattern(23);
}
