#ifndef TEXTUREDEF
#define TEXTUREDEF

#include "image.h"
#include "vect.h"

typedef struct texture {
	int xsize;
	int ysize;
	int zsize;
	int bpp;
	unsigned char **data;
} TEXTURE;

float frand();
float xsin();
float fnoise3();
float onewrap();
TEXTURE *tmopen();
TEXTURE *tmalloc();
TEXTURE *restotm();
float noisefunc();
float wave();
float ocean();
float turbulence();
float ease();

#endif
