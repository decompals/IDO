#ifndef TARGADEF
#define TARGADEF

typedef struct TARGA {
	unsigned char numid;	
	unsigned char maptyp;
	unsigned char imgtyp;	
	short maporig;
	short mapsize;
	unsigned char mapbits;
	short xorig;
	short yorig;
	short xsize;
	short ysize;
	unsigned char pixsize;
	unsigned char imgdes;
} TARGA;

#define	FLIPY	0x20

#endif
