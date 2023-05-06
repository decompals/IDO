/*****************************************************************************
 *
 * File:	createmovieConvert.c
 *
 * Description: Part of createmovie. Used only in conjunction with the
 *		SGI QuickTime Library. Code for converting between
 *		QuickTime and SGI rgb image data.
 *
 *****************************************************************************/

#include <sys/types.h>
#include "createmovieConvert.h"

/*
 * Forward declarations for functions local to this module.
 */

static void InvertImage32( void* buffer, int width, int height );
static void InvertImage16( void* buffer, int width, int height );

/********
 *
 * Apple16ToRGBX
 *
 * 16-bit images are stored with 5 bits each of red, green, and blue.
 * The bit layout is: 
 *
 *             XRRRRRGG GGGBBBBB
 *
 * The bit layout for SGI 32-bit RGBX images is: 
 *
 *             XXXXXXXX BBBBBBBB GGGGGGGG RRRRRRRR
 *
 * Apple stores images from top to bottom, while SGI goes from bottom to top.
 *
 ********/

void Apple16ToRGBX( int width, int height, void* from, void* to)
{
    static unsigned char Apple16Table[32];

    unsigned short* src  = ( unsigned short* ) from;
    __uint32_t*     dst  = ( __uint32_t* )     to;
    size_t          size = ( ( size_t ) width ) * ( ( size_t ) height );
    size_t          i;

    for ( i = 0;  i < 32;  i++ ) {
	Apple16Table[i] = ( ( i << 3 ) | ( i >> 2 ) );
    }

    for ( i = 0;  i < size;  i++ ) {
	unsigned short bits = src[i];
	unsigned char  red   = Apple16Table[( ( 0x1F << 10 ) & bits ) >> 10];
	unsigned char  green = Apple16Table[( ( 0x1F <<  5 ) & bits ) >>  5];
	unsigned char  blue  = Apple16Table[( ( 0x1F <<  0 ) & bits ) >>  0];
	dst[i] = ( blue << 16 ) | ( green << 8 ) | ( red << 0 );
    }
    
    InvertImage32( to, width, height );
}

/********
 *
 * RGBXToApple16
 *
 ********/

void RGBXToApple16( int width, int height, void* from, void* to)
{
    __uint32_t*     src  = ( __uint32_t* )     from;
    unsigned short* dst  = ( unsigned short* ) to;
    size_t          size = ( ( size_t ) width ) * ( ( size_t ) height );
    size_t          i;
    
    for ( i = 0;  i < size;  i++ ) {
	__uint32_t bits      = src[i];
	unsigned char  red   = ( ( 0x1F <<  3 ) & bits ) >>  3;
	unsigned char  green = ( ( 0x1F << 11 ) & bits ) >> 11;
	unsigned char  blue  = ( ( 0x1F << 19 ) & bits ) >> 19;
	dst[i] = ( blue << 0 ) | ( green << 5 ) | ( red << 10 );
    }
    
    InvertImage16( to, width, height );
}

/********
 *
 * Apple32ToRGBX
 *
 * 32-bit images are stored with 8 bits each of red, green, and blue.
 * The bit layout is: 
 *
 *         XXXXXXXX RRRRRRRR GGGGGGGG BBBBBBBB
 *
 * The bit layout for SGI 32-bit RGBX images is: 
 *
 *         XXXXXXXX BBBBBBBB GGGGGGGG RRRRRRRR
 *
 * Apple stores images from top to bottom, while SGI goes from bottom to top.
 *
 ********/

void Apple32ToRGBX( int width, int height, void* from, void* to)
{
    __uint32_t*     src  = ( __uint32_t* ) from;
    __uint32_t*     dst  = ( __uint32_t* ) to;
    size_t          size = ( ( size_t ) width ) * ( ( size_t ) height );
    size_t          i;

    for ( i = 0;  i < size;  i++ ) {
	__uint32_t bits = src[i];
	dst[i] = ( ( bits & ( 0xFF << 16 ) ) >> 16 ) |
	         ( ( bits & ( 0xFF <<  8 ) ) >>  0 ) |
		 ( ( bits & ( 0xFF <<  0 ) ) << 16 );
    }
  
    InvertImage32( to, width, height );
}

/********
 *
 * RGBXToApple32
 *
 * Apple packs the colors in a different order than SGI does:
 *
 ********/

void RGBXToApple32( int width, int height, void* from, void* to)
{
    __uint32_t*     src  = ( __uint32_t* ) from;
    __uint32_t*     dst  = ( __uint32_t* ) to;
    size_t          size = ( ( size_t ) width ) * ( ( size_t ) height );
    size_t          i;

    for ( i = 0;  i < size;  i++ ) {
	__uint32_t bits = src[i];
	dst[i] = ( ( bits & ( 0xFF << 16 ) ) >> 16 ) |
	         ( ( bits & ( 0xFF <<  8 ) ) >>  0 ) |
		 ( ( bits & ( 0xFF <<  0 ) ) << 16 );
    }
  
    InvertImage32( to, width, height );
}

/********
 *
 * InvertImage32
 *
 * Inverts a 32-bit image.
 *
 ********/

static void InvertImage32( void* buffer, int width, int height )
{
    __uint32_t* buff = ( __uint32_t* ) buffer;
    
    int x;
    int y1;
    for ( x = 0;  x < width;  x++ ) {
	for ( y1 = 0;  y1 < height/2; y1++ ) {
	    int y2 = height - y1 - 1;
	    int index1 = x + y1 * width;
	    int index2 = x + y2 * width;
	    
	    __uint32_t t = buff[index1];
	    buff[index1] = buff[index2];
	    buff[index2] = t;
	}
    }
}

/********
 *
 * InvertImage16
 *
 * Inverts a 16-bit image.
 *
 ********/

static void InvertImage16( void* buffer, int width, int height )
{
    unsigned short* buff = ( unsigned short* ) buffer;
    
    int x;
    int y1;
    for ( x = 0;  x < width;  x++ ) {
	for ( y1 = 0;  y1 < height/2; y1++ ) {
	    int y2 = height - y1 - 1;
	    int index1 = x + y1 * width;
	    int index2 = x + y2 * width;
	    
	    unsigned short t = buff[index1];
	    buff[index1]     = buff[index2];
	    buff[index2]     = t;
	}
    }
}

