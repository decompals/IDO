/* 
 *	getapixel - 
 *		Read a pixel from a specific screen location.
 *
 *				Paul Haeberli - 1985
 */
#include "gl.h"
#include "device.h"

getapixel(screenx, screeny)
int screenx, screeny;
{
    unsigned short pixel[1];

    pushviewport();
    pushmatrix();
    screenspace();
    cmov2i(screenx, screeny); 
    readpixels(1,pixel);
    popmatrix();
    popviewport();
    return pixel[0];
}

unsigned long getrgbpix(screenx, screeny, r, g, b)
int screenx, screeny;
int *r, *g ,*b;
{
    unsigned long lval;
    unsigned char cr, cg, cb;

    readdisplay(screenx,screeny,screenx,screeny,&lval,0);
    *r = (lval>>0)&0xff;
    *g = (lval>>8)&0xff;
    *b = (lval>>16)&0xff;
    return lval;
}
