#include "gl.h"
#include "image.h"

static int firsted = 0;


textureread(name,texno, filter_type)
    char *name;
    int texno, filter_type;
{
    unsigned long *imagedata;
    int i;
    static float texps0[] = { TX_NULL };
    static float texps1[] = { TX_MINFILTER, TX_MIPMAP_POINT, 
				   TX_MAGFILTER, TX_POINT, 
				   TX_NULL };
    static float texps2[] = { TX_MINFILTER, TX_MIPMAP_BILINEAR, 
				   TX_MAGFILTER, TX_BILINEAR, 
				   TX_NULL };
    static float texps3[] = { TX_MINFILTER, TX_BILINEAR, 
				   TX_MAGFILTER, TX_BILINEAR, 
				   TX_NULL };
    static float texps4[] = { TX_MINFILTER, TX_MIPMAP_LINEAR, 
				   TX_MAGFILTER, TX_BILINEAR, 
				   TX_NULL };
    static float tevps[] = { TV_NULL };

    imagedata = (unsigned long *)longimagedata(name);

    /*
    printf(" defining texture: %d\n", texno);
    */

    switch (filter_type) {
    case 0:
	texdef2d(texno,4,128,128,imagedata,0,texps0);
	break;
    case 1:
	texdef2d(texno,4,128,128,imagedata,0,texps1);
	break;
    case 2:
	texdef2d(texno,4,128,128,imagedata,0,texps2);
	break;
    case 3:
	texdef2d(texno,4,128,128,imagedata,0,texps3);
	break;
    case 4:
	texdef2d(texno,4,128,128,imagedata,0,texps0);
	break;
    }
    if(!firsted) {
       tevdef(1,0,tevps);
       tevbind(0,1);
       firsted = 1;
    }
}

textureread1(name,texno, filter_type)
    char *name;
    int texno, filter_type;
{
    long *imagedata;
    short *tmp;
    int i,j;
    static float texps0[] = { TX_NULL };
    static float texps1[] = { TX_MINFILTER, TX_MIPMAP_POINT, 
				   TX_MAGFILTER, TX_POINT, 
				   TX_NULL };
    static float texps2[] = { TX_MINFILTER, TX_MIPMAP_BILINEAR, 
				   TX_MAGFILTER, TX_BILINEAR, 
				   TX_NULL };
    static float texps3[] = { TX_MINFILTER, TX_BILINEAR, 
				   TX_MAGFILTER, TX_BILINEAR, 
				   TX_NULL };
    static float texps4[] = { TX_MINFILTER, TX_MIPMAP_LINEAR, 
				   TX_MAGFILTER, TX_BILINEAR, 
				   TX_NULL };
    static float tevps[] = { TV_BLEND, TV_COLOR, 0.65, 0.65, 0.60, 1.0,
			     TV_NULL };

    imagedata = (char *)charimagedata(name);
    for (i=0; i < 128*32; i++) {
    /*
            tmp = (short *)(imagedata + i);
	    *(tmp ) = (short) (((*tmp)/255.) * ((*tmp)/255.)) *255.*255.;
	    *(tmp +1) = *(tmp +1);
    */
	imagedata[i] = ~imagedata[i];
    }

    /*
    printf(" defining texture: %d\n", texno);
    */

    switch (filter_type) {
    case 0:
	texdef2d(texno,1,128,128,imagedata,0,texps0);
	break;
    case 1:
	texdef2d(texno,1,128,128,imagedata,0,texps1);
	break;
    case 2:
	texdef2d(texno,1,128,128,imagedata,0,texps2);
	break;
    case 3:
	texdef2d(texno,1,128,128,imagedata,0,texps3);
	break;
    case 4:
	texdef2d(texno,1,128,128,imagedata,0,texps0);
	break;
    }
    if(!firsted) {
       tevdef(1,0,tevps);
       tevbind(0,1);
       firsted = 1;
    }
}

static curtexture = 0;
settexture(n)
int n;
{
    if (n == curtexture)
      return;
    texbind(TX_TEXTURE_0,n);
    curtexture = n;
}

#define ENVSCALE 2.15

float stexgen(vert) 
     float vert[3];
{
     return ((vert[0] / ENVSCALE) + 0.5);
}

float ttexgen(vert) 
     float vert[3];
{
      return ((vert[1] / ENVSCALE) + 0.5);
}
