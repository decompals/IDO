/*
 *	glstuff -
 *		Graphics library support.
 *
 *				Paul Haeberli - 1988
 *
 */
#include "stdio.h"
#include "math.h"
#include "gl.h"
#include "device.h"
#include "port.h"
#include "gfxmach.h"

static quadrow();
static trirow();
static rectrow();


greybase()
{
    return 512;
}

savewindow(name)
char *name;
{
    char cmd[256];
    long xorg, yorg;
    long xsize, ysize;

    getorigin(&xorg,&yorg);
    getsize(&xsize,&ysize);
    sprintf(cmd,"scrsave %s %d %d %d %d\n",name,xorg,xorg+xsize-1,yorg,yorg+ysize-1);
    system(cmd);
}

printwindow()
{
    savewindow("/usr/tmp/print.rgb");
    system("printimage /usr/tmp/print.rgb");
    system("rm /usr/tmp/print.rgb");
    ringbell();
    ringbell();
}

static int outframe = 0;

savemovieframe()
{
    char oneline[256];

    if(outframe++ == 0) {
	system("rm -fr ssmovie");
	system("mkdir ssmovie");
    }
    sprintf(oneline,"ssmovie/%03d.rgb",outframe);
    savewindow(oneline); 
    ringbell();
    ringbell();
}

static int gfxmach = -1;
static long setnear, setfar;

gfxmachine()
{
    char str[256];

    if(gfxmach>=0)
	return gfxmach;
    gversion(str);
    if(strncmp(str,"GL4DNP",6) == 0)
        return MACH4DNP;
    if(strncmp(str,"GL4DRE",6) == 0)
        return MACH4DRE;
    if(strncmp(str,"GL4DXG",6) == 0)
        return MACH4DXG;
    if(strncmp(str,"GL4DLG",6) == 0)
	return MACH4DLIGHT;
    if(strncmp(str,"GL4DVG",6) == 0)
	return MACH4DVGX;
    if(strncmp(str,"GL4DGT",6) == 0)
	return MACH4DGT;
    if(strncmp(str,"GL4DPI",6) == 0) {
	if(getplanes()<=8)
	    return MACH4D8;
	else
	    return MACH4DPI;
    } 
    if(strncmp(str,"GL4D",4) == 0)
	return MACH4D;
    if(strncmp(str,"GL3D",4) == 0)
	return MACH3D;
    return MACH4DLIGHT;
}

smartsetdepth()
{
    fsetdepth(0.0,1.0);
}

static int firsted;
static double limnear, limdel;

fsetdepth(near,far)
float near, far;
{
    int inear, ifar;

    if(!firsted) {
	limnear = getgdesc(GD_ZMIN);
	limdel = (double)getgdesc(GD_ZMAX)-limnear;
	firsted = 1;
    }
    if(near<0.0)
	near = 0.0;
    if(far<0.0)
	far = 0.0;
    if(near>1.0)
	near = 1.0;
    if(far>1.0)
	far = 1.0;
    inear = (limnear+near*limdel)+0.499;
    ifar  = (limnear+far *limdel)+0.499;
    zviewport(inear,ifar);
}

zviewport(near,far)
long near, far;
{
    lsetdepth(near,far);
    setnear = near;
    setfar = far;
}

getzviewport(lnear,lfar)
long *lnear, *lfar;
{
    *lnear=setnear;
    *lfar=setfar;
}

drawmeshimage(imgdata,xsize,ysize)
unsigned long *imgdata;
int xsize, ysize;
{
    drawquadlimage(imgdata,xsize,ysize);
}

drawquadlimage(imgdata,xsize,ysize)
unsigned long *imgdata;
int xsize, ysize;
{
    int x, y;
    unsigned long *dptr;

    dptr = imgdata;
    pushmatrix();
    scale(1.0/(xsize-1),1.0/(ysize-1),1.0);
    for(y=0; y<(ysize-1); y++) {
	quadrow(dptr,dptr+xsize,xsize,y);
	dptr += xsize;
    }
    popmatrix();
}

static quadrow(y0,y1,n,y)
unsigned long *y0, *y1;
int n, y;
{
    float v0[2], v1[2];
    int x;
    
    v0[1] = y+0;
    v1[1] = y+1;
    bgntmesh();
    for(x=0; x<n; x++) {
	v1[0] = x;
	cpack(*y1++);
	v2f(v1);
	v0[0] = x;
	cpack(*y0++);
	v2f(v0);
    }
    endtmesh();
}

drawtrilimage(imgdata,xsize,ysize)
unsigned long *imgdata;
int xsize, ysize;
{
    int x, y, dxsize;
    unsigned long *dptr;

    ysize = (ysize-1)/2;
    dxsize = 2*((xsize-1)/2);
    dptr = imgdata;
    pushmatrix();
    scale(1.0/(dxsize-1),1.0/ysize,1.0);
    for(y=0; y<ysize; y++) {
	trirow(dptr,dptr+2*xsize,dxsize,y);
	dptr += 2*xsize;

    }
    popmatrix();
}

static trirow(y0,y1,n,y)
unsigned long *y0, *y1;
int n, y;
{
    float v0[2], v1[2];
    unsigned long *temp;
    int x;
    
    if(y&1) {
	temp = y0;
	y0 = y1;
	y1 = temp;;
	v0[1] = y+1;
	v1[1] = y+0;
    } else {
	v0[1] = y+0;
	v1[1] = y+1;
    }
    bgntmesh();
    v1[0] = 0;
    cpack(y1[0]);
    v2f(v1);
    for(x=0; x<n; x++) {
	if(x&1) {
	    v1[0] = x;
	    cpack(y1[x]);
	    v2f(v1);
	} else {
	    v0[0] = x;
	    cpack(y0[x]);
	    v2f(v0);
	}
    }
    if(n&1) {
	v1[0] = n-1;
	cpack(y1[n-1]);
	v2f(v1);
    } else {
	v0[0] = n-1;
	cpack(y0[n-1]);
	v2f(v0);
    }
    endtmesh();
}

drawrectlimage(imgdata,xsize,ysize)
unsigned long *imgdata;
int xsize, ysize;
{
    int x, y;
    unsigned long *dptr;

    dptr = imgdata;
    pushmatrix();
    scale(1.0/(xsize-1),1.0/(ysize-1),1.0);
    for(y=0; y<ysize-1; y++) {
	rectrow(dptr,dptr+xsize,xsize-1,y);
	dptr += xsize;

    }
    popmatrix();
}

static rectrow(y0,y1,n,y)
unsigned long *y0, *y1;
int n, y;
{
    float v0[2];
    int x;
    
    for(x=0; x<n; x++) {
	bgnpolygon();

	v0[0] = x;
	v0[1] = y;
	cpack(y0[0]);
	v2f(v0),

	v0[0] = x+1;
	v0[1] = y;
	cpack(y0[1]);
	v2f(v0),

	v0[0] = x+1;
	v0[1] = y+1;
	cpack(y1[1]);
	v2f(v0),

	v0[0] = x;
	v0[1] = y+1;
	cpack(y1[0]);
	v2f(v0),

	endpolygon();
	y0++;
	y1++;
    }
}
