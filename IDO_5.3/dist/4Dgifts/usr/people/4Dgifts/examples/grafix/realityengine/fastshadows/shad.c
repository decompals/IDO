/*
 *  shad.c - 
 *		A simple demo of real-time shadows.
 *
 *  			Rolf van Widenfelt - July 1992
 *			    also hacked by
 *  			Paul Haeberli - 1992
 *			    thanks for help from
 *			Mark Segal - 1992
 *
 *  $Revision: 1.3 $
 */
#include "stdio.h"
#include "string.h"
#include "math.h"
#include "gl.h"
#include "gl/device.h"
#include "gl/image.h"
#include "vect.h"
#include "sgiobj.h"

#define NLIGHTS 	3
#define SSIZE 		512	/* must be power of 2 */

#define MENU_LIGHTVIEW		1
#define MENU_EYEVIEW 		2
#define MENU_TOGGLECOLOR	3
#define MENU_QUIT 		4

#define SHADZNEAR	(0x0000)
#define SHADZFAR	(0xffff)
#define BACKCOLOR	(0x00303030)

#define WINTITLESTR "shad 4Dgifts"

void shadowdisplacez(int mode);
float *maketexprops();
float *maketevprops();
void fromlight(float mat[4][4]);
void myrectf(float x1,float y1,float x2,float y2,float z);
char *computetitle();

/*
 *  mouse and trackball functions
 */

typedef struct {
    float ztrans;
    float trotx, troty;
} TrackState;

float fgetmousex();
float fgetmousey();
void mytrackztrans(TrackState *ts,float z);
void mytrackrot(TrackState *ts,float x,float y);
void mytrackclick(TrackState *ts);
void mytrackpoll(TrackState *ts);
void mygettracktransform(TrackState *ts,float mat[4][4],float invmat[4][4]);

int nobjs;
sgiobj *obj[100];
long wxsize; /* window size */
long wysize;
long wxorg; /* window origin */
long wyorg;
long menu, lmenu;

long ZNear;
long ZFar;

int dolightview = 0;
int dozdisplace = 1;
int coloredlights = 0;
int dotracklight = 0; /* mouse button moves light */
int curlightnum = 0; /* by default, light 1 is edited with left shift key hack */

float light_fov = 20.0;
float light_near = 0.1;
float light_far = 10.0;

float eyemat[4][4];
float inveyemat[4][4];
float eye_dz = -0.0015;
float eye_fov = 20.0;
float eye_near = 0.1;
float eye_far = 10.0;

TrackState eyetrack[1];

typedef struct {
    int texno;
    float r, g, b;
    TrackState track[1];
    float mat[4][4];
    float invmat[4][4];
    float texm[4][4];
    unsigned long smap[(SSIZE*SSIZE)/2];
} LightBuffer;

void newshadowmap(LightBuffer *lightsource);

LightBuffer thelights[8];
LightBuffer *curlight;

float Identity[4][4] = { 
    1, 0, 0, 0,  
    0, 1, 0, 0,  
    0, 0, 1, 0,  
    0, 0, 0, 1,
};

/*
 *  control latitude with rotx, longitude with roty.
 *  translate away from object center with a negative tz.
 */
void
setuplight(LightBuffer *lightsource,float rotx,float roty,float tz)
{
    pushmatrix();
    loadmatrix(Identity);	    
    pushmatrix();
    translate(0.0,0.0,tz);
    rot(rotx,'x');
    rot(roty,'y');
    getmatrix(lightsource->mat);
    popmatrix();
    rot(-roty,'y');
    rot(-rotx,'x');
    translate(0.0,0.0,-tz);
    getmatrix(lightsource->invmat);
    popmatrix();
    lightsource->track->ztrans = tz;
    lightsource->track->trotx = rotx;
    lightsource->track->troty = roty;
    setlightcolor(lightsource,1.0,1.0,1.0);
}

setlightcolor(lightsource,r,g,b)
LightBuffer *lightsource;
float r, g, b;
{
    lightsource->r = r;
    lightsource->g = g;
    lightsource->b = b;
}

positionlight(invmat)
float invmat[4][4];
{
    pushmatrix();
    multmatrix(invmat);
    mymakelight(0,0.0,0.0,0.0);
    popmatrix();
}

mymakelight(i,x,y,z)
int i;
float x, y, z;
{
    float li_desc[14];

    li_desc[0] = AMBIENT;
    li_desc[1] = 0.1;
    li_desc[2] = 0.1;
    li_desc[3] = 0.1;
    li_desc[4] = LCOLOR;
    li_desc[5] = 1.0;
    li_desc[6] = 1.0;
    li_desc[7] = 1.0;
    li_desc[8] = POSITION;
    li_desc[9] = x;
    li_desc[10] = y;
    li_desc[11] = z;
    li_desc[12] = 1.0; 
    li_desc[13] = LMNULL;
    lmdef(DEFLIGHT,i+1,14,li_desc);
    lmbind(LIGHT0+i,i+1);
}

lfunc(n)
{
    n = n-1;
    if(n>=0 && n<NLIGHTS) {
	if(curlight) 
	    newshadowmap(curlight);
	curlight = &thelights[n];
	dolightview = 1;
	curlightnum = n; /* set current light for editing */
	wintitle(computetitle());
    }
    return 0;
}

#define LSCALE 		(0.25)
#define CLSCALE		(0.75)
#define SPECSCALE 	(0.50)

setlcolors()
{
    if(coloredlights) {
	setlightcolor(thelights+0,1.0*CLSCALE,0.0*CLSCALE,0.0*CLSCALE);
	setlightcolor(thelights+1,0.0*CLSCALE,0.6*CLSCALE,0.0*CLSCALE);
	setlightcolor(thelights+2,0.0*CLSCALE,0.0*CLSCALE,1.0*CLSCALE);
	setlightcolor(thelights+3,1.0*CLSCALE,0.5*CLSCALE,0.0*CLSCALE);
    } else {
	setlightcolor(thelights+0,1.0*LSCALE,1.0*LSCALE,0.6*LSCALE);
	setlightcolor(thelights+1,1.0*LSCALE,0.6*LSCALE,1.0*LSCALE);
	setlightcolor(thelights+2,0.6*LSCALE,1.0*LSCALE,1.0*LSCALE);
	setlightcolor(thelights+3,1.0*LSCALE,1.0*LSCALE,1.0*LSCALE);
    }
}

main(argc, argv)
int argc;
char *argv[];
{
    short val;
    int dev, i;
    int firstobj = 1;
    float *tevps;
    int needredraw;
    TrackState *track;

    if(argc<2) {
	fprintf(stderr,"usage: shadow obj1.sgo [obj2.sgi obj3.sgo . . .\n");
	exit(1);
    }
    minsize(SSIZE,SSIZE);
    keepaspect(5,4);
    winopen("shad");
    wintitle(computetitle());
    getorigin(&wxorg, &wyorg);
    getsize(&wxsize, &wysize);
    matrixinit();
    shadeinit();
    ZNear = getgdesc(GD_ZMIN);
    ZFar = getgdesc(GD_ZMAX);

    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);
    qdevice(LEFTSHIFTKEY);
    qdevice(LEFTCTRLKEY);
    qdevice(LEFTALTKEY);
    qdevice(TABKEY);
    qdevice(SPACEKEY);
    qdevice(ESCKEY);
    qdevice(QKEY);
    qdevice(WKEY);
    lmenu = defpup("Light Views %t %F|light 1|light 2|light 3",lfunc);
    menu = defpup("Options %t|light views %m|eye view|colored lights toggle|quit",lmenu);
    RGBmode();
    doublebuffer();
    gconfig();

    subpixel(TRUE);
    zbuffer(TRUE); 
    zfunction(ZF_LEQUAL);
    pixmode(PM_SIZE,16);
    readsource(SRC_ZBUFFER);

    cpack(0);
    clear();
    swapbuffers();
    nobjs = 0;
    for(i=firstobj; i<argc; i++) {
	obj[nobjs] = readsgiobj(argv[i]);
 	nobjs++;
    }

    mytrackztrans(eyetrack,-2.0);
    mytrackrot(eyetrack,0.0,0.0);

    setuplight(thelights+0, 30.0,  0.0,-2.2); /* choose z to avoid clipping */
    setuplight(thelights+1, 40.0,120.0,-2.2);
    setuplight(thelights+2, 50.0,240.0,-2.2);
    setuplight(thelights+3, 60.0,270.0,-2.2);
    setlcolors();

    tevps = maketevprops(0);
    tevdef(1,0,tevps);
    tevbind(0,1);

    gennewshadowmaps();

    curlight = 0;
    drawit();
    while (1)  {
	/* while (qtest())  { */
	    dev = qread(&val);
	    switch (dev)  {
		case QKEY:
		    if(val) {
			eye_dz -= 0.0001;
			fprintf(stderr,"eye_dz is %g\n",eye_dz);
			needredraw = 1;
		    }
		    break;
		case WKEY:
		    if(val) {
			eye_dz += 0.0001;
			fprintf(stderr,"eye_dz is %g\n",eye_dz);
			needredraw = 1;
		    }
		    break;
		case LEFTMOUSE:
		    if(val) {
			if (!dolightview && getbutton(LEFTSHIFTKEY)) {
			    dotracklight = 1;
			    wintitle(computetitle());
			    curlight = &thelights[curlightnum];
			    mytrackclick(curlight->track);
			    while(getbutton(LEFTMOUSE)) { 
				mytrackpoll(curlight->track);
				drawit();
			    }
			    dotracklight = 0;
			    wintitle(computetitle());
			    curlight = 0;
			    needredraw = 1;
			} else {
			    if (dolightview)
				track = curlight->track;
			    else
				track = eyetrack;
			    mytrackclick(track);
			    while(getbutton(LEFTMOUSE)) { 
				mytrackpoll(track);
				drawit();
			    }
			}
		    }
		    break;
		case MIDDLEMOUSE:
		    if(val) {
			if (!dolightview && getbutton(LEFTSHIFTKEY)) {
			    dotracklight = 1;
			    wintitle(computetitle());
			    curlight = &thelights[curlightnum];
			    mytrackclick(curlight->track);
			    while(getbutton(MIDDLEMOUSE)) {
				mytrackpoll(curlight->track);
				drawit();
			    }
			    dotracklight = 0;
			    wintitle(computetitle());
			    curlight = 0;
			    needredraw = 1;
			} else {
			    if (dolightview)
				track = curlight->track;
			    else
				track = eyetrack;
			    mytrackclick(track);
			    while(getbutton(MIDDLEMOUSE)) {
				mytrackpoll(track);
				drawit();
			    }
			}
		    }
		    break;
		case RIGHTMOUSE:
		    if (val) {
    			switch(dopup(menu)) {
			    case MENU_EYEVIEW:
				if(curlight)
				    newshadowmap(curlight);
				curlight = 0;
				dolightview = 0;
				wintitle(computetitle());
				break;
			    case MENU_TOGGLECOLOR:
				coloredlights = coloredlights ? 0 : 1;
				setlcolors();
				break;
			    case MENU_QUIT:
				exit(0);
			}
			needredraw = 1;
		    }
		    break;
		case REDRAW:
		    reshapeviewport();
		    getorigin(&wxorg,&wyorg);
		    getsize(&wxsize, &wysize);
		    needredraw = 1;
		    break;
		case ESCKEY:
		    exit(0);
		    break;
		default:
		    break;
	    }
	/* } */
	if (needredraw) {
	    drawit();
	    needredraw = 0;
	}
    }
}

gennewshadowmaps()
{
int i;
    for(i=0; i<NLIGHTS; i++) {
	thelights[i].texno = i+1;
	newshadowmap(thelights+i);
    }
}

void
newshadowmap(LightBuffer *lightsource)
{
    float *texps;

    shadeoff();
    mmode(MVIEWING); /* otherwise, messes up first time */
    loadmatrix(Identity); /* needed, but why? */
    lsetdepth(SHADZNEAR,SHADZFAR);
    viewport(0,SSIZE-1,0,SSIZE-1); 
    czclear(0,SHADZFAR);
    blendfunction(BF_ONE,BF_ZERO);
    zfunction(ZF_LEQUAL);
    ortho(-.5,SSIZE-.5,-.5,SSIZE-.5,-1,1);
    loadmatrix(Identity); /* needed, but why? */
    cpack(0xff00);
    myrectf(0,0,SSIZE-1,SSIZE-1,1); /* draw 2-pixel wide border with z=SHADZNEAR */
    myrectf(1,1,SSIZE-2,SSIZE-2,1);
    viewport(2,SSIZE-3,2,SSIZE-3); /* render real shadow map inside */
    fromlight(lightsource->mat);
    getmatrix(lightsource->texm);
    if (dozdisplace) 
	shadowdisplacez(1);
    drawmodel();
    if (dozdisplace) 
	shadowdisplacez(0);

    texps = maketexprops();
    lrectread(0,0,SSIZE-1,SSIZE-1,lightsource->smap);
    texdef2d(lightsource->texno,1,SSIZE,SSIZE,
	(unsigned long *)lightsource->smap,0,texps);
    viewport(0,wxsize-1,0,wysize-1); 
}

float specialmat[4][4] =  {
	0.5, 0.0, 0.0, 0.0,
	0.0, 0.5, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.5, 0.5, 0.0, 1.0,
};

texsetup(lightsource,inveye)
LightBuffer *lightsource;
float inveye[4][4];
{
    float p[4];

    /*
     *  set up texture matrix, but first, make sure ModelView is Identity
     *	so S,T,R,Q plane eqs are the same.  Remember, texgen(TG_CONTOUR)
     *	will xform the plane eqs by that ModelView matrix.
     */
    pushmatrix();
    loadmatrix(Identity);
    mmode(MTEXTURE);
    loadmatrix(Identity);
    translate(0.0,0.0,32768.0);
    scale(1.0,1.0,32767.5);
    translate(0.0,0.0,eye_dz);

    multmatrix(specialmat);
    multmatrix(lightsource->texm); 
    multmatrix(inveye);

    p[0] = 1.0; p[1] = 0; p[2] = 0; p[3] = 0;
    texgen(TX_S,TG_CONTOUR,p);
    p[0] = 0.0; p[1] = 1; p[2] = 0; p[3] = 0;
    texgen(TX_T,TG_CONTOUR,p);
    p[0] = 0.0; p[1] = 0; p[2] = 1; p[3] = 0;
    texgen(TX_R,TG_CONTOUR,p);
    p[0] = 0.0; p[1] = 0; p[2] = 0; p[3] = 1;
    texgen(TX_Q,TG_CONTOUR,p);
    texgen(TX_S,TG_ON,0);
    texgen(TX_T,TG_ON,0);
    texgen(TX_R,TG_ON,0);
    texgen(TX_Q,TG_ON,0);
    mmode(MVIEWING); /* clean up after texsetup() */
    popmatrix();
}

drawit()
{
    LightBuffer *lamp;
    int i,j;
    float *tevps;

    if (!dolightview) {
	if (dotracklight) {
	    mygettracktransform(curlight->track,curlight->mat,curlight->invmat);
	    newshadowmap(curlight);
	} else {
	    mygettracktransform(eyetrack,eyemat,inveyemat);
	}
    }

    lsetdepth(ZNear,ZFar);
    czclear(BACKCOLOR,ZFar);

    blendfunction(BF_ONE,BF_ZERO);
    zfunction(ZF_LEQUAL);
    if (dolightview) {
	mmode(MVIEWING);
	perspective((int)(light_fov*20),5.0/4.0,light_near,light_far);
	loadmatrix(Identity);
	pushmatrix();
	    mygettracktransform(curlight->track,curlight->mat,curlight->invmat);
	    multmatrix(curlight->mat);
	    positionlight(curlight->invmat); /* hack to get light at 0,0,0 */
	    shadeon();
	    setspecular(SPECSCALE,SPECSCALE,SPECSCALE);
	    setdiffuse(.6,.6,.6);
	    drawmodel();
	    shadeoff();
	popmatrix();
    } else {
	mmode(MVIEWING);
	perspective((int)(eye_fov*20),5.0/4.0,eye_near,eye_far);
	loadmatrix(eyemat);
	lamp = thelights;
	for(i=0; i<NLIGHTS; i++) {
	    texsetup(lamp,inveyemat);
	    if(i==0) {
		zfunction(ZF_LEQUAL);
		blendfunction(BF_ONE,BF_ZERO);
	    } else {
		zfunction(ZF_EQUAL);
		blendfunction(BF_ONE,BF_ONE);
	    }
	    pushmatrix();
		positionlight(lamp->invmat);
		mysettexture(lamp->texno);
		shadeon();
		setspecular(SPECSCALE/NLIGHTS,SPECSCALE/NLIGHTS,SPECSCALE/NLIGHTS);
		setdiffuse(lamp->r,lamp->g,lamp->b);
		drawmodel();
		shadeoff();
		mysettexture(0);
	    popmatrix();
	    lamp++;
  	}
    }

    swapbuffers();
}

void
shadowdisplacez(int mode)
{
    if (mode)
	displacepolygon(1.0);
    else
	displacepolygon(0.0);
}

void
fromlight(float mat[4][4])
{
    float m[4][4];

    mmode(MSINGLE);
    pushmatrix();
	perspective((int)(light_fov*20),1.0,light_near,light_far);
	multmatrix(mat);
	getmatrix(m);
    popmatrix();
    multmatrix(m);
}

drawmodel()
{
    int i;

    cpack(0xffffffff);
    for(i=0; i<nobjs; i++) {
	drawsgiobj(obj[i],DRAW_POINTS|DRAW_NORMALS);
    }
    lmcolor(LMC_COLOR);
}

float *pack3f(float x,float y,float z)
{
static float v[3];
    v[0] = x;
    v[1] = y;
    v[2] = z;
    return v;
}

void
myrectf(float x1,float y1,float x2,float y2,float z)
{
    bgnline();
    v3f(pack3f(x1,y1,z));
    v3f(pack3f(x2+1,y1,z));
    endline();
    bgnline();
    v3f(pack3f(x2,y1,z));
    v3f(pack3f(x2,y2+1,z));
    endline();
    bgnline();
    v3f(pack3f(x1,y1,z));
    v3f(pack3f(x1,y2+1,z));
    endline();
    bgnline();
    v3f(pack3f(x1,y2,z));
    v3f(pack3f(x2+1,y2,z));
    endline();
}

float *maketexprops()
{
    static float texps[32];
    int i = 0;

    texps[i++] = TX_MAGFILTER;
    texps[i++] = TX_BILINEAR_GEQUAL;
    texps[i++] = TX_MINFILTER;
    texps[i++] = TX_BILINEAR_GEQUAL;

    texps[i++] = TX_WRAP;
    texps[i++] = TX_CLAMP;
    texps[i++] = TX_INTERNAL_FORMAT;
    texps[i++] = TX_I_16;
    texps[i++] = TX_EXTERNAL_FORMAT;
    texps[i++] = TX_PACK_16;
#if 0
    texps[i++] = TX_NOCOPY; /* undocumented, but makes it quite fast */
#endif

    texps[i++] = TX_NULL;

    return texps;
}

float *maketevprops()
{
    static float tevps[32];
    int i = 0;

    tevps[i++] = TV_MODULATE;
    tevps[i++] = TV_COLOR;
    tevps[i++] = 1.0;
    tevps[i++] = 1.0;
    tevps[i++] = 1.0;
    tevps[i++] = 0.0;
    tevps[i++] = TX_NULL;

    return tevps;
}

mysettexture(n)
int n;
{
    if(n==0) {
        texbind(0,0);
    } else {
        texbind(0,n);
    }
}


static float oztrans;
static float otrotx, otroty;
static float trx, try;
static int dotrans;

float fgetmousex()
{
    return ((float)getvaluator(MOUSEX)-wxorg)/(float)wxsize;
}

float fgetmousey()
{
    return ((float)getvaluator(MOUSEY)-wyorg)/(float)wysize;
}

void
mytrackztrans(TrackState *ts,float z)
{
    ts->ztrans = z;
}

void
mytrackrot(TrackState *ts,float x,float y)
{
    ts->trotx = x;
    ts->troty = y;
}

void
mytrackclick(TrackState *ts)
{
    trx = fgetmousex();
    try = fgetmousey();
    if(getbutton(MIDDLEMOUSE))
	dotrans = 1;
    else
	dotrans = 0;
    oztrans = ts->ztrans;
    otrotx = ts->trotx;
    otroty = ts->troty;
}

void
mytrackpoll(TrackState *ts)
{
    if(dotrans) {
	ts->ztrans = oztrans+3.0*(fgetmousey()-try);
    } else {
	ts->trotx = otrotx+90.0*(fgetmousey()-try);
	ts->troty = otroty+360.0*(fgetmousex()-trx);
    }
}

void
mygettracktransform(TrackState *ts,float mat[4][4],float invmat[4][4])
{
    pushmatrix();
    loadmatrix(Identity);	    
    pushmatrix();
    translate(0.0,0.0,ts->ztrans);
    rot(ts->trotx,'x');
    rot(ts->troty,'y');
    getmatrix(mat);

    /* compute inverse by reversing transforms */
    popmatrix();
    rot(-ts->troty,'y');
    rot(-ts->trotx,'x');
    translate(0.0,0.0,-ts->ztrans);
    getmatrix(invmat);
    popmatrix();
}

char *computetitle()
{
static char s[100];
static char ls[100];

    ls[0] = 0;
    if (dolightview) {
	sprintf(ls,"- view from light %d",curlightnum+1);
    }
    else if (dotracklight) {
	sprintf(ls,"- tracking light %d",curlightnum+1);
    }
    sprintf(s,"%s %s",WINTITLESTR,ls);
    return s;
}
