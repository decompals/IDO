/*
 *  envmap.c - simple demo of the progression of drawing quality
 *      (from wireframe to flat lighting to texture mapping and reflections)
 *
 *  Rolf van Widenfelt - August 1990
 *
 *  Reflection (or environment) mapping is done using texture mapping.
 *  Texture coordinates are not provided by the application but are
 *  instead generated based on the normals defined in eye-coordinates.
 *  Appropriate texture coordinates are generated for a texture that
 *  contains the complete environment "warped" into a circle.
 *  Details of this mapping are explained in the texgen(3g) man page.
 *  The actual reflection calculation is done in the geometry engines.
 *
 *  The popup menu--accesible via the RIGHTMOUSE button--provides the
 *  interface to alter the object being mapped/viewed.  The "Change
 *  Model" item toggles between the initial cylinder and a "warped"
 *  version of the same.   
 *
 *  The 3 texture mappings on the menu breakdown as follows:
 *
 *   "Texgen"       texture coords S & T are generated based on their 
 *                  distance from 2 planes.
 *
 *   "Textured"     texture coords are used to wrap the texture around 
 *                  the cylinder.
 *
 *   "Reflection"   texgen is used to convert normals into the 
 *                  appropriate texture coords for the reflection map.
 *
 *  The "Wireframe" menu item, for simplicity sake, polymode(PYM_LINE)
 *  is used.  For highest performance use bgnline/endline instead of 
 *  polymode.
 *
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/image.h>

#define DOTEXTURE 1
#define DOREFLECTION 1

typedef struct {float x, y, z;} Vector3f;

Matrix Identity =
	{ 1, 0, 0, 0,  0, 1, 0, 0,  0, 0, 1, 0,  0, 0, 0, 1 };

float mat[] = {
	AMBIENT, .1, .1, .1,
	DIFFUSE, 0, .369, .165,
	SPECULAR, .5, .5, .5,
	SHININESS, 10,
	LMNULL,
};

float whitemat[] = {
	AMBIENT, .1, .1, .1,
	DIFFUSE, .6, .6, .6,
	SPECULAR, .5, .5, .5,
	SHININESS, 10,
	LMNULL,
};

static float lm[] = {
	AMBIENT, .1, .1, .1,
	LOCALVIEWER, 1,
	LMNULL
};

static float inflm[] = {
	AMBIENT, .1, .1, .1,
	LOCALVIEWER, 0,
	LMNULL
};

static float lt[] = {
	LCOLOR, 1, 1, 1,
	POSITION, 0, 0, 0, 1,
	LMNULL
};

static float inflt[] = {
	LCOLOR, 1, 1, 1,
	POSITION, 0, 1, 3, 0,
	LMNULL
};


static char defaultreflectname[] = "cafe.rgb";
static char defaulttexturename[] = "aztecNM.rgb";
static char defaulttexgenname[] = "corinth.rgb";


int drawcyl(int);
int drawwarpcyl(int);
void drawbg();
void textureread();
void framerate(char *);


#define MENU_WIREFRAME 1
#define MENU_FLATLIGHTED 2
#define MENU_LIGHTED 3
#define MENU_LOCALLIGHTED 4
#define MENU_TEXGENMAP 5
#define MENU_TEXTUREMAP 6
#define MENU_REFLECTMAP 7
#define MENU_NEWMODEL 8
#define MENU_EXIT 9


static char menustr[] =
    "Options %t|Wireframe|Flat Lighted|Lighted|Local Lighted|Texgen|Textured|Reflection Mapped|Change Model|Exit";


int
main(int argc, char *argv[])
{
    long xorig, yorig, xsize, ysize;
    float mx, my;
    float rx = 0, ry = 0;
    short val;
    long dev;
    int leftdown = 0, middledown = 0;
    int done = 0;
    long menuid;
    int dolines = 0;
    int doflatshade = 0;
    int dolighting = 1;
    int dolocal = 0;
    int dotexture = 0;
    int dotexgen = 0;
    int doreflection = 0;
    int dorefraction = 0;
    char msg[80], str[80];
    int ct;
    int model = 0;
    float focus = 4; /* distance to object center */
    float ztrans = 0;



    msg[0] = '\0';
    str[0] = '\0';

    prefsize(470,390);
    winopen("cylinder");
    winconstraints();
    getorigin(&xorig, &yorig);
    getsize(&xsize, &ysize);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);
    qdevice(ESCKEY);
    menuid = defpup(menustr);

    RGBmode();
    doublebuffer();
    gconfig();
    subpixel(TRUE);
    zbuffer(TRUE);
    backface(TRUE);
    linesmooth(SML_ON|SML_SMOOTHER /* |SML_END_CORRECT */ );
#ifdef DOTEXTURE
    if (argc > 3)  {
	textureread(argv[1], 1);    /* reflection map */
	textureread(argv[2], 2);    /* texture map */
	textureread(argv[3], 3);    /* texgen map */
    } else {
	textureread(defaultreflectname, 1);
	textureread(defaulttexturename, 2);
	textureread(defaulttexgenname, 3);
    }
#endif
    mmode(MVIEWING);
    loadmatrix(Identity);
    perspective(600, xsize/(float)ysize, .25, 20.0);
    lmdef(DEFMATERIAL, 1, 0, mat);
    lmdef(DEFMATERIAL, 2, 0, whitemat);
    lmdef(DEFLMODEL, 1, 0, lm);
    lmdef(DEFLMODEL, 2, 0, inflm);
    lmdef(DEFLIGHT, 1, 0, lt);
    lmdef(DEFLIGHT, 2, 0, inflt);

    lmbind(LIGHT0, 2); /* inf light */
    translate(0, 0, -focus);


    /*
     *	main event loop
     */
    while (!done)  { int i;

	czclear(0x404040, getgdesc(GD_ZMAX));

	framerate(msg);

	if (msg[0]) {
	    loadmatrix(Identity);
	    perspective(600, xsize/(float)ysize, .25, 20.0);
	    translate(0, 0, -focus);
	}

	while (qtest() != 0)  {

	    dev = qread(&val);
	    switch (dev)  {
		case LEFTMOUSE:
		    leftdown = val;
		    break;

		case MIDDLEMOUSE:
		    middledown = val;
		    break;

		case RIGHTMOUSE:
		    if (val)  {
			int menuval;
			menuval = dopup(menuid);
			if (menuval < 1)  break;
			if (menuval != MENU_NEWMODEL)  {
			    dolines = 0;
			    doflatshade = 0;
			    dolighting = 0;
			    dolocal = 0;
			    dotexture = 0;
			    dotexgen = 0;
			    doreflection = 0;
			    dorefraction = 0;
			}

			switch (menuval) {
			    case MENU_WIREFRAME:
				dolines = 1;
				break;
			    case MENU_FLATLIGHTED:
				doflatshade = 1;
				dolighting = 1;
				dolocal = 1;
				break;
			    case MENU_LIGHTED:
				dolighting = 1;
				break;
			    case MENU_LOCALLIGHTED:
				dolocal = 1;
				dolighting = 1;
				break;
			    case MENU_TEXGENMAP:
				dolighting = 1;
				dotexture = 1;
				dotexgen = 1;
				break;
			    case MENU_TEXTUREMAP:
				dolighting = 1;
				dotexture = 1;
				break;
			    case MENU_REFLECTMAP:
				dotexture = 1;
				doreflection = 1;
				break;
			    case MENU_NEWMODEL:
				model = model ? 0 : 1;
				break;
			    case MENU_EXIT:
				done = 1;
				break;
			}
	            }
		    break;

		case REDRAW:
		    reshapeviewport();
		    getorigin(&xorig, &yorig);
		    getsize(&xsize, &ysize);
		    perspective(600, xsize/(float)ysize, .25, 20.0);
		    break;

		case ESCKEY:
		    if (val)  done = 1;
		    break;

		default:
		    break;

            } /* end: switch (dev) */

	} /* end:  while (qtest() != 0) */

	pushmatrix();

	if (dolines) {
	    polymode(PYM_LINE);
	    blendfunction(BF_SA, BF_MSA);
	    zbuffer(FALSE);
	    backface(FALSE);
        } else {
	    polymode(PYM_FILL);
	    blendfunction(BF_ONE, BF_ZERO);
	    zbuffer(TRUE);
	    backface(TRUE);
	}

	if (doflatshade) {
	    shademodel(FLAT);
	} else {
	    shademodel(GOURAUD);
	}

	if (dolighting)
	if (dolocal) {
	    lmbind(LMODEL, 1); /* loc viewer */
	} else {
	    lmbind(LMODEL, 2); /* inf viewer */
	}


	if (dolighting && !dotexture) {
	    lmbind(MATERIAL, 1);
	} else if (dolighting && dotexture) {
	    lmbind(MATERIAL, 2);
	} else {
	    lmbind(MATERIAL, 0);
	}

#ifdef DOTEXTURE
	if (dotexture) {
	    tevbind(TV_ENV0, 1);
	}

	if (dotexture && doreflection) {
	    texbind(TX_TEXTURE_0, 1);
#if DOREFLECTION
	    texgen(TX_S, TG_SPHEREMAP, 0);
	    texgen(TX_T, TG_SPHEREMAP, 0);
	    texgen(TX_S, TG_ON, NULL);
	    texgen(TX_T, TG_ON, NULL);
#endif
	} else if (dotexture) {
	    static float splane[4] = {1,0,0,0};
	    static float tplane[4] = {0,.7071,.7071,0};

	    if (!dotexgen) {
		texbind(TX_TEXTURE_0, 2);
		texgen(TX_S, TG_OFF, NULL);
		texgen(TX_T, TG_OFF, NULL);
	    } else {
		texbind(TX_TEXTURE_0, 3);
		texgen(TX_S, TG_LINEAR, splane);
		texgen(TX_T, TG_LINEAR, tplane);
		texgen(TX_S, TG_ON, NULL);
		texgen(TX_T, TG_ON, NULL);
#if 0
		mmode(MTEXTURE);
		loadmatrix(Identity);
		scale(.3,.3,.3);
		mmode(MVIEWING);
#endif
	    }
	} else {
	    texbind(TX_TEXTURE_0, 0);
	}
#endif

	mx = 2.0*(getvaluator(MOUSEX)-xorig)/xsize-1.0;
	my = 2.0*(getvaluator(MOUSEY)-yorig)/ysize-1.0;

	if (leftdown) {
	    ztrans = 3*(1-mx)*(1-mx) - 3;
	}
	translate(0, 0, -ztrans);

	if (!leftdown)  if (middledown) {
	    rx = -300.0 * my;
	    ry = 300.0 * mx;
	} else {
	    rx += -10.0 * my;
	    ry += 10.0 * mx;
	}

	if (rx > 360.0)  rx -= 360.0;
	else if (rx < -360.0)  rx += 360.0;
	if (ry > 360.0)  ry -= 360.0;
	else if (ry < -360.0)  ry += 360.0;

	rot(ry, 'y');
	rot(rx, 'x');

	if (dolines)  cpack(0x80ffffff);
	else  cpack(0xffffffff);

	if (model) {
	    nmode(NNORMALIZE);
	    ct = drawwarpcyl(dotexture && !dotexgen);
	} else {
	    nmode(NAUTO);
	    ct = drawcyl(dotexture && !dotexgen);
	}

	popmatrix();

	if (msg[0]) {
	    sprintf(str, "%s  %d polygons", msg, ct);

	    if (dotexture) {
		texbind(TX_TEXTURE_0, 0);
	    }
	    pushmatrix();
	    loadmatrix(Identity);
	    zbuffer(FALSE);
	    blendfunction(BF_ONE, BF_ZERO);
	    ortho2(-.5, xsize-.5, -.5, ysize-.5);
	    cmov2i(5,5);
	    cpack(0xffff);
	    charstr(str);
	    popmatrix();
	}
	swapbuffers();
    }
}


void
drawbg()
{
    static float np[3] = {0,0,1};
    static float vp[4][3] = {{-3,-3,-1.2},{3,-3,-1.2},{3,3,-1.2},{-3,3,-1.2}};

    cpack(0xffffffff);
    bgnpolygon();
    n3f(np);
    v3f(vp[0]);
    v3f(vp[1]);
    v3f(vp[2]);
    v3f(vp[3]);
    endpolygon();
}


int
drawcyl(int stflag)
{
#define MAXI (10*2)
#define MAXJ (20*2*4)
#define MAXK 4
#define BEV .05
    double dy = 2.0/MAXI;
    double theta, dtheta = 2*M_PI/MAXJ;
    double x, y, z;
    double x2, z2;
    float u;
    float t[2], n[3], v[3], nb[3], vb[3];
    int i, j, k;
    int jj;
    static int inited = 0;
    static double costable[MAXJ+1], sintable[MAXJ+1];
    int ct = 0;

    if (!inited)  {
	for (j = 0, theta = 0;  j <= MAXJ;  j++, theta += dtheta)  {
	    costable[j] = cos(theta);
	    sintable[j] = sin(theta);
	}
	inited = 1;
    }

    for (i = 0, y = -1;  i < MAXI;  i++, y += dy)  {

	/* generate end caps */
	if (i == 0 || i == (MAXI-1))  {
	    t[0] = .5;  t[1] = .5;  t2f(t); /* want something caps */

	    for (j = 0, theta = 0;  j < MAXJ;  j++, theta += dtheta)  {

		jj = j;
		if (j == MAXJ)  jj = 0;
		x = costable[jj];
		z = sintable[jj];
		x2 = costable[jj+1];
		z2 = sintable[jj+1];

		bgntmesh();
		n[0] = 0;  n[2] = 0;
		if (i == 0)  n[1] = -1;  else  n[1] = 1;
		n3f(n);
		v[0] = 0;  v[2] = 0;
		if (i == 0)  v[1] = y - BEV;  else  v[1] = y + dy + BEV;
		v3f(v);
		for (k = 1;  k <= MAXK;  k++)  {
		    u = (1.0-BEV)* ((float)k) / MAXK;
		    v[0] = x * u;
		    v[2] = z * u;
		    vb[0] = x2 * u;
		    vb[1] = v[1];
		    vb[2] = z2 * u;
		    if (i == 0)  { v3f(v);  v3f(vb); }
		    else    { v3f(vb);  v3f(v); }
		}
		endtmesh();
		ct += 2*MAXK - 1;

		/* bevel */
		{  
		    Vector3f n0, v0, n1, v1, n2, v2, n3, v3;

		    n[0] = .7071 * x;
		    n[1] = -.7071;
		    n[2] = .7071 * z;
		    if (i == (MAXI-1))  n[1] = .7071;
		    nb[0] = .7071 * x2;
		    nb[1] = n[1];
		    nb[2] = .7071 * z2;

		    n0.x = nb[0];  n0.y = nb[1];  n0.z = nb[2];
		    v0.x = vb[0];  v0.y = vb[1];  v0.z = vb[2];

		    n1.x = n[0];  n1.y = n[1];  n1.z = n[2];
		    v1.x = v[0];  v1.y = v[1];  v1.z = v[2];

		    n2 = n1;
		    v2.x = x;
		    if (i == 0)  v2.y = y;  else  v2.y = y + dy;
		    v2.z = z;

		    n3 = n0;
		    v3.x = x2;
		    if (i == 0)  v3.y = y;  else  v3.y = y + dy;
		    v3.z = z2;

		    if (i == (MAXI-1))  {  float t; /* swap vertexes 1 and 3 */
			t = n1.x;  n1.x = n3.x;  n3.x = t;
			t = n1.y;  n1.y = n3.y;  n3.y = t;
			t = n1.z;  n1.z = n3.z;  n3.z = t;
			t = v1.x;  v1.x = v3.x;  v3.x = t;
			t = v1.y;  v1.y = v3.y;  v3.y = t;
			t = v1.z;  v1.z = v3.z;  v3.z = t;
		    }

		    bgnpolygon();
		    n3f(&n0.x);
		    v3f(&v0.x);
		    n3f(&n1.x);
		    v3f(&v1.x);
		    n3f(&n2.x);
		    v3f(&v2.x);
		    n3f(&n3.x);
		    v3f(&v3.x);
		    endpolygon();
		    ct++;
		}
	    }
	}

	/* generate cylinder body */
	bgntmesh();
	for (j = 0, theta = 0;  j <= MAXJ;  j++, theta += dtheta)  {
	    jj = j;
	    if (j == MAXJ)  jj = 0;
	    x = costable[jj];
	    z = sintable[jj];
	    if (stflag) {
		t[0] = 1 - ((float)j)/MAXJ;
		t[1] = (y+1)/2;
		t2f(t);
	    }
	    n[0] = x;  n[1] = 0;  n[2] = z;
	    n3f(n);
	    v[0] = x;  v[1] = y;  v[2] = z;
	    v3f(v);
	    if (stflag) {
		t[1] = (y+dy+1)/2;
		t2f(t);
	    }
	    v[1] = y + dy;
	    v3f(v);
	}
	endtmesh();
	ct += 2*MAXJ;

    }  /* end:   for (i=0, y=-1; i<MAXI; i++, y+=dy) */

    return ct;
}


#undef MAXI
#undef MAXJ
#undef MAXK
#undef BEV

int
drawwarpcyl(int stflag)
{
#define MAXI (2*10*2)
#define MAXJ (3*20)
#define MAXK 4
#define BEV .05
    double dy = 2.0/MAXI;
    double theta, dtheta = 2*M_PI/MAXJ;
    double x, y, z;
    double x2, z2;
    float u;
    float t[2], n[3], v[3], nb[3], vb[3];
    int i, j, k;
    int jj;
    static int inited = 0;
    static double costable[MAXJ+1], sintable[MAXJ+1];
    static double rtable[MAXI+1], drtable[MAXI+1];
    int ct = 0;

    if (!inited)  {
	for (j = 0, theta = 0;  j <= MAXJ;  j++, theta += dtheta)  {
	    costable[j] = cos(theta);
	    sintable[j] = sin(theta);
	}
	for (i = 0, y = -1;  i <= MAXI;  i++, y += dy)  {
	    float yy, d = 3, p = 3.2*2*M_PI, a = .2;
	    yy = (y+1)/2;
	    rtable[i] = 1 + a*(exp(-d*yy)-exp(-d)) * sin(p*yy);
	    drtable[i] = -d*a*exp(-d*yy)*sin(p*yy) +
		    a*exp(-d*yy)*p*cos(p*yy) - a*exp(-d)*p*cos(p*yy);
	}
	inited = 1;
    }

    for (i = 0, y = -1;  i < MAXI;  i++, y += dy)  {

	/* generate end caps */
	if (i == 0 || i == (MAXI-1))  {
	    t[0] = .5;  t[1] = .5;  t2f(t); /* want something caps */

	    for (j = 0, theta = 0;  j < MAXJ;  j++, theta += dtheta)  {

		jj = j;
		if (j == MAXJ)  jj = 0;
		x = costable[jj];
		z = sintable[jj];
		x2 = costable[jj+1];
		z2 = sintable[jj+1];

		bgntmesh();
		n[0] = 0;  n[2] = 0;
		if (i == 0)  n[1] = -1;  else  n[1] = 1;
		n3f(n);
		v[0] = 0;  v[2] = 0;
		if (i == 0)  v[1] = y - BEV;  else  v[1] = y + dy + BEV;
		v3f(v);
		for (k = 1;  k <= MAXK;  k++)  {
		    u = (1.0-BEV)* ((float)k) / MAXK;
		    v[0] = x * u;
		    v[2] = z * u;
		    vb[0] = x2 * u;
		    vb[1] = v[1];
		    vb[2] = z2 * u;
		    if (i == 0)  { v3f(v);  v3f(vb); }
		    else    { v3f(vb);  v3f(v); }
		}
		endtmesh();
		ct += 2*MAXK - 1;

		/* bevel */
		{  Vector3f n0, v0, n1, v1, n2, v2, n3, v3;
		n[0] = .7071 * x;
		n[1] = -.7071;
		n[2] = .7071 * z;
		if (i == (MAXI-1))  n[1] = .7071;
		nb[0] = .7071 * x2;
		nb[1] = n[1];
		nb[2] = .7071 * z2;

		n0.x = nb[0];  n0.y = nb[1];  n0.z = nb[2];
		v0.x = vb[0];  v0.y = vb[1];  v0.z = vb[2];

		n1.x = n[0];  n1.y = n[1];  n1.z = n[2];
		v1.x = v[0];  v1.y = v[1];  v1.z = v[2];

		n2 = n1;
		v2.x = x;
		if (i == 0)  v2.y = y;  else  v2.y = y + dy;
		v2.z = z;

		n3 = n0;
		v3.x = x2;
		if (i == 0)  v3.y = y;  else  v3.y = y + dy;
		v3.z = z2;

		if (i == (MAXI-1))  {  float t; /* swap vertexes 1 and 3 */
		    t = n1.x;  n1.x = n3.x;  n3.x = t;
		    t = n1.y;  n1.y = n3.y;  n3.y = t;
		    t = n1.z;  n1.z = n3.z;  n3.z = t;
		    t = v1.x;  v1.x = v3.x;  v3.x = t;
		    t = v1.y;  v1.y = v3.y;  v3.y = t;
		    t = v1.z;  v1.z = v3.z;  v3.z = t;
		}

		bgnpolygon();
		n3f(&n0.x);
		v3f(&v0.x);
		n3f(&n1.x);
		v3f(&v1.x);
		n3f(&n2.x);
		v3f(&v2.x);
		n3f(&n3.x);
		v3f(&v3.x);
		endpolygon();
		ct++;
	    }
	}
    }

	/* generate cylinder body */
	bgntmesh();
	for (j = 0, theta = 0;  j <= MAXJ;  j++, theta += dtheta)  {

	    jj = j;
	    if (j == MAXJ)  jj = 0;
	    x = costable[jj];
	    z = sintable[jj];
	    if (stflag) {
		t[0] = 1 - ((float)j)/MAXJ;
		t[1] = (y+1)/2;
		t2f(t);
	    }
	    n[0] = x;  n[1] = -drtable[i];  n[2] = z;
	    n3f(n);
	    v[0] = x*rtable[i];  v[1] = y;  v[2] = z*rtable[i];
	    v3f(v);
	    if (stflag) {
		t[1] = (y+dy+1)/2;
		t2f(t);
	    }
	    n[1] = -drtable[i+1];
	    n3f(n);
	    v[0] = x*rtable[i+1];  v[1] = y+dy;  v[2] = z*rtable[i+1];
	    v3f(v);
	}
	endtmesh();
	ct += 2*MAXJ;
    }

    return ct;
}


/*
 *  textureread reads a 128x128 texture file and defines it
 */
void textureread(name,texno)
char *name;
long texno;
{
    unsigned long *imagedata;

#if 0
    static float texps[] = {TX_MINFILTER,TX_BILINEAR,
			    TX_MAGFILTER,TX_BILINEAR,TX_NULL};
#else
    static float texps[] = {TX_MINFILTER,TX_MIPMAP_BILINEAR,
			    TX_MAGFILTER,TX_BILINEAR,TX_NULL};
#endif
    static float tevps[] = {/*TV_MODULATE,*/ TV_NULL};

    imagedata = (unsigned long *)longimagedata(name);
    texdef2d(texno, 4, 128, 128, imagedata, 0, texps);
    tevdef(1, 0, tevps);
}


#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

/* Update polygons/sec every FRAMES frames */
#define FRAMES 30

/*
 *  called every frame to display drawing performance.
 *  something is displayed every frame but recalculated less often.
 */
void
framerate(char *s)
{
    static long lastt = 0;
    static int frames = 0;
    struct tms buf;
    float rate;

    if (lastt == 0)	{
	lastt = times(&buf);
	*s = '\0';
	return;
    }

    if (++frames >= FRAMES)	{
	long t, et;

	t = times(&buf);
	et = t - lastt;
	rate = ((float)(frames * HZ)) / et;
	if (rate >= 1.0) {
	    sprintf(s, "%4.2g frames/second", rate);
	} else {
	    sprintf(s, "%4.2g seconds/frame", 1/rate);
	}
	frames = 0;
	lastt = t;
    }
}
