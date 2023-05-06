/*
 *  projtex.c -
 *              A basic demonstration of texture projection. A
 *              scene is illuminated by either a spotlight or a
 *              pinhole slide projector, both of which are represented
 *              as texture maps projected into the environment.
 *		When selected, the geometry of the texture projection
 *		frustum (the projector) is drawn; the original unprojected
 *		texture appears on the screen of this texture projector.
 *
 *                  -- The RIGHT mouse button selects the "Options" 
 *                     popup menu.
 *		    -- Holding the LEFT mouse button down and moving
 *		       the mouse re-positions the viewpoint of the
 *		       texture projector.
 *                  -- Holding the MIDDLE mouse button down and moving
 *		       the mouse re-directs the texture
 *                     projector.
 *
 *      	For further details, please refer to the companion
 *		file README.projtex in the 4dgifts directory. 
 *
 *                      Carl Korobkin - November 1992
 *
 *
 */
#include <math.h>
#include <stdio.h>
#include <gl/gl.h>
#include <device.h>
#include <gl/image.h>

#define SAMPLES		4
#define ZSIZE		32
#define STENCILSIZE	0	

#define MENU_TOGGLE_PROJECTOR       1
#define MENU_TOGGLE_GEOMETRY        2
#define MENU_QUIT                   3

#define AMB		0x20202020
#define SPOT		1	
#define SLIDE		2
#define BRICK 		3	
#define MARBLE		4	
#define OAK		5	
#define SIGN		6
#define POST		7

#define DEGtoRAD  3.14159/180.

typedef struct vector {
            float x,y,z;
        } Vector;

void init_database();
void init_graphics();
void enable_texgenXYZW(int);
void process_image(char *, int, int, unsigned long*);

void draw_texscene();
void draw_ptexscene(int);
void draw_lookat_geometry();
void drawtri(float[],float[],float[],float[],float[],float[]);
void draw_texture_frustum(Vector,float,float,float[3],float,int);

void update_texture_matrix();
void update_arb_clipplanes(Vector,float[3], float, float);

void normalize(Vector *);
void xform_v4(float[], Matrix, float[]);
void xform_xpose_v4(float[], Matrix, float[]);
float dotprod(Vector, Vector);
void crossprod(Vector, Vector, Vector *);

Matrix rotXZ, MtexProjection, Mtex;
Matrix Mlook;

Matrix Id = { 1.0, 0.0, 0.0, 0.0,
              0.0, 1.0, 0.0, 0.0,
              0.0, 0.0, 1.0, 0.0,
              0.0, 0.0, 0.0, 1.0 };

float texprops00[] = {
			TX_MINFILTER, TX_MIPMAP_TRILINEAR, 
			TX_MAGFILTER, TX_BILINEAR,
			TX_WRAP, TX_REPEAT,
                        TX_INTERNAL_FORMAT, TX_RGB_5,
			TX_NULL };

float texprops01[] = {
			TX_MINFILTER, TX_BILINEAR,
			TX_MAGFILTER, TX_BILINEAR,
                        TX_INTERNAL_FORMAT, TX_RGB_5,
		 	TX_WRAP, TX_SELECT, TX_NULL };
/* XXX */
float texprops02[] = {
			TX_MINFILTER, TX_BILINEAR,
			TX_MAGFILTER, TX_BILINEAR,
                        TX_INTERNAL_FORMAT, TX_RGB_5,
		 	TX_WRAP, TX_SELECT, TX_NULL };

float tevprops[] = {TV_MODULATE, TV_NULL};

unsigned long slidetex[256*205];
unsigned long oaktex[128*64];
unsigned long buf256[256*256];
unsigned long buf64[64*64];

short rbuf[1024], gbuf[1024], bbuf[1024];

float v13[3], v14[3], v15[3], v16[3];
float v17[3], v18[3], v19[3], v20[3];
float v21[3], v22[3], v23[3], v24[3];
float v33[3], v34[3], v35[3], v36[3];
float v41[3], v42[3], v43[3], v44[3];
float v100[3], v200[3], v300[3], v400[3];
float v101[3], v201[3], v301[3], v401[3];

float t1[2], t2[2], t3[2], t4[2];
float t5[2], t6[2], t7[2], t8[2];
float t9[2], t10[2], t11[2], t12[2];

float t13[2], t14[2], t15[2], t16[2];
float t17[2], t18[2], t19[2], t20[2];
float t21[2], t22[2], t23[2], t24[2];
float t33[2], t34[2], t35[2], t36[2];
float t41[2], t42[2], t43[2], t44[2];
float t100[2], t200[2], t300[2], t400[2];

float vtmp1[4], vtmp2[4];

float eye_near, eye_far;	/* near and far for eye frustum */
float light_near, light_far;	/* near and far for texture frustum */
float tvx, tvy, tvz;		/* texture viewpoint */
float tpx, tpy, tpz;		/* reference pt. along texture's line-of-sight*/
Angle light_angle;		/* texture frustum field-of-view */

short val;
int relocate = FALSE;
int redirect = FALSE;
int drawgeometry = TRUE;
long shadow_x, shadow_y, shadow_z;
long mouse_x, mouse_y, delta_x, delta_y;
long menu;
long zmin, zmax;

IMAGE *iimage;

void
main(int argc, char *argv[])
{
    int i, y, txsize, tysize, projector = SPOT;
    float viewpt[3];
    float Angtan, Angcos;
    Vector Direction;

    eye_near = 10.;
    eye_far = 2500.;
    light_near = 10.;
    light_far = 2500.;

    init_graphics();
    init_database();

    qdevice(LEFTMOUSE);
    qdevice(RIGHTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(ESCKEY);

    menu =defpup("Options %t|toggle spot/slide|toggle projector geometry|quit");

    /*************************************************************/
    /* INITIAL TEXTURE-VIEW SPECIFICATION                        */
    /*************************************************************/

    light_angle = 600; 
    Angtan = ftan(DEGtoRAD*(light_angle/10.));
    Angcos = fcos(DEGtoRAD*(light_angle/20.));
    tvx = 50.; tvy = 75.; tvz = -70.;   /* texture viewpoint (arbitrary) */
    tpx = 30.; tpy = -90.; tpz = -70.;  /* texture ref point (arbitrary) */


    /* always take positive tangent */
    if (Angtan < 0)
        Angtan *= -1.;

    /* arbitrary clipplanes */
    viewpt[0] = tvx; viewpt[1] = tvy; viewpt[2] = tvz;
    Direction.x = tpx-tvx; Direction.y = tpy-tvy; Direction.z = tpz-tvz;
    update_arb_clipplanes(Direction, viewpt, light_near, light_far);

    mmode(MPROJECTION);
    perspective(light_angle, 1.0 , light_near, light_far);
    getmatrix(MtexProjection);
    update_texture_matrix();

    /*************************************************************/
    /* INITIALZE EYE-VIEW SPECIFICATION				 */
    /*************************************************************/
    mmode(MVIEWING);
    loadmatrix(Id);
    perspective(600, 1.0 , eye_near, eye_far);
    translate(0.0, 0.0, -400.);
    rot(-10., 'y');
    rot(5., 'x');

    /*************************************************************/
    /* Initilize texgen to produce homogenous texture coordinates*/
    /* relative to incoming object-space geometry.		 */
    /*************************************************************/
    {   float p[4];
        p[0] = 1.; p[1] = 0; p[2] = 0; p[3] = 0;
        texgen(TX_S,TG_LINEAR,p);
        p[0] = 0.; p[1] = 1; p[2] = 0; p[3] = 0;
        texgen(TX_T,TG_LINEAR,p);
        p[0] = 0.; p[1] = 0; p[2] = 1; p[3] = 0;
        texgen(TX_R,TG_LINEAR,p);
        p[0] = 0.; p[1] = 0; p[2] = 0; p[3] = 1;
        texgen(TX_Q,TG_LINEAR,p);
    }

    /*************************************************************/
    /* PARSE TEXTURES                                            */
    /*************************************************************/
    process_image("/usr/demos/data/textures/spotlight.rgb",256,256, buf256);
    texdef2d(SPOT, 4, 256,256, buf256, 0, texprops01);
    process_image("/usr/demos/data/textures/carl.rgb",256,205, slidetex);
    texdef2d(SLIDE, 4, 256,205, slidetex, 0, texprops01);
    process_image("/usr/demos/data/textures/brick2.rgb",64,64,buf64);
    texdef2d(BRICK, 4, 64,64, buf64, 0, texprops00);
    process_image("/usr/demos/data/textures/marble.rgb",64,64,buf64);
    texdef2d(MARBLE, 4, 64, 64, buf64, 0, texprops00);
    process_image("/usr/demos/data/textures/oak.rgb",128,64,oaktex);
    texdef2d( OAK, 4, 128, 64, oaktex, 0, texprops00);
    process_image("/usr/demos/data/textures/sign.rgb",64,64,buf64);
    texdef2d(SIGN, 4, 64, 64, buf64, 0, texprops00);
    process_image("/usr/demos/data/textures/material.rgb",64,64,buf64);
    texdef2d(POST, 4, 64, 64, buf64, 0, texprops00);

    tevdef( 1, 0, tevprops );
    tevbind( TV_ENV0, 1);

    /****************************/
    /* RENDER LOOP              */
    /****************************/
    while (TRUE) {
        while (qtest()) {
            switch(qread(&val)) {
            case REDRAW:
                reshapeviewport();
                break;
            case ESCKEY:
                if (!val) {
                    gexit();
                    exit(0);
                }
                break;
            case MIDDLEMOUSE:
                if (redirect == TRUE)
                    redirect = FALSE;
                else {
                    shadow_x = getvaluator(MOUSEX);
                    shadow_y = getvaluator(MOUSEY);
                    redirect = TRUE;
                }
                break;
            case LEFTMOUSE:
                if (relocate == TRUE)
                    relocate = FALSE;
                else {
                    shadow_y = getvaluator(MOUSEY);
                    shadow_x = getvaluator(MOUSEX);
                    relocate = TRUE;
                }
                break;
            case RIGHTMOUSE:
                if (val) {
                    switch(dopup(menu)) {
                        case MENU_TOGGLE_GEOMETRY:
                            drawgeometry = drawgeometry ? 0 : 1;
                            break;
                        case MENU_TOGGLE_PROJECTOR:
			    if (projector == SPOT) projector = SLIDE;
			    else projector = SPOT;
                            break;
                        case MENU_QUIT:
                            exit(0);
                        }
                    }
                    break;
            default:
                break;
            }
        }

        if (redirect) {
            mouse_x = getvaluator(MOUSEX);
            mouse_y = getvaluator(MOUSEY);
            delta_x = mouse_x - shadow_x;
            delta_y = mouse_y - shadow_y;

	    pushmatrix();
    	    loadmatrix(Id);
    	    rot((float)delta_y*0.5, 'x');
    	    rot((float)delta_x*0.5, 'z');
    	    getmatrix(rotXZ);
	    popmatrix();

	    /* get new tp (xyz) rotated by rotXZ tv(xyzw) origin */
            vtmp1[0] = tpx - tvx; 
            vtmp1[1] = tpy - tvy;
            vtmp1[2] = tpz - tvz;
            vtmp1[3] = 1.;
	    xform_v4(vtmp1, rotXZ, vtmp2);
	    tpx = vtmp2[0] + tvx;	    /* translate back */
	    tpy = vtmp2[1] + tvy;
	    tpz = vtmp2[2] + tvz;

            shadow_x = mouse_x;
            shadow_y = mouse_y;
            update_texture_matrix();

    	    viewpt[0] = tvx; viewpt[1] = tvy; viewpt[2] = tvz;
    	    Direction.x = tpx-tvx; Direction.y = tpy-tvy; Direction.z = tpz-tvz;
	    update_arb_clipplanes(Direction, viewpt, light_near, light_far);
        }

        if (relocate) {
            mouse_x = getvaluator(MOUSEX);
            mouse_y = getvaluator(MOUSEY);
            delta_x = mouse_x - shadow_x;
            delta_y = mouse_y - shadow_y;
            tvx += (float)delta_x;
            tvy += (float)delta_y;
	    v100[0]+= (float)delta_x;
	    v200[0]+= (float)delta_x;
	    v300[0]+= (float)delta_x;
	    v400[0]+= (float)delta_x;
	    v100[1]+= (float)delta_y;
	    v200[1]+= (float)delta_y;
	    v300[1]+= (float)delta_y;
	    v400[1]+= (float)delta_y;
	
            shadow_x = mouse_x;
            shadow_y = mouse_y;

            update_texture_matrix();

    	    viewpt[0] = tvx; viewpt[1] = tvy; viewpt[2] = tvz;
    	    Direction.x = tpx-tvx; Direction.y = tpy-tvy; Direction.z = tpz-tvz;
	    update_arb_clipplanes(Direction, viewpt, light_near, light_far);
        }

	/****************************************************************/
	/* RENDER PASS 1:  z-buffer from eye         		 	*/
	/****************************************************************/
        cpack(0xffffffff); 
	wmpack(0xffffffff);	/* clear rgba to ambient */
	czclear(AMB, zmax);
        zfunction(ZF_LEQUAL); 
        wmpack(0x00000000); 

	clipplane(0, CP_ON, NULL);
	clipplane(1, CP_ON, NULL);

        mmode(MTEXTURE);
        loadmatrix(Mtex);
        mmode(MVIEWING);

        /* note: do not bind a texture here using TX_SELECT, as */
        /*       texture coordinates outside the 0 to 1 range   */
        /*       will not be obtained.				*/ 

	draw_ptexscene(MARBLE);

	/************************************************************/
	/* RENDER PASS 2: render projected intensity into rgba      */
	/************************************************************/
	wmpack(0xffffffff);
        zfunction(ZF_EQUAL);
        blendfunction(BF_ONE, BF_ONE);	/* alpha <- ambient + intensity */

	if (projector == SPOT)
	    cpack(0xc0c0c0c0);

        draw_ptexscene(projector);

	clipplane(0, CP_OFF, NULL); 
	clipplane(1, CP_OFF, NULL);

	/************************************************************/
	/* RENDER PASS 3: resolve z again			    */
	/************************************************************/
        mmode(MTEXTURE);
        loadmatrix(Id);
        mmode(MVIEWING);

        wmpack(0x00000000); 
	zclear();		/* resolve z again for different path */
        zfunction(ZF_LEQUAL);
	draw_texscene();

	/************************************************************/
	/* RENDER PASS 4: draw scene at full brightness blended     */
	/************************************************************/
        wmpack(0xffffffff); 
        cpack(0xffffffff);              	/* full brightness */
        zfunction(ZF_EQUAL);
        blendfunction(BF_DC, BF_ZERO);
	draw_texscene();

	if (drawgeometry) {
            blendfunction(BF_ONE, BF_ZERO);
            zfunction(ZF_LEQUAL);
	    draw_texture_frustum(Direction,Angtan,Angcos,viewpt,40.,projector); 
	    draw_lookat_geometry();
	}
        swapbuffers();
    }
}


void
init_database()
{

    /* rear wall */
    v33[0] = -220.0;     v33[1] = -100.0;        v33[2] = -375.0;
    t33[0] = 0.0;      t33[1] = 0.0;
    v34[0] = 360.0;     v34[1] = -100.0;        v34[2] = -375.0;
    t34[0] = 1.0;       t34[1] = 0.0;
    v35[0] = 360.0;     v35[1] = 300.0;         v35[2] = -375.0;
    t35[0] = 1.0;       t35[1] = 1.0;
    v36[0] = -220.0;     v36[1] = 300.0;         v36[2] = -375.0;
    t36[0] = 0.0;      t36[1] = 1.0;

    /* left wall */
    v41[0] = -220.0;    v41[1] = -100.0;        v41[2] = 150.0;
    t41[0] = -3.0;      t41[1] = -3.0;
    v42[0] = -220.0;    v42[1] = -100.0;        v42[2] = -375.0;
    t42[0] = 3.0;       t42[1] = -3.0;
    v43[0] = -220.0;    v43[1] = 300.0;         v43[2] = -375.0;
    t43[0] = 3.0;       t43[1] = 3.0;
    v44[0] = -220.0;    v44[1] = 300.0;         v44[2] = 150.0;
    t44[0] = -3.0;      t44[1] = 3.0;

    /* floor */
    v13[0] = -220.0;    v13[1] = -100.0;        v13[2] = 125.0;
    t13[0] = -3.1;      t13[1] = -3.0;
    v14[0] = -220.0;    v14[1] = -100.0;        v14[2] = -375.0;
    t14[0] = 3.0;       t14[1] = -3.0;
    v15[0] = 360.0;     v15[1] = -100.0;        v15[2] = -375.0;
    t15[0] = 3.1;      t15[1] = 3.0;
    v16[0] = 360.0;     v16[1] = -100.0;        v16[2] = 125.0;
    t16[0] = -3.0;      t16[1] = 3.0;

    /* sign */
    v17[0] = 120.0;     v17[1] = 100.0;         v17[2] = -250.0;
    t17[0] = 0.13;      t17[1] = 0.04;
    v18[0] = 200.0;     v18[1] = 100.0;         v18[2] = -250.0;
    t18[0] = 0.84;      t18[1] = 0.04;
    v19[0] = 200.0;     v19[1] = 210.0;         v19[2] = -250.0;
    t19[0] = 0.84;      t19[1] = 0.93;
    v20[0] = 120.0;     v20[1] = 210.0;         v20[2] = -250.0;
    t20[0] = 0.13;      t20[1] = 0.93;

    /* post */
    v21[0] = 150.0;     v21[1] = -100.0;        v21[2] = -250.0;
    t21[0] = 0.0;       t21[1] = 0.0;
    v22[0] = 170.0;     v22[1] = -100.0;        v22[2] = -250.0;
    t22[0] = 1.0;       t22[1] = 0.0;
    v23[0] = 170.0;     v23[1] = 100.0;         v23[2] = -250.0;
    t23[0] = 1.0;       t23[1] = 1.0;
    v24[0] = 150.0;     v24[1] = 100.0;         v24[2] = -250.0;
    t24[0] = 0.0;       t24[1] = 1.0;

   /* texmap */
    v100[0] = 30.0;     v100[1] = 50.0; v100[2] = -50.0;
    t100[0] = 1.0;      t100[1] = 1.0;
    v200[0] = 30.0;     v200[1] = 50.0; v200[2] = -90.0;
    t200[0] = 1.0;      t200[1] = 0.0;
    v300[0] = 70.0;     v300[1] = 50.0; v300[2] = -90.0;
    t300[0] = 0.0;      t300[1] = 0.0;
    v400[0] = 70.0;     v400[1] = 50.0; v400[2] = -50.0;
    t400[0] = 0.0;      t400[1] = 1.0;

}


void
init_graphics()
{
    foreground();
    prefposition(10,1269,20, 1003);
    winopen("ProjectedTextures");
    subpixel(TRUE);
    RGBmode();
    doublebuffer();

    /* Invoke multisample buffer full-scene antialiasing
     * if available.
     */
    if (getgdesc(GD_MULTISAMPLE)) {
        multisample(TRUE);
        mssize(SAMPLES, ZSIZE, STENCILSIZE);
        zbsize(0);
    } else {
        multisample(FALSE);
    }

    zmin = getgconfig(GC_ZMIN);
    zmax = getgconfig(GC_ZMAX);
    lsetdepth(zmin, zmax);
    gconfig();

    frontbuffer(TRUE);
    cpack(0x00ff0000);
    clear();
    cpack(0x00ffffff);
    cmov2i(500,500);
    charstr("LOADING TEXTURES: PLEASE STAND BY");
    frontbuffer(FALSE);
    zbuffer(TRUE);
    mmode(MVIEWING);
}

void
update_texture_matrix()
{
    /* re-compose texture matrix */
    mmode(MTEXTURE);
    loadmatrix(Id);
    scale(.5,.5,1.);            /* !! REQUIRED FOR PROPER PROJECTION */
    translate(1.,1.,0.);        /* ditto */
    multmatrix(MtexProjection);
    lookat(tvx,tvy,tvz,tpx,tpy,tpz, 0.);
    getmatrix(Mtex);
    mmode(MVIEWING);
}


/********************************************************/
/* Draw a triangle for the given texture and geometry	*/
/* coordinates.						*/
/********************************************************/
void
drawtri(float tt1[2],float v1[3],float tt2[2], float v2[3],
         float tt3[2],float v3[3])
{
    bgnpolygon();
        t2f(tt1); v3f(v1);
        t2f(tt2); v3f(v2);
        t2f(tt3); v3f(v3);
    endpolygon();
}

void
update_arb_clipplanes(Vector N, float vp[3], float near, float far)
{
    float plane[4];
    Vector Pn, Pf;

    normalize(&N);

    Pn.x = vp[0]+near*N.x; Pn.y = vp[1]+near*N.y; Pn.z = vp[2]+near*N.z;
    Pf.x = vp[0]+far*N.x; Pf.y = vp[1]+far*N.y; Pf.z = vp[2]+far*N.z;

    plane[0] = N.x; plane[1] = N.y; plane[2] = N.z;
    plane[3] = -dotprod(Pn,N);
    clipplane(0, CP_DEFINE, plane); 
    N.x = -N.x; N.y = -N.y; N.z = -N.z;
    plane[0] = N.x; plane[1] = N.y; plane[2] = N.z;
    plane[3] = -dotprod(Pf,N);
    clipplane(1, CP_DEFINE, plane); 
}

void
process_image(char *iname, int xsize, int ysize, unsigned long* iarray)
{
    int y, i;

    if( (iimage=iopen(iname,"r")) == NULL ) {
        fprintf(stderr,"tobw: can't open input file\n");
        exit(1);
    }
    for(y=0; y < ysize; y++) { 
        getrow(iimage,rbuf,y,0);
        getrow(iimage,gbuf,y,1);
        getrow(iimage,bbuf,y,2);

        for( i=0; i< xsize; i++ )
            iarray[y*xsize + i] = 0xff000000 | bbuf[i]<<16 |
                                  gbuf[i]<<8 | rbuf[i];
    }
}

void
draw_ptexscene(int id)
{
    texbind(TX_TEXTURE_0,id);
    enable_texgenXYZW(TRUE); 

    drawtri(t13, v13, t15, v15, t16, v16);
    drawtri(t13, v13, t14, v14, t15, v15);

    drawtri(t33, v33, t35, v35, t36, v36 );
    drawtri(t33, v33, t34, v34, t35, v35 );

    drawtri(t41, v41, t43, v43, t44, v44 );
    drawtri(t41, v41, t42, v42, t43, v43 );

    drawtri(t17, v17, t19, v19, t20, v20);
    drawtri(t17, v17, t18, v18, t19, v19);

    drawtri(t21, v21, t23, v23, t24, v24 );
    drawtri(t21, v21, t22, v22, t23, v23 );

    enable_texgenXYZW(FALSE);
    texbind (TX_TEXTURE_0, 0); 
}


void
draw_texscene()
{
    texbind (TX_TEXTURE_0, OAK); 
    drawtri(t13, v13, t15, v15, t16, v16);
    drawtri(t13, v13, t14, v14, t15, v15);

    texbind (TX_TEXTURE_0, SIGN);
    drawtri(t17, v17, t19, v19, t20, v20);
    drawtri(t17, v17, t18, v18, t19, v19);

    texbind (TX_TEXTURE_0, POST);
    drawtri(t21, v21, t23, v23, t24, v24 );
    drawtri(t21, v21, t22, v22, t23, v23 );

    texbind (TX_TEXTURE_0, MARBLE); 
    drawtri(t33, v33, t35, v35, t36, v36 );
    drawtri(t33, v33, t34, v34, t35, v35 );

    texbind (TX_TEXTURE_0, BRICK);
    drawtri(t41, v41, t43, v43, t44, v44 );
    drawtri(t41, v41, t42, v42, t43, v43 );

    texbind (TX_TEXTURE_0, 0);  /* disable texture */
}

void
enable_texgenXYZW(int mode)
{
    if (mode == TRUE) {
        texgen(TX_S,TG_ON,0);
        texgen(TX_T,TG_ON,0);
        texgen(TX_R,TG_ON,0);
        texgen(TX_Q,TG_ON,0);
    } else {
        texgen(TX_S,TG_OFF,0);
        texgen(TX_T,TG_OFF,0);
        texgen(TX_R,TG_OFF,0);
        texgen(TX_Q,TG_OFF,0);
    }
}

void
draw_lookat_geometry()
{
        float v1[3], v2[3];

        v1[0] = tvx; v1[1] = tvy; v1[2] = tvz;
        v2[0] = tpx; v2[1] = tpy; v2[2] = tpz;

        cpack(0x00ff0000);
        bgnline();
            v3f(v1);
            v3f(v2);
        endline();
}

void
draw_texture_frustum(Vector Direction, float Atan, float Acos,
                      float vp[3], float d, int tid)
{
    float r;
    float v0[3], v1[3], v2[3], v3[3];
    float t0[2], t1[2], t2[2], t3[2];
    
    float ref[4], twist[4], x[4], y[4];
    Vector Ref, Twist, R0, R1, R2, R3;

    x[0] = 1.; x[1] = 0.; x[2] = 0.; x[3] = 0.;
    y[0] = 0.; y[1] = 1.; y[2] = 0.; y[3] = 0.;

    t0[0] = 0.; t0[1] = 0.;
    t1[0] = 1.; t1[1] = 0.;
    t2[0] = 1.; t2[1] = 1.;
    t3[0] = 0.; t3[1] = 1.;

    normalize(&Direction); 

    mmode(MVIEWING);
    pushmatrix();
    loadmatrix(Id);
    lookat(tvx,tvy,tvz,tpx,tpy,tpz, 0.);
    getmatrix(Mlook);
    popmatrix();
    xform_xpose_v4(x, Mlook, ref);
    xform_xpose_v4(y, Mlook, twist);

    Ref.x = ref[0]; Ref.y = ref[1]; Ref.z = ref[2];
    Twist.x = twist[0]; Twist.y = twist[1]; Twist.z = twist[2];

    /* compute corner 4 rays in plane of SQUARE bounding box:
     *    r0 = Ref + Twist; 
     *    r1 = Ref - Twist; 
     *    r2 = -Ref+ Twist; 
     *    r3 = -Ref - Twist; 
     */
    /* must be in a clockwise or coutner-clockwise order */
    R0.x = Ref.x+Twist.x;    R0.y = Ref.y+Twist.y;    R0.z = Ref.z+Twist.z;
    R1.x = Ref.x-Twist.x;    R1.y = Ref.y-Twist.y;    R1.z = Ref.z-Twist.z;
    R2.x = -Ref.x-Twist.x;   R2.y = -Ref.y-Twist.y;   R2.z = -Ref.z-Twist.z;
    R3.x = Twist.x-Ref.x;    R3.y = Twist.y-Ref.y;    R3.z = Twist.z-Ref.z;

    normalize(&R0); normalize(&R1); normalize(&R2); normalize(&R3); 

    /* Ray = Direction + (b)(R); where b = tan(theta)  */
    R0.x = Direction.x + (Atan*R0.x); R0.y = Direction.y +(Atan*R0.y);
    R0.z = Direction.z + (Atan*R0.z);
    R1.x = Direction.x + (Atan*R1.x); R1.y = Direction.y +(Atan*R1.y);
    R1.z = Direction.z + (Atan*R1.z);
    R2.x = Direction.x + (Atan*R2.x); R2.y = Direction.y +(Atan*R2.y);
    R2.z = Direction.z + (Atan*R2.z);
    R3.x = Direction.x + (Atan*R3.x); R3.y = Direction.y +(Atan*R3.y);
    R3.z = Direction.z + (Atan*R3.z);

    normalize(&R0); normalize(&R1); normalize(&R2); normalize(&R3); 

    r = d/Acos;

    v0[0] = (R0.x*r)+vp[0]; v0[1] = (R0.y*r)+vp[1]; v0[2] = (R0.z*r)+vp[2];
    v1[0] = (R1.x*r)+vp[0]; v1[1] = (R1.y*r)+vp[1]; v1[2] = (R1.z*r)+vp[2];
    v2[0] = (R2.x*r)+vp[0]; v2[1] = (R2.y*r)+vp[1]; v2[2] = (R2.z*r)+vp[2];
    v3[0] = (R3.x*r)+vp[0]; v3[1] = (R3.y*r)+vp[1]; v3[2] = (R3.z*r)+vp[2];

    /* Draw unprojected texture onto the texture screen geometry. */

    cpack(0xffffffff);
    texbind(0,tid);
    bgnpolygon();
        t3f(t3); v3f(v0);
        t3f(t0); v3f(v1);
        t3f(t1); v3f(v2);
        t3f(t2); v3f(v3);
    endpolygon();
    texbind(0,0);

    /* Draw the four corner rays of the texture frustum. */
    cpack(0x000000ff);
    bgnline(); v3f(vp); v3f(v0); endline();
    bgnline(); v3f(vp); v3f(v1); endline();
    bgnline(); v3f(vp); v3f(v2); endline();
    bgnline(); v3f(vp); v3f(v3); endline();
}


void
xform_v4(float v[4], Matrix m, float xv[4])
{
 xv[0] = v[0]*m[0][0] + v[1]*m[1][0] + v[2]*m[2][0] +v[3]*m[3][0];
 xv[1] = v[0]*m[0][1] + v[1]*m[1][1] + v[2]*m[2][1] +v[3]*m[3][1];
 xv[2] = v[0]*m[0][2] + v[1]*m[1][2] + v[2]*m[2][2] +v[3]*m[3][2];
 xv[3] = v[0]*m[0][3] + v[1]*m[1][3] + v[2]*m[2][3] +v[3]*m[3][3];
}

void
xform_xpose_v4(float v[4], Matrix m, float xv[4])
{
 xv[0] = v[0]*m[0][0] + v[1]*m[0][1] + v[2]*m[0][2] +v[3]*m[0][3];
 xv[1] = v[0]*m[1][0] + v[1]*m[1][1] + v[2]*m[1][2] +v[3]*m[1][3];
 xv[2] = v[0]*m[2][0] + v[1]*m[2][1] + v[2]*m[2][2] +v[3]*m[2][3];
 xv[3] = v[0]*m[3][0] + v[1]*m[3][1] + v[2]*m[3][2] +v[3]*m[3][3];
}

void
crossprod(Vector A, Vector B, Vector *R)
{
    R->x = (A.y*B.z)-(A.z*B.y);
    R->y = (A.z*B.x)-(A.x*B.z);
    R->z = (A.x*B.y)-(A.y*B.x);
}


float
dotprod(Vector A, Vector B)
{
    return(A.x*B.x + A.y*B.y + A.z*B.z); 
}


void
normalize(Vector *v)
{
    float mag;

    mag = fsqrt( (v->x * v->x) + (v->y * v->y) + (v->z * v->z) );

    if (mag == 0.) {
	 v->x = 1.0; v->y = 0.0; v->z = 0.0;
         mag = 1.;
        }
    mag = 1./mag;

    v->x = v->x * mag;
    v->y = v->y * mag;
    v->z = v->z * mag;
}
