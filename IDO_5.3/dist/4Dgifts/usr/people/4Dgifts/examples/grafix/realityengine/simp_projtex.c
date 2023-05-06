 /*
  *
  *	A simple example of how to do projective textures 
  *	Note: for Reality Engines only
  *
  *
  *
  * The algorythm:
  *
  * o  transform each (x,y,z,w) vertex into a (s,t,q,r) vertex using texgen()
  * o  Set texture matrix with:
  *		Vinv * L * Clip
  *    	   where Vinv is the inverse view matrix, L is all the light view
  *	   tranformations, and Clip is a matrix which transforms clip
  *	   coordinates (-1..1 for x,y,z) to texture coordinates.
  * o  Draw the scene from the eye point.
  *
  */

#include <math.h>
#include <gl.h>

void set_tex_proj_transform();
void draw_scene();
void draw_tex_proj_pt();
void init_texture_matrix();
void set_tex_proj_matrix_params();
void set_view_matrix_params();
void mult_tex_proj_matrix();
void mult_view_matrix();
void mult_inverse_view_matrix();
void mult_inverse_tex_proj_matrix();

#define LIGHT_FOV		400 	/* defined in 10's of degrees */

float proj_tex_props[] = {
    TX_MINFILTER, TX_MIPMAP_LINEAR,
    TX_MAGFILTER, TX_BILINEAR,
    TX_NULL
    };

float tex_env_props[] = {
    TV_BLEND,
    TV_COLOR, 0.1, 0.1, 0.1, 0,
    TX_NULL
    };

float default_lighting_props[] = {
    LMNULL
    };

float light_props[] = {
    POSITION, 0, 0, 1, 0,
    LMNULL
    };

float tex_proj_rotation;
int view_azimuth;

Matrix tex_proj_projection;
Matrix tex_proj_transform;

float clip_to_texture[4][4] =  { 	/* Converts clip coordinates to */
    0.5, 0.0, 0.0, 0.0,			/* S and T texture coordinates */
    0.0, 0.5, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.5, 0.5, 0.0, 1.0,
};

float identity[4][4] = {
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
};


float floor_norm[3] = {			/* floor geometry */
    0, 0, 1
    };
float floor_vert[4][3] = {
    -3, -3, 0,
     3, -3, 0,
     3,  3, 0,
    -3,  3, 0
    };

float cube_norm[6][3] = {		/* cube geometry */
     0, 0, 1,
     0, 0,-1,
     1, 0, 0,
    -1, 0, 0,
     0, 1, 0,
     0,-1, 0
     };
float cube_vert[8][3] = {
    -0.5, -0.5, -0.5,
     0.5, -0.5, -0.5,
     0.5,  0.5, -0.5,
    -0.5,  0.5, -0.5,
    -0.5, -0.5,  0.5,
     0.5, -0.5,  0.5,
     0.5,  0.5,  0.5,
    -0.5,  0.5,  0.5
    };

unsigned long checker[64] =  {
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,

	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff,
	0xff00ffff, 0xff00ffff, 0xff00ffff, 0xff00ffff,
	0xff0000ff, 0xff0000ff, 0xff0000ff, 0xff0000ff
};


void
main()
{
    foreground();
    winopen("shadow");
    RGBmode();
    zbsize(32);
    doublebuffer();
    gconfig();

    zbuffer(TRUE);
    subpixel(TRUE);

    tevdef(1, 0, tex_env_props);
    tevbind(TV_ENV0, 1);

    mmode(MVIEWING);
    loadmatrix(identity);

    lmdef(DEFLMODEL, 1, 0, default_lighting_props);
    lmdef(DEFMATERIAL, 1, 0, default_lighting_props);
    lmdef(DEFLIGHT, 1, 0, light_props);
    lmcolor(LMC_DIFFUSE);
    lmbind(MATERIAL, 1);

    lmbind(LMODEL, 1);	/* Turn on lighting */

				/* Set up texture */
    texdef2d(1,4,8,8, checker, 5, proj_tex_props);
    texbind(TX_TEXTURE_0, 1); 

				/* Texgen generates texture coordinates (s,t,q,r)
				   at each vertex that equal the world
				   coordinates of the vertex (x,y,z,w). */
				/* The model-view matrix should be loaded with an
				   identity matrix when texgen is called */
    texgen(TX_S, TG_CONTOUR, identity[0]);
    texgen(TX_T, TG_CONTOUR, identity[1]);
    texgen(TX_R, TG_CONTOUR, identity[2]);
    texgen(TX_Q, TG_CONTOUR, identity[3]);
    texgen(TX_S, TG_ON, 0);
    texgen(TX_T, TG_ON, 0);
    texgen(TX_R, TG_ON, 0);
    texgen(TX_Q, TG_ON, 0);


    while (1)
    {
	set_tex_proj_matrix_params();	/* Move the tex projection around */
	set_view_matrix_params();	/* Move the viewer around */
        set_tex_proj_transform();	/* View from tex projection */

	cpack(0x884422);
	clear();
	zclear();
				/* set up the texture projection matrix */
				/* The inverse view matrix is needed
				   because texgen(*, TG_CONTOUR, *)
				   transforms coordinates by the
				   inverse of the model-view matrix.
				   We want the tex coords to be
				   stationary relative to the world,
				   not the viewer. */
	mmode(MTEXTURE);
				/* when loading the texture projection matrix
				   we want the Clip matrix, the texture projection
				   matrix, and the movement of the texture projection
				   matrix and the modeling transforms of the object
				   but not the viewing transforms of the eye */
	init_texture_matrix();
	mult_tex_proj_matrix();
	mult_inverse_view_matrix();

				/* Now, view from the eye. */
	mmode(MVIEWING);
	perspective(900, 1, 1, 20);
	loadmatrix(identity);

				/* Binding the light here makes the
				   light come from the eye.  I do this
				   so we can see the box drawn where
				   the spotlight shines from. */
	lmbind(LIGHT0, 1); 

	mult_view_matrix();

				/* The tex_proj matrix is used to draw
				   from the viewpoint of the texture projection.
				   So, the inverse texture projection matrix will
				   transform the origin to the texture projection. */
	pushmatrix();

	    mult_inverse_tex_proj_matrix();
				/* turn the texture off for the projection box */
    	    texbind(TX_TEXTURE_0, 0); 
	    texgen(TX_S, TG_OFF, 0);
	    texgen(TX_T, TG_OFF, 0);
	    texgen(TX_R, TG_OFF, 0);
	    texgen(TX_Q, TG_OFF, 0);

	    draw_tex_proj_pt();		/* Draw a box at the texture projection pt */

    	    texbind(TX_TEXTURE_0, 1); 
	    texgen(TX_S, TG_ON, 0);
	    texgen(TX_T, TG_ON, 0);
	    texgen(TX_R, TG_ON, 0);
	    texgen(TX_Q, TG_ON, 0);

	popmatrix();

	draw_scene();

	swapbuffers();
    }
}



/*
  The matrix routines below set projection, viewing, and inverse view
  transformation.  These routines are short but inefficient.  For a
  real application, matrices should be computed on the host rather
  than using getmatrix().
*/

void
init_texture_matrix()
{
    loadmatrix(identity);

				/* Transforms x, y clip coords [-1, 1]
				   to s, t texture coords [0, 1] so
				   the texture is centered in the
				   spotlight. */
				/* this is equivalent to a 
				scale(0.5,0.5,1.0); 
				translate(0.5,0.5,0.0); */
    multmatrix(clip_to_texture);
    scale(5.2,5.2,5.2); 	/* resize texture on objects to be bigger */
}

void 
set_tex_proj_matrix_params()
{
	static float tex_proj_angle = 0.0;

				/* Move the texture projection around */
	/*tex_proj_angle += M_PI * 1.33333 / 360;*/
	tex_proj_angle += M_PI * 0.66666 / 360;
	if (tex_proj_angle > 2 * M_PI) 
	     tex_proj_angle -= 2 * M_PI;
	tex_proj_rotation = 70 * sinf(tex_proj_angle);
}

void
set_tex_proj_transform()
{
				/* Setup to draw from viewpoint of
				   light.  Keep these matrices around
				   to setup the texture matrix. */
    mmode(MPROJECTION);
    perspective(LIGHT_FOV, 1, 0.5, 10);
    getmatrix(tex_proj_projection);

    mmode(MVIEWING);
    loadmatrix(identity);
    translate(0, 0, -5);
    rot(tex_proj_rotation, 'y');
    getmatrix(tex_proj_transform);
}

void
mult_tex_proj_matrix()
{
				/* Projection matrices generate clip
				   coordinates in the [-1, 1] range. */
    multmatrix(tex_proj_projection);
    multmatrix(tex_proj_transform);
}

void
mult_inverse_tex_proj_matrix()
{
				/* Move the origin to the light point.
				   Gotten by doing the opposite
				   transforms of the light view
				   matrix. */
    rot(-tex_proj_rotation, 'y');
    translate(0, 0, 5);
}

void 
set_view_matrix_params()
{
				/* Move the viewer around */
	/* view_azimuth += 20; */
	view_azimuth += 10;
	if (view_azimuth > 3600)
	    view_azimuth -= 3600;
}

void
mult_view_matrix()
{
    translate(0, 0, -5);
    rotate(-700, 'x');
    rotate(view_azimuth, 'z');
    translate(0, 0, -2.5);
}

void
mult_inverse_view_matrix()
{
				/* Do the opposite of the view matrix. */
    translate(0, 0, 2.5);
    rotate(-view_azimuth, 'z');
    rotate(700, 'x');
    translate(0, 0, 5);
}

void
draw_cube()
{
    bgnpolygon();
    n3f(cube_norm[0]);
    v3f(cube_vert[4]);
    v3f(cube_vert[5]);
    v3f(cube_vert[6]);
    v3f(cube_vert[7]);
    endpolygon();

    bgnpolygon();
    n3f(cube_norm[1]);
    v3f(cube_vert[3]);
    v3f(cube_vert[2]);
    v3f(cube_vert[1]);
    v3f(cube_vert[0]);
    endpolygon();

    bgnpolygon();
    n3f(cube_norm[2]);
    v3f(cube_vert[1]);
    v3f(cube_vert[2]);
    v3f(cube_vert[6]);
    v3f(cube_vert[5]);
    endpolygon();

    bgnpolygon();
    n3f(cube_norm[3]);
    v3f(cube_vert[3]);
    v3f(cube_vert[0]);
    v3f(cube_vert[4]);
    v3f(cube_vert[7]);
    endpolygon();

    bgnpolygon();
    n3f(cube_norm[4]);
    v3f(cube_vert[2]);
    v3f(cube_vert[3]);
    v3f(cube_vert[7]);
    v3f(cube_vert[6]);
    endpolygon();

    bgnpolygon();
    n3f(cube_norm[5]);
    v3f(cube_vert[0]);
    v3f(cube_vert[1]);
    v3f(cube_vert[5]);
    v3f(cube_vert[4]);
    endpolygon();
}

void
draw_scene()
{
    cpack(0xff);
    bgnpolygon();
    n3f(floor_norm);
    v3f(floor_vert[0]);
    v3f(floor_vert[1]);
    v3f(floor_vert[2]);
    v3f(floor_vert[3]);
    endpolygon();

    cpack(0xff00);
    pushmatrix();

    translate(0, 0, 0.5);
    scale(2.,2.,2.);
    draw_cube();

    popmatrix();
}

void
draw_tex_proj_pt()
{
    cpack(0xffff);
    pushmatrix();

    scale(0.25, 0.25, 0.25);
    translate(0, 0, 0.5);
    draw_cube();

    popmatrix();
}

