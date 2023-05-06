 /*
  *
  *	A simple example of how to do fast shadows on the RealityEngine
  *
  *
  *
  * The algorythm:
  *
  * o  Draw scene as viewed from light.
  * o  Take z-buffer values and make a shadow-map texture.
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

void make_shadow_map();
void set_light_transform();
void draw_scene();
void draw_light();
void init_texture_matrix();
void set_light_matrix_params();
void set_view_matrix_params();
void mult_light_matrix();
void mult_view_matrix();
void mult_inverse_view_matrix();
void mult_inverse_light_matrix();

#define LIGHT_FOV		400 	/* defined in 10's of degrees */

#define SHADOW_MAP_SIZE		512	/* must be power of 2 */
#define SHADOW_ZNEAR		(0x0000)
#define SHADOW_ZFAR		(0xffff)

unsigned long shadow_map_buffer[(SHADOW_MAP_SIZE * SHADOW_MAP_SIZE)/2];

float shadow_tx_props[] = {
    TX_MAGFILTER, TX_BILINEAR_LEQUAL,
    TX_MINFILTER, TX_BILINEAR_LEQUAL,
    TX_INTERNAL_FORMAT, TX_I_16,
    TX_WRAP, TX_CLAMP,
    TX_EXTERNAL_FORMAT, TX_PACK_16,
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

float translate_fudge 	= -0.0305;	/* fudge factors (see below) */
float slope_fudge 	= -11.125;

float light_rotation;
int view_azimuth;

Matrix light_projection;
Matrix light_transform;
Matrix light_inverse;

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


void
main()
{
    minsize(SHADOW_MAP_SIZE, SHADOW_MAP_SIZE);
    foreground();
    winopen("shadow");
    RGBmode();
    zbsize(32);
    doublebuffer();
    gconfig();

    zbuffer(TRUE);
    subpixel(TRUE);
    pixmode(PM_SIZE,16);         /* can only use 16 bits of Z data */
    readsource(SRC_ZBUFFER);		/* lrectreads get from z-buffer */

    tevdef(1, 0, tex_env_props);
    tevbind(TV_ENV0, 1);

    mmode(MVIEWING);
	loadmatrix(identity);

    lmdef(DEFLMODEL, 1, 0, default_lighting_props);
    lmdef(DEFMATERIAL, 1, 0, default_lighting_props);
    lmdef(DEFLIGHT, 1, 0, light_props);
    lmcolor(LMC_DIFFUSE);
    lmbind(MATERIAL, 1);

				/* Texgen generates texture coordinates
				   at each vertex that equal the world
				   coordinates of the vertex. */
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
	set_light_matrix_params();	/* Move the light around */

	set_view_matrix_params();	/* Move the viewer around */

	make_shadow_map();

	cpack(0x884422);
	clear();
	zclear();

				/* The inverse view matrix is needed
				   because texgen(*, TG_CONTOUR, *)
				   transforms coordinates by the
				   inverse of the model-view matrix.
				   We want the tex coords to be
				   stationary relative to the world,
				   not the viewer. */
	mmode(MTEXTURE);
	init_texture_matrix();
	mult_light_matrix();
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
	lmbind(LMODEL, 1);	/* Turn on lighting */

	mult_view_matrix();

				/* The light matrix is used to draw
				   from the viewpoint of the light.
				   So, the inverse light matrix will
				   transform the origin to the light.
				   We also bind the light again so
				   things will be lit from this
				   direction. */
	pushmatrix();
	mult_inverse_light_matrix(); 
	draw_light();		/* Draw a box at the light source */
	lmbind(LIGHT0, 1);
	popmatrix();

				/* Use the shadow map and draw the
				   scene.  At each pixel the texture
				   hardware will compare the R value
				   from texgen and the texel in the
				   shadow-map texture.  If R is <= to
				   the texel, the pixel is not in
				   shadow so the resulting texture
				   output (alpha) is 1.  Otherwise
				   texture output is 0. The comparison
				   is made after [s t r q] are
				   transformed by the textuer matrix. */
	texbind(TX_TEXTURE_0, 1); 

	draw_scene();

				/* Turn off lighting and texturing. */
	lmbind(LMODEL, 0);
	texbind(TX_TEXTURE_0, 0);
	swapbuffers();
    }
}

void
make_shadow_map()
{
float d;
    unsigned long pixel;
				/* The scene is drawn from the
				   viewpoint of the light in a window
				   the size of the shadow map texture.
				   lsetdepth is used because shadow
				   texels will hold a single 16 bit
				   value. */
    pushviewport();
    viewport(0, SHADOW_MAP_SIZE -1, 0, SHADOW_MAP_SIZE -1);
    lsetdepth(SHADOW_ZNEAR, SHADOW_ZFAR);
    czclear(0, SHADOW_ZFAR);

    set_light_transform();	/* View from light */
				/* Because an entire span of a polygon
				   might resolve to a single pixel, z
				   values for polygons that slope must
				   be shifted.  Slope_fudge is
				   empirically derived. */
    displacepolygon(slope_fudge);
    draw_scene();
    displacepolygon(0.0);

				/* Things outside of the spotlight
				   should be in shadow.  With TX_CLAMP
				   set, we need only change the z
				   values around the edge of the
				   texture to ZNEAR so the shadow test
				   will indicate shadow. */
    pixel = SHADOW_ZNEAR;
    zbuffer(FALSE);
    zdraw(TRUE);
    rectzoom(2, SHADOW_MAP_SIZE);
    lrectwrite(0, 0, 0, 0, & pixel);
    lrectwrite(SHADOW_MAP_SIZE -2, 0, SHADOW_MAP_SIZE -2, 0, & pixel);
    rectzoom(SHADOW_MAP_SIZE, 2);
    lrectwrite(0, 0, 0, 0, & pixel);
    lrectwrite(0, SHADOW_MAP_SIZE -2, 0, SHADOW_MAP_SIZE -2, & pixel);
    rectzoom(1, 1);
    zbuffer(TRUE);
    zdraw(FALSE);

				/* Read the z-buffer and turn it into
				   a shadow map. */
    lrectread(0, 0, SHADOW_MAP_SIZE -1, SHADOW_MAP_SIZE -1, shadow_map_buffer);
    texdef2d(1, 1, SHADOW_MAP_SIZE, SHADOW_MAP_SIZE,
	     (unsigned long *)shadow_map_buffer, 0, shadow_tx_props);

    popviewport();
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

				/* Transform z clip coords [-1, 1] to
				   16 bit unsigned z-buffer values */
    translate(0.0,0.0,32768.0);
    scale(1.0,1.0,32767.5);

				/* Empirically derived fudge factor to
				   deal with roundoff. */
    translate(0.0, 0.0, translate_fudge);

				/* Transforms x, y clip coords [-1, 1]
				   to s, t texture coords [0, 1] so
				   the texture is centered in the
				   spotlight. */
    multmatrix(clip_to_texture);
}

void 
set_light_matrix_params()
{
	static float light_angle = 0.0;
				/* Move the light around */
	light_angle += M_PI * 1.33333 / 360;
	if (light_angle > 2 * M_PI) 
	     light_angle -= 2 * M_PI;
	light_rotation = 70 * sinf(light_angle);
}

void
set_light_transform()
{
				/* Setup to draw from viewpoint of
				   light.  Keep these matrices around
				   to setup the texture matrix. */
    mmode(MPROJECTION);
    perspective(LIGHT_FOV, 1, 0.5, 10);
    getmatrix(light_projection);

    mmode(MVIEWING);
    loadmatrix(identity);
    translate(0, 0, -5);
    rot(light_rotation, 'y');
    getmatrix(light_transform);
}

void
mult_light_matrix()
{
				/* Projection matrices generate clip
				   coordinates in the [-1, 1] range. */
    multmatrix(light_projection);
    multmatrix(light_transform);
}

void
mult_inverse_light_matrix()
{
				/* Move the origin to the light point.
				   Gotten by doing the opposite
				   transforms of the light view
				   matrix. */
    rot(-light_rotation, 'y');
    translate(0, 0, 5);
}

void 
set_view_matrix_params()
{
				/* Move the viewer around */
	view_azimuth += 20;
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
    draw_cube();

    popmatrix();
}

void
draw_light()
{
    cpack(0xffff);
    pushmatrix();

    scale(0.25, 0.25, 0.25);
    translate(0, 0, 0.5);
    draw_cube();

    popmatrix();
}

