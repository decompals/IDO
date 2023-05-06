/*
 *      surf.h
 *
 *      contains all global definitions/variables for calculating, 
 *      displaying and altering via menu input, the 5 nurbs models 
 *      defined in modeldata.c and displayed in model.c
 *
 *                                    R. E. Chang - 1990
 */

#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>


#define rad .7071067811865 /*  square root of 2 all divided by 2  */

typedef struct  {
        double x, y, z, w;
}   Vector4d;

typedef struct  {
        double s, t, w;
}   Vector3d;

typedef struct  {
        float x, y, z;
}   FVect3d;

typedef struct  {
        double s, t;
}   Vect2d;

/*
 *              model parameters
 */
int model;
float r, R;           /* r and R are minor and major radii of the torus;
                         R is also used as the radius of the sphere          */
Vector4d cpts[81];    /* control points for all the models                   */
Vector4d cpts2[81];   /* additional control pt array for hemisphere          */
Vector3d tpts[9];     /* contains rational trimming curve for the hemisphere */
Vector3d tcpts[2][3]; /* contains 1&3 quarter NURBS trimming curves          */
Vect2d cir[2][21];    /* contains the 2&4 quarater PWL trimming curve        */
Vector3d trimhole[9]; /* contains NURBS curve for circular hole in hemis1    */
int trimming;         /* toggles trimming, initially set on, only works for
                         model = 1                                           */
int prev_sample;
int sample;           /* sample rate of the PWL trim segments of hemis 2     */

/*
 *              NURBS parameters
 */
int order;             /*  order of the surface, all surfaces are quadric  */
int size_s;                /* number of columns of the control point array */
int size_s;                /* number of columns of the control point array */
int size_t;                /* number of rows of the control point array    */

/*
 *              display parameters                                        
 */
int cnet;                        /* toggles the display of the control net */
int mode;                /* keeps track of the state of MIDDLEMOUSE events
			  which rotate the model currently being displayed */
int BF_cull;                           /* backface culling default is off  */
float display_mode;                               /*  NURBS display param  */ 
float prev_pix_tol;
float pix_tol; 
float zpos;                    /* position of the eye--looking along x axis */

int mainmenu;
int modelmenu;
int trimmenu;
int displaymenu;
int pixelmenu;
int sample_rate_menu;



enum {NOTHING, ORIENT} movemode;

int omx, mx, omy, my;	/* old and new mouse position */

/* like qread only compresses extra MOUSEX, MOUSEY events */
static long nextqueue(short*);


extern void draw_model(void);
extern void define_torus(void);
extern void define_hemisphere(void);
extern void trim_hemis(void);
extern void trim_hole(void);
extern void trim_hemis_pieces(void);
extern void define_SR_sphere(void);
extern void draw_net(void);
extern void NURBS_quadric(void);
extern void makeaxes();
extern void set_props();
extern int set_model(int);
extern int set_display(int);
extern int set_pixtol(int);
extern int set_trim_sample_rate(int);
extern void build_displaymenu(void);

void initialize(void);
void orient(void);
void update_scene(void);
void draw_scene(void);
