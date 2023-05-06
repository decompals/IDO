/* 3Danim -  demonstration of an animation sequence using 
** a 3D texture. The texture is built up one image at a 
** time capturing frames from series of cylinders. Animation
** is simulated by moving a polygon through the 3D texture
** a little bit at a time.
**
** The polygon is rotated based on MOUSEX and MouseY position.
*/

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <math.h>

#define TEXSIZEX 64
#define TEXSIZEY 64
#define TEXSIZEZ 64

unsigned long texture[TEXSIZEX*TEXSIZEY*TEXSIZEZ];


/* This is actually from cylinder2.c of the glpg ch09 */
void
drawcyl()
{
double dy = .2;
double theta, dtheta = 2*M_PI/20;
double x, y, z;
float n[3], v[3];
int i, j;

        for (i = 0, y = -1;  i < 10;  i++, y += dy)  {
                bgntmesh();
                for (j = 0, theta = 0;  j <= 20;  j++, theta += dtheta)  {
                        if (j == 20)  theta = 0;
                        x = cos(theta);
                        z = sin(theta);
                        n[0] = x;  n[1] = 0;  n[2] = z;
                        n3f(n);
                        v[0] = x;  v[1] = y;  v[2] = z;
                        v3f(v);
                        v[1] = y + dy;
                        v3f(v);
                }
                endtmesh();
        }
}


/* This is actually from cylinder2.c of the glpg ch09 */
void
draw_texture_frame(int i)
{
Matrix Identity = { 1, 0, 0, 0,  0, 1, 0, 0,  0, 0, 1, 0,  0, 0, 0, 1 };

float mat[] = {
        AMBIENT, .1, .1, .1,
        DIFFUSE, 0.369, .7, .165,
        SPECULAR, .5, .5, .5,
        SHININESS, 10,
        LMNULL,
};

static float lm[] = {
        AMBIENT, .1, .1, .1,
        LOCALVIEWER, 1,
        LMNULL
};

static float lt[] = {
        LCOLOR, 1, 1, 1,
        POSITION, 0, 0, 1, 0,
        LMNULL
};

int xsize = TEXSIZEX;
int ysize = TEXSIZEY;
int xorigin = 0;
int yorigin = 0;
float ry,rx;

        loadmatrix(Identity);
        perspective(600, xsize/(float)ysize, .25, 15.0);
        lmdef(DEFMATERIAL, 1, 0, mat);
        lmdef(DEFLIGHT, 1, 0, lt);
        lmdef(DEFLMODEL, 1, 0, lm);
        lmbind(MATERIAL, 1);
        lmbind(LMODEL, 1);
        lmbind(LIGHT0, 1);
        translate(0., 0., -4.);

                ry = 300 * (2.0*(i-xorigin)/xsize-1.0);
                rx = -300 * (2.0*(i-yorigin)/ysize-1.0);
                czclear(0x404040, getgdesc(GD_ZMAX));
                pushmatrix();
                rot(ry, 'y');
                rot(rx, 'x');
                drawcyl();
                popmatrix();
}


/*draws a sequence of a lighted cylinder as the 3D texture */
void
compute_texture()
{
	int i;

	printf("computing texture...\n");

	pushviewport();
	viewport(0,TEXSIZEX-1,0,TEXSIZEY-1);

/* Render each frame and save as one slice of the 3D texture.
** Each slice is rendered into the bottom left corner to view
** the first time through.
*/
	for(i=0;i<TEXSIZEZ; i++)
	{
	 	draw_texture_frame(i);
		lrectread(0,0, TEXSIZEX-1,TEXSIZEY-1,
			texture+i*TEXSIZEX*TEXSIZEY);	
	pushviewport();
		viewport( 200, 450, 200, 450);
		lrectwrite (200, 200, 200+TEXSIZEX-1, 200+TEXSIZEY-1, texture+i*TEXSIZEX*TEXSIZEY);
	popviewport();
                swapbuffers();
	}
	popviewport();
	printf("texture computed\n");
}

/* END TEXTURE COMPUTATION */


int
main(int argc, char *argv[])
{
/* TRI_LINEAR is a new filter available. It may be used as a MAGFILTER
** only with 3D textures.
*/
float texprops[] = {TX_MINFILTER, TX_TRILINEAR,
                    TX_MAGFILTER, TX_TRILINEAR,
                    TX_WRAP, TX_REPEAT, 
		    TX_INTERNAL_FORMAT, TX_RGBA_12,
			TX_NULL};

/* Use the default texture environment */
float tevprops[] = {TV_NULL};

float t0[] = {0.0, 0., 0.0};   
float t1[] = {1.0, 0., 0.0};  
float t2[] = {1.0, 1.0, 0.0}; 
float t3[] = {0., 1.0, 0.0};  

float v0[] = {-2., -4.,0., 0.0};
float v1[] = {2., -4.,0., 0.0};
float v2[] = {2., 4.,0., 0.0};
float v3[] = {-2., 4.,0., 0.0};

short val;
int   dev;
long	win;

    if (getgdesc(GD_TEXTURE) == 0) {
        fprintf(stderr, "texture mapping not availble on this machine\n");
        return 1;
    }


	prefsize(500,500);
	keepaspect(1,1);
	foreground();
	win = winopen("3D texture");
	doublebuffer();
	RGBmode();
	gconfig();

	subpixel(TRUE);
	zbuffer(TRUE);
        lsetdepth(getgdesc(GD_ZMIN), getgdesc(GD_ZMAX));
	mmode(MVIEWING);

    	qdevice(ESCKEY);
    	qdevice(LEFTMOUSE);
    	qenter (LEFTMOUSE, 0);

	czclear(0x0,getgdesc(GD_ZMAX));
	swapbuffers();
	czclear(0x0,getgdesc(GD_ZMAX));
	swapbuffers();

	compute_texture();
	zbuffer(FALSE);

    perspective(600, 1., 1., 14.);
    texdef3d(1, 4, TEXSIZEX, TEXSIZEY, TEXSIZEZ, texture, 0, texprops);
    tevdef(1, 0, tevprops);
    texbind(TX_TEXTURE_0, 1);
    tevbind(TV_ENV0, 1);


    translate(0., 0., -4.);

    /* We need to slow this down to get a good view of the "animated"
    ** cylinder.  This is otherwise to fast and difficult to see.
    */
    swapinterval(3);

    while (TRUE){
        while(qtest()){
           dev = qread(&val);
           switch(dev){
                 case ESCKEY: 
			      gexit();
			      exit(0);
                              break;
                 case REDRAW: reshapeviewport();
                              break;

           } /* end main switch */
        } /* end qtest */

	czclear(0, getgdesc(GD_ZMAX));

        pushmatrix();
        rotate(getvaluator(MOUSEX)*5,'y');
        rotate(getvaluator(MOUSEY)*5,'z');

        cpack(0xffffffff);
        bgnpolygon();
        t3f(t0);   v3f(v0);
        t3f(t1);   v3f(v1);
        t3f(t2);   v3f(v2);
        t3f(t3);   v3f(v3);
        endpolygon();
        popmatrix();
        swapbuffers();

	/* By moving the texture coordinate forward, animation
	** is simulated.
	*/
	t0[2] += .05;
	t1[2] += .05;
	t2[2] += .05;
	t3[2] += .05;
    }
}
