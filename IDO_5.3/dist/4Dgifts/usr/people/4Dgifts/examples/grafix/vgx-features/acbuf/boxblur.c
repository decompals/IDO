/*			boxblur.c
 *	
 *Description: "boxblur" demonstrates the use of the accumulation
 *buffer to do blurring using a "box" filter. Each frame has equal
 *weight in the final picture. A predetermined number of frames are
 *chosen and all frames beyond this number are removed. This requires 
 *tracking the transformation state. The scene "n times ago" must be
 *rendered again and then removed from the accumulation buffer. This
 *is accomplished with the AC_ACCUMULATE option to the acbuf command.
 *The value passed along with this accumulating option may be negative.
 *By passing -1 as an option, the frames contribution is removed from
 *the accumulation buffer.
 *
 *					 Martin R. McDonald
 *					 SGI
 *					 JULY 1990
 *Disclaimer:
 * "Zusammengestohlen aus Vershiedenem diesem und jenem."
 *					Ludwig van Beethoven
 *
 *
*/

#include <gl.h>
#include <device.h>
#include <math.h>
#include <stdio.h>

Matrix ident4 = {1.0, 0.0, 0.0, 0.0,
		 0.0, 1.0, 0.0, 0.0,
		 0.0, 0.0, 1.0, 0.0,
		 0.0, 0.0, 0.0, 1.0};

def_simple_light_calc(){
	lmdef(DEFMATERIAL, 1, 0, NULL);
	lmdef(DEFLIGHT, 1, 0, NULL);
	lmdef(DEFLMODEL, 1, 0, NULL);
}

use_simple_light_calc(){
        lmbind(MATERIAL,1);
	lmbind(LIGHT0, 1);
	lmbind(LMODEL, 1);
}

float	angle = 0.;		/* counter for angle rotation */
float	frames = 10.;		/* Used to set the number of frames
				   saved in the accumulation buffer. */
int	framecount = 0;		/* used to see if the number of frames
				   accumulated thus far has reached the
				   number we want to save */
float point[25][50][3];		/*  endpoints of the torus	*/
float norm[25][50][3];		/*  normals to the torus	*/

main ()
{
    short attached;
    short value;
    int dev;
    int numc, numt;	/*  number of sides around section and top of torus*/

    attached = 1;
    initialize(&numc, &numt);

    def_simple_light_calc();       /* use defaults for lighting*/
    use_simple_light_calc();


    while (TRUE)
    {
	while (qtest() || !attached)
	{
	    dev = qread (&value);
	    switch(dev)
	    {
	      case ESCKEY:
		exit(0);
	      case REDRAW:
		reshapeviewport();
		acbuf(AC_CLEAR, 0.); /* clear the accumulation buffer out */
		framecount = 0;      /* and start over again */
		break;
	      case INPUTCHANGE:
		attached = value;	
		break;
	    }
	}   
	drawscene(numc, numt);
    }   
}   


initialize(numc, numt)
int *numc, *numt;	/*  number of sides around section and top	*/
{
    int gid1;
    float radc, radt;
    char answer;

    if(getgdesc(GD_BITS_ACBUF) == 0){
        printf("\nYou need a VGX to run this on\n");
        exit(0);
    }
    if(getgdesc(GD_BITS_ACBUF_HW) == 0){
        printf("\nYou have no hardware accumulation buffer.\n");
        printf("This will be VERY slow, do you wish to continue?\n");
        scanf("%c",&answer);
        if(answer != 'y') exit(0);
        prefsize(150,150);
    }
    else prefsize(600,600);

    gid1 = winopen ("boxblur");

    doublebuffer();
    RGBmode();
    acsize(16);  
    gconfig();
    lsetdepth(0, 0x7FFFFF);
    zbuffer(TRUE);
    subpixel(TRUE);
    radc = 0.2;
    *numc = 10;
    radt = 1.0;
    *numt = 20;
    inittorus (radc, *numc, radt, *numt);

    qdevice (ESCKEY);
    qdevice (REDRAW);
    qdevice (INPUTCHANGE);
    qdevice (LEFTMOUSE);
    qenter (REDRAW, gid1);

    mmode(MVIEWING);
    perspective( 450, 1/1, 0.1, 100.0);
    loadmatrix(ident4);
    translate(0.0, 0.0, -5.0);

    acbuf(AC_CLEAR, 0.);	/* initialize buffer to 0 */

}

drawscene(numc, numt)
int numc, numt;	/*  number of sides around section and top	*/
{
    float  lag_angle;

    czclear(0,getgdesc(GD_ZMAX));
    pushmatrix();
       rot(angle * 2.0, 'y');
       rot(angle * 2.0, 'x');
       drawtorus(numc, numt);
       acbuf(AC_ACCUMULATE,1.0);
    popmatrix();

    /* We need to remove the rendering that occured "frames" times
    ** ago. To do this we need to know how we transformed objects
    ** then and then go back to that state.
    */

    lag_angle = (2.0 * angle) - (2.0 * frames);
    if (framecount >= frames){
	pushmatrix();
	  cpack(0);
          clear();
          zclear();
	  rot(lag_angle, 'y');
	  rot(lag_angle, 'x');
	  drawtorus(numc, numt);
	  acbuf(AC_ACCUMULATE,-1.);
        popmatrix();
    }

    /* The accumulation buffer needs to be scaled back down 
    ** to 8 bits per pixel component. This is down by dividing by the
    ** number of frames that have been saved.
    */

    if(framecount<frames) acbuf(AC_RETURN, 1.0/(framecount));
    else acbuf(AC_RETURN, 1.0/(frames));
    swapbuffers();
    framecount++;
    angle++;
}

inittorus (rc, numc, rt, numt)
float rc;
int numc;
float rt;
int numt;
{
    int i, j, maxnumc, maxnumt;
    float twopi, fi, fj, val, xc, yc, nx, ny, nz, n;

    twopi = 2.0 * M_PI;

    maxnumc = 25;
    maxnumt = 50;
    
    if (numc > maxnumc)
        numc = maxnumc;
    if (numt > maxnumt)
        numt = maxnumt;

	/*  go around cross section	*/
    for (i = 0; i < numc; i = i + 1)
    {		
      fi = (float) i;
	  
	  /*  go around top view		*/
      for (j = 0; j < numt; j = j + 1)
        {	
            fj = (float) j;

            point[i][j][0] = (rt + rc*fcos(twopi*fi/numc))*fcos(twopi*fj/numt);
            point[i][j][1] = (rt + rc*fcos(twopi*fi/numc))*fsin(twopi*fj/numt);
            point[i][j][2] = rc * fsin (twopi * fi/numc);

		/* calculate normals */
		xc = rt*fcos(twopi*fj/numt);
		yc = rt*fsin(twopi*fj/numt);

		nx = point[i][j][0] - xc;
		ny = point[i][j][1] - yc;
		nz = point[i][j][2];

		n = fsqrt(nx*nx + ny*ny + nz*nz);

		norm[i][j][0] = nx/n;
		norm[i][j][1] = ny/n;
		norm[i][j][2] = nz/n;
        }
    }
}


drawtorus(numc, numt)
int numc, numt;
{
    int i,j;
    for (i = 0; i < numc; i = i + 1)
	{
        bgntmesh();
	        n3f( norm[(i+1)%numc][0] );
	        v3f( point[(i+1)%numc][0] );
	        for (j = 0; j < numt; j = j + 1)
	        {
		            n3f( norm[i][j] );
		            v3f( point[i][j] );
			        n3f( norm[(i+1)%numc][(j+1)%numt] );
			        v3f( point[(i+1)%numc][(j+1)%numt] );
			}
	        n3f(norm[i][0]);
	        v3f(point[i][0]);
		endtmesh();
	}
}
