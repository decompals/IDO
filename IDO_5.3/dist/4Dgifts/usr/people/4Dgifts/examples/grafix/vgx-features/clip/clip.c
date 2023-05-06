/*			clip.c
 *
 *Description: "clip" shows how to define and turn on 
 *arbitrary clip planes. It also demonstrates that 
 *clipping planes are affected by the transformation stack 
 *at definition time. Also demonstrated is how to render
 *a convex object.
 *
 *					Martin R. McDoanld
 *					SGI
 *					JULY 1990
 *
 *Disclaimer:
 * "Zusammengestohlen aus Vershiedenem diesem und jenem."
 *                                      Ludwig van Beethoven
*/


#include <gl.h>
#include <device.h>
#include <math.h>
#include <stdio.h>


Matrix ident4 = {1.0, 0.0, 0.0, 0.0,
		 0.0, 1.0, 0.0, 0.0,
		 0.0, 0.0, 1.0, 0.0,
		 0.0, 0.0, 0.0, 1.0};

/* lighting set up */

float  yellow_material[] = {DIFFUSE,  1.0, 1.0, 0.0,
			    SPECULAR, 0.9, 0.9, 0.0,
			    LMNULL};

float  red_material[] = {DIFFUSE,  1.0, 0.0, 0.0,
			 SPECULAR, 0.9, 0.0, 0.0,
			 LMNULL};

float  green_material[] = {DIFFUSE,  0.0, 1.0, 0.0,
			   SPECULAR, 0.0, 0.9, 0.0,
			   LMNULL};
/*
** add TWOSIDE to the light model
*/
float two[] = { TWOSIDE, 1, LMNULL };


/*
** define a BACKMATERIAL
*/
float back[] =
	{SPECULAR,  0.3, 0.3, 0.3,
	 DIFFUSE,   0.8, 0.8, 0.8,
         SHININESS, 3.0,         
         AMBIENT,   0.2,0.2,0.2,
         LMNULL};

def_simple_light_calc(){
	lmdef(DEFMATERIAL, 1, 0, yellow_material);
	lmdef(DEFMATERIAL, 2, 0, red_material);
	lmdef(DEFMATERIAL, 3, 0, green_material);
	lmdef(DEFMATERIAL, 4, 0, back);
	lmdef(DEFLIGHT, 1, 0, NULL);
	lmdef(DEFLMODEL, 1, 0, two);
}

use_simple_light_calc(){
	lmbind(LIGHT0, 1);
	lmbind(LMODEL, 1);
	lmbind(BACKMATERIAL, 4);
}

float	angle = 0.;		/* counter for angle rotation */
int     clip = 1;
int	cncave = 0;
float   point[25][50][3]; /*  endpoints of the torus      */
float   norms[25][50][3]; /*  normals to the torus        */
int     numc = 25;
int     numt = 50;          /*  number of sides around section and top      */
float   radc = 0.2;
float   radt = 1.0;

/* plane equations for arbitrary clip
** planes. 
*/

float   clipnorms[6][4] = {
	{  1.,  0.,  0., .4 },
	{ -1.,  0.,  0., .4 },
	{  0.,  1.,  0., .4 },
	{  0., -1.,  0., .4 },
	{  0.,  0.,  1., .4 },
	{  0.,  0., -1., .4 }
	};

main ()
{
    short attached;
    short value;
    int dev;
    int menu,pupval;
    int i,j;

    attached = 1;
    initialize();

    def_simple_light_calc();
    use_simple_light_calc();

    menu = defpup("Menu%t|Clip on|Clip off|Convex|Concave|Exit");

    while (TRUE)
    {
	while (qtest() || !attached)
	{
	    dev = qread (&value);
	    switch(dev)
	    {
		 case RIGHTMOUSE:
		      pupval = dopup(menu);
			   switch(pupval)
			   {
			      case 1:
				 clip = 1;
				 break;
			      case 2:
				 for(i=0;i<6;i++)
				     clipplane(i,CP_OFF,NULL);
				 clip = 0;
				 if(cncave){
                                    for(i=0;i<6;i++)
                                    {
                                      for(j=0;j<4;j++)
                                      {
                                         clipnorms[i][j] *= -1;
                                       }
                                    }
                                 }
				 cncave = 0;
				 break;

/* Altering the plane definition
** here is all that we want to 
** do. The rendering subroutines
** will handle turning the planes
** on and off.
*/
			      case 3:
				 if (!clip) printf("turn clipping on\n");
				 else if(cncave){
				    for(i=0;i<6;i++)
				    {
				      for(j=0;j<4;j++)
				      {
					 clipnorms[i][j] *= -1;
				       }
				    }
				 }
				 cncave = 0;
				 break;
			      case 4:
				 if(!clip)
				   printf("turn clipping on\n");
				 else if(!cncave)
				 {
				    cncave = 1;
				    for(i=0;i<6;i++)
				    {
				      for(j=0;j<4;j++)
					 clipnorms[i][j] *= -1;
				    }
				 }
				 break;
			      case 5:
				 exit(0);
			   }
		      break;

	         case ESCKEY:
		      exit(0);

	    	 case REDRAW:
		      reshapeviewport();
		      drawpic();
		      break;

	         case INPUTCHANGE:
		      attached = value;	
		      break;
	    }
	}  
	drawpic();
    }   
} 
/*  end main()  */


initialize()
{
    int gid1;
    int i;

    if(!getgdesc(GD_CLIPPLANES)){
        printf("This only works on VGX machines/n");
        exit(0);
    }
    foreground();
    keepaspect(1,1);
    gid1 = winopen ("moving planes");

    doublebuffer();
    RGBmode();
    acsize(16);
    gconfig();
    lsetdepth(0, 0x7FFFFF);
    zbuffer(TRUE);
    subpixel(TRUE);
    inittorus ();

    qdevice (ESCKEY);
    qdevice (REDRAW);
    qdevice (INPUTCHANGE);
    qdevice (LEFTMOUSE);
    qdevice (MIDDLEMOUSE);
    qdevice (RIGHTMOUSE);
    qenter (REDRAW, gid1);

    mmode(MPROJECTION);
    perspective( 450, 1/1, 0.1, 100.0);

    mmode(MVIEWING);
    loadmatrix(ident4);
    polarview(5.0, 0., 0., 0.);

}

/* There are several items to note in this subroutine.
** Clipping planes are affected by the transformation
** stack at define time, not when turned on. It is thus
** necessary to redefine each plane in each frame. Also
** This subroutine also shows how to render a concave
** object by orienting the clipplanes in an outward facing
** direction and then render the object in multiple passes.
*/

drawpic()
{

	int	i;

    cpack(0);
    clear();
    zclear();
	if(cncave)
	{
	  for(i=0;i<6;i++)
	  {
          pushmatrix();
          rot(angle * 2.0, 'y');
            clipplane(i,CP_DEFINE,clipnorms[i]);
	    clipplane(i,CP_ON,NULL);
	  popmatrix();
	    drawscene(numc, numt);
	    clipplane(i,CP_OFF,NULL);
	  }
	}
	else
	{
           pushmatrix();
           rot(angle * 2.0, 'y');
           if(clip) for(i=0;i<6;i++)
		       {
           	       clipplane(i,CP_DEFINE,clipnorms[i]);
                       clipplane(i,CP_ON,NULL);
		       }
           popmatrix();
	   drawscene(numc, numt);
           if(clip) for(i=0;i<6;i++)
                       clipplane(i,CP_OFF,NULL);
	}
        swapbuffers();
    angle++;
}


drawscene()
{

       pushmatrix();
       lmbind(MATERIAL,3);
       drawtorus();
       scale(.45,.45,.45);
       rotate(900, 'x');
       rotate(450, 'y');
       lmbind(MATERIAL,2);
       drawtorus();
       rotate(900, 'y');
       lmbind(MATERIAL,1);
       drawtorus();
       popmatrix();
}


inittorus ( )
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
          point[i][j][0] = (radt+ radc*fcos(twopi*fi/numc))*fcos(twopi*fj/numt);
          point[i][j][1] = (radt+ radc*fcos(twopi*fi/numc))*fsin(twopi*fj/numt);
          point[i][j][2] = radc * fsin (twopi * fi/numc);
			{
				xc = radt*fcos(twopi*fj/numt);
				yc = radt*fsin(twopi*fj/numt);

				nx = point[i][j][0] - xc;
				ny = point[i][j][1] - yc;
				nz = point[i][j][2];

				n = fsqrt(nx*nx + ny*ny + nz*nz);

				norms[i][j][0] = nx/n;
				norms[i][j][1] = ny/n;
				norms[i][j][2] = nz/n;
			}
        }
    }
}

drawtorus()
{
    int i, j;


    for (i = 0; i < numc; i = i + 1)
	{
        bgntmesh();
	        n3f( norms[(i+1)%numc][0] );
	        v3f( point[(i+1)%numc][0] );
	        for (j = 0; j < numt; j = j + 1)
	        {
		            n3f( norms[i][j] );
		            v3f( point[i][j] );
			        n3f( norms[(i+1)%numc][(j+1)%numt] );
			        v3f( point[(i+1)%numc][(j+1)%numt] );
			}
	        n3f(norms[i][0]);
	        v3f(point[i][0]);
		endtmesh();
	}
 
}

