/*                        
 *                            reflection.c
 *        
 *    Reflections may be achieved through explicit assignment of 
 *  texture coordinates. The simplest way to start with textures is to 
 *  have a scanned in image made from a wide angle photograph. The 
 *  center of the picture represents the top of a sphere. Offsets from 
 *  the center may be thought of as the x and y components of a normal. 
 *  These components will be used in assigning the texture component. 
 *  True reflection mapping would take into account the positional data
 *  of the normal as well. As can be seen, very good results can be 
 *  achieved without the extra work.  While the program is running, 
 *  alpha blending may be activated by pressing the A key. The G key 
 *  will give the surface a gold appearance rather than the default 
 *  chrome.
 *
 *                        Martin R. McDonald  -- SGI  -- JULY 1990
 *  Disclaimer:
 *     "Zusammengestohlen aus Vershiedenem diesem und jenem."
 *                                        Ludwig van Beethoven
*/

#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

Matrix ident4 = {1.0, 0.0, 0.0, 0.0,
                 0.0, 1.0, 0.0, 0.0,
                 0.0, 0.0, 1.0, 0.0,
                 0.0, 0.0, 0.0, 1.0};

/* specify texture mapping 
** and texture environment 
** properties.
*/

float texps[] = {TX_MINFILTER,TX_MIPMAP_BILINEAR,
                 TX_MAGFILTER,TX_BILINEAR,
                 TX_NULL};
float tevps[] = {TV_NULL};

int     angle = 0;              /* counter for angle rotation */
unsigned long *image;
float t0[2], t1[2], t2[2], t3[2];
float point[25][50][3];        /*  endpoints of the torus        */
float norms[25][50][3];        /*  normals to the torus        */
int numc = 25;
int numt = 50;                /*  number of sides around section and top        */
float radc = 0.2;
float radt = 1.0;
int  glass = 0;
int  gold = 0;

blend()
{
        if(glass)
	   {
           blendfunction(BF_ONE, BF_ONE);
	   zbuffer(FALSE);
	   }
        else
	   {
           blendfunction(BF_ONE, BF_ZERO);
	   zbuffer(TRUE);
	   }
}

inittex()
{

    /* Read in the image file to use for reflection.
    ** this image was taken with a 180 degree wide 
    ** angle lens. It has the properties desired to do
    ** reflection mapping. This saves us the trouble of
    ** having to warp several images on cube faces onto 
    ** a sphere.
    */
    image = (unsigned long *) longimagedata("/usr/demos/data/textures/flowers.rgb");
    texdef2d(1,4,128,128,image,1,texps);
    tevdef(1,1,tevps);
    texbind(TX_TEXTURE_0,1);
    tevbind(TV_ENV0,1);
    texgen(TX_S, TG_SPHEREMAP, NULL);
    texgen(TX_T, TG_SPHEREMAP, NULL);
    texgen(TX_S, TG_ON, NULL);
    texgen(TX_T, TG_ON, NULL);
}

main ()
{
    short attached;
    short value;
    long dev;

    attached = 1;
    initialize();


    while (TRUE)
    {
        while (qtest() || !attached)
        {
            dev = qread (&value);
            switch (dev) {
             case ESCKEY:
                exit(0);
             case REDRAW:
                reshapeviewport();
                break;
             case AKEY:
                if(value) {
                  glass = 1 - glass;
                  blend();
                }
                break;
             case GKEY:
                if(value) gold = 1 - gold;
                break;
             case INPUTCHANGE:
                attached = value;        
                break;
            }
        }   
        drawscene();
    }   
}   /*  end main()  */

/*        The initialize subroutine positions the window and specifies
 *        its future constraints.  The program is in double buffer
 *        and RGB mode.  The torus is initialized with parameters
 *        for number of sides around and radius across its cross section
 *        and number of sides around and radius across its top.
 */ 

initialize()
{
    int gid1;

    if(!getgdesc(GD_TEXTURE)){
        printf("This only works on VGX machines/n");
        exit(0);
    }
    minsize(500,500);
    keepaspect(1,1);
    gid1 = winopen ("reflections");

    doublebuffer();
    RGBmode();
    overlay(2);
    gconfig();
    zbuffer(TRUE);
    subpixel(TRUE);
    drawmode(OVERDRAW);
    mapcolor(1, 255,255,255);
    drawmode(NORMALDRAW);

    inittorus ();

    qdevice (ESCKEY);
    qdevice (AKEY);
    qdevice (DKEY);
    qdevice (GKEY);
    qenter (REDRAW, gid1);

    mmode(MVIEWING);
    perspective(450, 1.0, 0.1, 100.0);
    loadmatrix(ident4);
    polarview(5.0, 0., 0., 0.);

    inittex(); /* define all the texture stuff! */

}

/* The current matrix is saved to calculate the 
** transformed normal directions. 
*/

drawscene()
{
    Matrix mat;

    czclear(0,getgdesc(GD_ZMAX));
    if(gold) 
        cpack(0xc5057fcc);
    else
	cpack(0x7fffffff);
    pushmatrix();
        rot(angle * 2.0, 'z');
        rot(angle * 2.0, 'y');
        getmatrix(mat);
        drawtorus(mat);
        pushmatrix();
        rot(90.0, 'x');
        getmatrix(mat);
        drawtorus(mat);
        popmatrix();
    popmatrix();
    swapbuffers();
    angle++;
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

        /*  go around cross section        */
    for (i = 0; i < numc; i = i + 1)
    {                
      fi = (float) i;
          /*  go around top view                */
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

/* Each normal for each vertex needs to be
** check for transformed direction. Texture
** assignment is then based on the x and y 
** components of the transformed normal.
** Note also that these components range from
** -1.0 to 1.0. Since texture space ranges from
** 0.0 to 1.0, the normal direction must be 
** mapped to this range as well.
*/

drawtorus(mat)
Matrix mat;
{
    int i, j;
    float tn[3];


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

