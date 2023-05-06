/*                
 *                              texture.c
 *
 *    "texture" allows a user to see the difference between automatic 
 *  texture coordinate assignment parameters.  The user may pick LINEAR 
 *  or CONTOUR in either direction. The user also has a chance to see 
 *  the effects of lighting on a textured surface as well as alpha 
 *  blending. It is of worth to note that the definition of the 
 *  contouring and linear planes are affected by the state of the 
 *  transformation matrix.  In this case both planes are defined with 
 *  only a translate in the z direction on the view stack.
 *
 *                               Martin R. McDonald -- SGI --  JULY 1990
 */

#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

Matrix ident4 = {1.0, 0.0, 0.0, 0.0,
                 0.0, 1.0, 0.0, 0.0,
                 0.0, 0.0, 1.0, 0.0,
                 0.0, 0.0, 0.0, 1.0};

float material[] = { ALPHA, .75, LMNULL };

/* use two sided lighting */
float two[] = { TWOSIDE, 1, LMNULL };

/* Tell the graphics library what parameters
** to us for lighting.
*/

def_simple_light_calc(){
        lmdef(DEFMATERIAL, 1, 3, material);
        lmdef(DEFLIGHT, 1, 0, NULL);
        lmdef(DEFLMODEL, 1, 3, two);
}

use_simple_light_calc(){
        lmbind(MATERIAL,1);
        lmbind(LIGHT0, 1);
        lmbind(LMODEL, 1);
}

/* Parameters for texturing.
*/
float texps[] = {TX_MINFILTER,TX_MIPMAP_BILINEAR,
                 TX_MAGFILTER,TX_BILINEAR,
                 TX_NULL};
float tevps[] = {TV_NULL};

float params[4] = { 0., 1., 0., 4.};
float params2[4] = { 1., 0., 0., 4.};
float params3[4] = { 1., -1., 0., 4.};

int     xangle = 0;              /* counter for angle rotation */
int     yangle = 0;              /* counter for angle rotation */
int     zangle = 0;              /* counter for angle rotation */
int        X=1,Y=1,Z=1;
unsigned long arr[128][128];
unsigned long *image;
float point[31][31][3];        
float norms[31][31][3];

inittex()
{

    int i,j;
    double theta;

    /* read in the "paper" image file. */

    image=(unsigned long *)longimagedata("/usr/demos/data/textures/paper.rgb");

    /* Create a "rainbow" of colors to use
    ** as a texture.
    */

    for(i=0;i<128;i++){
        theta = (double)((double)i * M_PI / (double)128);
        for(j=0;j<128;j++)
            arr[j][i]=(long)(
     ((long)((1+sin((2.*theta)-(M_PI_2)))/2 * 0xff000000) & 0xff000000) |
     ((long)((1+sin((2.*theta)-(M_PI_2)))/2 * 0xff0000) & 0xff0000) |
     ((long)((1+sin((2.*theta)+(-M_PI_2+2*M_PI/3)))/2 * 0x00d800) & 0x00ff00) |
     ((long)((1+sin((2.*theta)+(-M_PI_2+4*M_PI/3)))/2 * 0x0000ff) & 0x0000ff) );
    }

    /* these 2 verticle bars of black are used for "contouring" */
    for(i=0;i<128;i++){
        for(j=0;j<128;j++)
            if(!(j%32)) arr[j][i] = 0;
    }
    /* and these 2 horizontal bars of black are used for "contouring" */
    for(i=0;i<128;i++){
        for(j=0;j<128;j++)
            if(!(j%32)) arr[i][j] = 0;
    }
    texdef2d(1,4,128,128,image,1,texps);
    texdef2d(2,4,128,128,&arr[0][0],1,texps);
    tevdef(1,1,tevps);
    texbind(TX_TEXTURE_0,2);           /* start with the rainbow texture */
    tevbind(TV_ENV0,1);
    texgen(TX_T, TG_CONTOUR, params); /* use contouring in the T plane */
    texgen(TX_T, TG_ON, params);
    texgen(TX_S, TG_CONTOUR, params2); /* use contouring in the S plane */
    texgen(TX_S, TG_ON, params2);
}

setalpha(alpha)
int alpha;
{
    if(alpha){
        blendfunction(BF_ONE,BF_ONE);
	/* Disable z-buffering. We wouldn't expect to have transparent
	** surfaces clipped in the "real world".
	*/
	zbuffer(FALSE);			
    }
    else {
        blendfunction(BF_ONE,BF_ZERO);
	/* When transparent surfaces are rendered, we need to make sure
	** that zbuffering is active.
	*/
	zbuffer(TRUE);
    }
}

settext(texture)
int texture;
{
    /* bind previously defined texture, selected by user.  */

    texbind(TX_TEXTURE_0,texture);
}

setcontour(cont)
int cont;
{

    switch(cont){
        case 1:
            texgen(TX_T, TG_CONTOUR, params); 
            texgen(TX_T, TG_ON, params);
            break;
        case 2:
            texgen(TX_T, TG_LINEAR, params); 
            texgen(TX_T, TG_ON, params);
            break;
        case 3:
            texgen(TX_S, TG_CONTOUR, params2);
            texgen(TX_S, TG_ON, params2);
            break;
        case 4:
            texgen(TX_S, TG_LINEAR, params2);
            texgen(TX_S, TG_ON, params2);
            break;
    }
}

dolights(on)
int on;
{
    if(on) 
	lmbind(MATERIAL,1);
    else   
	lmbind(MATERIAL,0);
}

main ()
{
    short attached = 1;
    short value;
    int dev;
    int pupval,mainmenu,map,alpha,contour,lights;

    initialize();
    lights = defpup("Lights %t %F|On%x1|Off%x0",dolights);
    map = defpup("Textures %t %F|Paper%x1|Rainbow%x2",settext);
    alpha = defpup("Alpha %t %F|On%x1|Off%x0",setalpha);
    contour = defpup("Texgen %t %F|ContourT%x1|LinearT%x2\
                |ContourS%x3|LinearS%x4",setcontour);
    mainmenu = defpup("Menu%t|Map%m|Alpha%m|Contour%m|Lights%m",
                  map,alpha,contour,lights);

    def_simple_light_calc();         /* use light definitions set up */
    use_simple_light_calc();



    while (TRUE)
    {
        while (qtest() || !attached)
        {
            dev = qread (&value);
            switch(dev)
            {
                case RIGHTMOUSE:
                    pupval = dopup(mainmenu);
                    break;
                case XKEY: if(value) X = 1 - X; break;
                case YKEY: if(value) Y = 1 - Y; break;
                case ZKEY: if(value) Z = 1 - Z; break;
                case ESCKEY:
                    exit(0);
                    break;
                case REDRAW:
                    reshapeviewport();
                    drawscene();
                    break;
                case INPUTCHANGE:
                    attached = value;        
                    break;
            }
        }  
        drawscene();
    }   
}

/*        The initialize subroutine positions the window and specifies
 *        its future constraints.  The program is in double buffer
 *        and RGB mode.  The torus is initialized with parameters
 */ 

initialize()
{
    int gid1;
    float radc, radt;

    if(!getgdesc(GD_TEXTURE)){
        printf("This only works on VGX machines/n");
        exit(0);
    }
    keepaspect(1,1);
    gid1 = winopen ("texturing");

    doublebuffer();
    RGBmode();
    gconfig();
    zbuffer(TRUE);
    subpixel(TRUE);
    initsaddle();

    qdevice (ESCKEY);
    qdevice (XKEY);
    qdevice (YKEY);
    qdevice (ZKEY);
    qdevice (RIGHTMOUSE);

    mmode(MVIEWING);
    perspective(450, 1.0, 0.1, 100.0);
    loadmatrix(ident4);
    translate(0.0, 0.0, -5.0);

    inittex(); /* define all the texture stuff! */

}

drawscene()
{
    czclear(0,getgdesc(GD_ZMAX));
    cpack(0x80ffffff);
    pushmatrix();
        if(X) {rot(xangle * 2.0, 'x'); xangle++;}
        if(Y) {rot(yangle * 2.0, 'y'); yangle++;}
        if(Z) {rot(zangle * 2.0, 'z'); zangle++;}
        drawsaddle();
    popmatrix();
    swapbuffers();
}


/* Surface calculated is the "monkey-saddle",
** a hyperbolic paraboloid.
*/

initsaddle ( )
{
    int i, j;
    float x,y;

    for (j = 0; j < 21; j++)
    {
          y = (float)j / 10.0 - 1.0;
        for (i = 0; i < 21; i++)
        {
            x = (float)i / 10.0 - 1.0;
            point[i][j][0] = x;
            point[i][j][1] = y;
            point[i][j][2] = (x*x*x - 3*x*y*y)/2;
            norms[i][j][0] = (-3*x*x + 3*y*y)/2;
            norms[i][j][1] = 3*x*y;
            norms[i][j][2] = 1.0;
            normalize(norms[i][j]);
        }
    }
}

normalize(vect)
float vect[3];
{
    float length;

    length = vect[0]*vect[0] + vect[1]*vect[1] + vect[2]*vect[2];
    length = fsqrt(length);
    vect[0] /= length;
    vect[1] /= length;
    vect[2] /= length;
}



/* If lighting is disabled, the n3f commands are ignored.
** If lighting is enabled, the cpack command from drawscene
** will be overriden by the colors set by the lighting calculations.
** Final color will modulated by texturing.
*/

drawsaddle()
{
    int i, j;

        for (j = 0; j < 20; j++)
            for (i = 0; i < 20; i++) {
                bgnpolygon();
                    n3f(norms[i][j]);
                    v3f(point[i][j]);
                    n3f(norms[i+1][j]);
                    v3f(point[i+1][j]);
                    n3f(norms[i+1][j+1]);
                    v3f(point[i+1][j+1]);
                    n3f(norms[i][j+1]);
                    v3f(point[i][j+1]);
                endpolygon();
            }

}

