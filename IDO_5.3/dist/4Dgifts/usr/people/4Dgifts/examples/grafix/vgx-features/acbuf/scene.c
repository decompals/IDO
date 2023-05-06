/*			scene.c
 *	
 *Description: "scene" demonstrates how to use the accumulation buffer
 *to overcome the "jaggies" on polygons and lines. It is crucial to
 *turn subpixel positioning on for accurate placement of vertices. The
 *image is then rendered several times with the positioning of the image 
 *shifted by portions of a pixel. The subroutines "subpixwindow" and 
 *"subpixperspective" do this. 
 *To see the effects of aliasing, press the LEFTMOUSE button. To get
 *"smooth" edges again, release the button. An interesting thing to
 *do while running the program is to look at the polygon edges and
 *lines with the /usr/sbin/mag or /usr/demos/bin/snoop program to see 
 *how edges have color intensity variations to achieve the "smooth" 
 *look.
 *
 *				  Martin R. McDonald
 *				  SGI
 *				  JULY 1990
 *Disclaimer:
 * "Zusammengestohlen aus Vershiedenem diesem und jenem."
 *					Ludwig van Beethoven
*/

#include "gl.h"
#include "device.h"
#include "math.h"
#include "stdio.h"


#define jitters 10

int mouse,oldmouse = 0;

main ()
{
    short attached;
    short value;
    int dev;
    static Angle fovy = 650;
    static float aspect = 1., 
	      near = 1.1, far =  2000.0;

    attached = 1;
    initialize(fovy,aspect,near,far);

    while (TRUE)
    {
	while (qtest() || !attached)
	{
	    dev = qread (&value);
	    switch(dev)
	    {
	    case  ESCKEY:
		exit(0);
	    case REDRAW:
		reshapeviewport();
		acbuf(AC_CLEAR, 0.);
		break;
	    case INPUTCHANGE:
		attached = value;	
		break;
	    } 
	}   
	smoothit(fovy,aspect,near,far);
    }   
}   


initialize(fovy,aspect,near,far)
Angle fovy;
float aspect,near,far;
{
    int gid1;
    char  answer;


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

    gid1 = winopen ("smooth scene");

    doublebuffer();
    RGBmode();
    acsize(16);  
    gconfig();
    lsetdepth(0, 0x7FFFFF);
    zbuffer(TRUE);
    subpixel(TRUE);

    qdevice (ESCKEY);
    qdevice (LEFTMOUSE);
    qdevice (MIDDLEMOUSE);
    qdevice (REDRAW);
    qdevice (INPUTCHANGE);
    qenter (REDRAW, gid1);

    perspective( fovy, aspect, near, far);

    acbuf(AC_CLEAR, 0.);   /* initialize buffer to 0 */
    srandom(getpid());     /* initialize randum number generator */

}

smoothit(fovy,aspect,near,far)
Angle fovy;
float aspect,near,far;
{
	float pixdx, pixdy;
	int i,angle;
        static long x = 0x7fffffff;
        float y;

	mouse = getvaluator(MOUSEY);
	angle = mouse * 5;

	/* no need to update the frame if the mouse hasn't moved*/

	if(mouse != oldmouse){
	 if(getbutton(LEFTMOUSE)) {
	   pushmatrix();
            translate(0.0, 0.0, -500.0);
            rotate(angle, 'z');
	    drawsquares();
	   popmatrix();
	 }
	 else {
	  acbuf(AC_CLEAR, 0.0);
	  for(i= 0; i<jitters; i++){
	    pushmatrix();

	      /* It is better to jitter the frame by random increments
	      ** than to march along rows or columns. The values to set
	      ** pixdx and pixdy will depend on what kind of filtering
	      ** is desired. random() returns positive integers so
	      ** it needs to be scaled down. Mapping to the range from
	      ** -1 to 1 will center the jitterings around original
	      ** location
	      */

	      pixdx = 2.*(random() / (float) x) - 1.; 
	      pixdy = 2.*(random() / (float) x) - 1.;
	      subpixperspective(fovy,aspect,near,far,pixdx,pixdy);
              translate(0.0, 0.0, -500.0);
              rotate(angle, 'z');
	      drawsquares();
	      acbuf(AC_ACCUMULATE,1.0);
	    popmatrix();
	  }
	  acbuf(AC_RETURN, 1.0/(jitters));
	  oldmouse = mouse;
	 }
	swapbuffers();
	}
}

/* subpixwindow and subpixperspective are taken out of the 
** Graphics Library Programmer's Guide page 15-39. Their use
** is to "jitter" the object by fractions of a pixel to overcome
** aliasing at polygon edges and onlines.
*/

subpixwindow(left,right,bottom,top,near,far,pixdx,pixdy)
float left,right,bottom,top,near,far,pixdx,pixdy;
{
        short vleft, vright,vbottom,vtop;
        float xwsize,ywsize,dx,dy;
        int xpixels,ypixels;

        getviewport(&vleft,&vright,&vbottom,&vtop);
        xpixels = vright - vleft + 1;
        ypixels = vtop - vbottom + 1;
        xwsize = right - left;
        ywsize = top - bottom;
        dx = -pixdx * xwsize / xpixels;
        dy = -pixdy * ywsize / ypixels;
        window(left+dx,right+dx,bottom+dy,top+dy,near,far);
}

subpixperspective(fovy,aspect,near,far,pixdx,pixdy)
Angle fovy;
float aspect,near,far,pixdx,pixdy;
{
        float fov2,left,right,bottom,top;
        fov2 = ((fovy*M_PI) / 1800) / 2.0;
        top = near / (fcos(fov2) / fsin(fov2));
        bottom = -top;
        right = top * aspect;
        left = -right;
        subpixwindow(left,right,bottom,top,near,far,pixdx,pixdy);
}

drawsquares()
{
    static float v0[3] = { -50.0, -50.0, 0. };
    static float v1[3] = {  50.0, -50.0, 0. };
    static float v2[3] = {  50.0,  50.0, 0. };
    static float v3[3] = { -50.0,  50.0, 0. };
    static float v4[3] = { -100.0,  -100.0, 0. };
    static float v5[3] = {  100.0,  100.0, 0. };

    cpack(0);
    clear();
    zclear();
      pushmatrix();
       translate(-150.0, -150.0, 0.0);
       cpack(0xff0000);
       bgnpolygon(); v3f(v0); v3f(v1); v3f(v2); v3f(v3); endpolygon();
      popmatrix();
      pushmatrix();
       translate(150.0, -150.0, 0.0);
       cpack(0x0000ff);
       bgnpolygon(); v3f(v0); v3f(v1); v3f(v2); v3f(v3); endpolygon();
      popmatrix();
      pushmatrix();
       translate(150.0, 150.0, 0.0);
       cpack(0x00ff00);
       bgnpolygon(); v3f(v0); v3f(v1); v3f(v2); v3f(v3); endpolygon();
      popmatrix();
      pushmatrix();
       translate(-150.0, 150.0, 0.0);
       cpack(0x00ffff);
       bgnpolygon(); v3f(v0); v3f(v1); v3f(v2); v3f(v3); endpolygon();
      popmatrix();
      cpack(0xffffff);
      bgnline(); v3f(v4); v3f(v5); endline();
      pushmatrix();
      rotate(900, 'z');
      bgnline(); v3f(v4); v3f(v5); endline();
      popmatrix();
}

