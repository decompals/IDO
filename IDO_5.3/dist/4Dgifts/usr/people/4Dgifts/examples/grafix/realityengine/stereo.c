/***********************************************************************
*                                                                      *
*                 Stereo In Window Sample Program                      *
*                                                                      *
* WARNING: YOU MUST RUN  960x680_108s  VOF TO USE STEREO IN WINDOW     *
*          WILL ONLY WORK ON REALITY ENGINE                            *
*                                                                      *
* To put your system into the correct video format, performe these     *
* commands, preferably from a terminal other than the console.         *
* 								       *
* 1) Become root.                                                      *
* 2) /usr/gfx/setmon 960x680_108s                                      *
* 3) /usr/gfx/stopgfx                                                  *
* 4) /usr/gfx/startgfx                                                 *
*                                                                      *
***********************************************************************/
#include <stdio.h>
#include <gl/gl.h>

#define XSIZE 640.0
#define YSIZE 512.0

static float ang = 0.0;

static float v[8][3] = { -1.0,-1.0,-1.0,
                          1.0,-1.0,-1.0,
                          1.0, 1.0,-1.0,
                         -1.0, 1.0,-1.0,
                         -1.0,-1.0, 1.0,
                          1.0,-1.0, 1.0,
                          1.0, 1.0, 1.0,
                         -1.0, 1.0, 1.0,
};

static unsigned long c[6] = { 
			  0x990000,
                          0x009900,
                          0x990099,
                          0x999900,
                          0x000099,
                          0x009999,
};

static void draw_scene(long);
static void draw_polyg(float *,float *,float *,float *,unsigned long);

main()
{
   foreground();
   prefposition(0,XSIZE-1,0,YSIZE-1);
   winopen("Stereo In Window");

   RGBmode();
   doublebuffer();
   stereobuffer();
   gconfig();

   /* Check to see if the requested features were granted. */

   if (!getgconfig(GC_DOUBLE) || !getgconfig(GC_STEREO)) {
     fprintf(stderr,"ERROR: Could not configure doublebuffer stereo!\n");
     exit(0);
     }

   subpixel(TRUE);
   zbuffer(TRUE);
   lsetdepth(getgdesc(GD_ZMIN),getgdesc(GD_ZMAX));

   mmode(MPROJECTION);
   perspective(450,XSIZE/YSIZE,0.1,100.0);
   mmode(MVIEWING);

   /* The scene rendering needs to toggle between the right and left
   ** eyes.
   */
   while (TRUE)  {
     leftbuffer(TRUE);
     rightbuffer(FALSE);
     draw_scene(0);

     leftbuffer(FALSE);
     rightbuffer(TRUE);
     draw_scene(1);

     swapbuffers();
     }
}

static void draw_scene(long flag)
{
   czclear(0,getgdesc(GD_ZMAX));

   pushmatrix();

   /* left/right eye */
   if (flag == 0)
        lookat(-.15,0.0,5.0,0.0,0.0,0.0,0);
   else lookat(0.15,0.0,5.0,0.0,0.0,0.0,0);

   ang+=2.0;
   rotate(ang,'y');
   rotate(ang/2.7,'z');

   cpack(0xffffffff);
   draw_polyg(v[0],v[1],v[2],v[3],c[0]);
   draw_polyg(v[4],v[5],v[6],v[7],c[1]);
   draw_polyg(v[0],v[1],v[5],v[4],c[2]);
   draw_polyg(v[1],v[2],v[6],v[5],c[3]);
   draw_polyg(v[2],v[3],v[7],v[6],c[4]);
   draw_polyg(v[3],v[0],v[4],v[7],c[5]);

   popmatrix();
}

static void draw_polyg(float *v0,float *v1,float *v2,float *v3,unsigned long c)
{
   cpack(c);
   bgnpolygon();
   v3f(v0);
   v3f(v1);
   v3f(v2);
/*
   Use this vertex to make a solid cube.
   The triangles look "cooler" .

   v3f(v3);
*/
   endpolygon();
}

