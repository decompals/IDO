/* Spaceball_fly.c -- basic fly-around demo using Spaceball
 *
 * Copyright (C) 1990 Spatial Systems Inc.
 *
 * RESTRICTIONS
 *
 *	The following code is licensed for use and redistribution provided
 *	the code is used exclusively in conjunction with a Spaceball.
 *	No portion may be used for other purposes without prior written
 *	permission from Spatial Systems Inc.  This notice must accompany
 *	any full or partial copies.
 *
 * Author:	Jim Wick
 *
 */


#include <stdio.h>
#include <math.h>
#include <gl.h>
#include <device.h>
#include <gl/spaceball.h>

#define SKYLINE_SCALE 400.0

/* MACROS */
#define MIN(a,b) ((a)<(b)?(a):(b))
#define MAX(a,b) ((a)>(b)?(a):(b))
#define IDENTITY(m) memcpy(m,imat,sizeof(Matrix))
#define MATCPY(d,s) memcpy(d,s,sizeof(Matrix))

/* COMMON TYPES */
typedef struct VERT_st
         {
         float x,y,z,nx,ny,nz,r,g,b;
         struct VERT_st *pprevv,*pnextv;
         }
      VERT_t;

typedef struct POLY_st
         {
         VERT_t *v;
         struct POLY_st *pprevp,*pnextp;
         }
      POLY_t;

typedef struct OBJ_st
         {
         POLY_t *p;
         struct OBJ_st *pprevo,*pnexto;
         }
      OBJ_t;


/* GLOBAL DATA */

/* Data holding globals */
#define NUM_LANDS               50   /* thats rows & columns of land patchs */
#define NUM_BUILDINGS           (NUM_LANDS-1) /* rows & columns of buildings */
#define NUM_ROADS               (NUM_BUILDINGS+1)

OBJ_t       *buildings[NUM_BUILDINGS][NUM_BUILDINGS],
            *land[NUM_LANDS][NUM_LANDS],
            *roads[NUM_ROADS][2],
	    *bead=NULL;

float	    building_dist_scale;


/* Misc globals */
Angle fov=600;
float aspect=1.0,
      land_minx=  10e20,
      land_maxx= -10e20,
      land_minz=  10e20,
      land_maxz= -10e20;
Coord near=1.0,
      far =3000.0;
int   Draw_roads=TRUE,
      Num_lands=10,
      Num_buildings=4,
      Num_roads=5;


Matrix  imat=
          {
             {1.,0.,0.,0.},
             {0.,1.,0.,0.},
             {0.,0.,1.,0.},
             {0.,0.,0.,1.}
          },
        world_to_eye=
          {
             {1.,0.,0.,0.},
             {0.,1.,0.,0.},
             {0.,0.,1.,0.},
             {-100.,-100.,-800.,1.} /* !! */
          },
        eye_to_world=
          {
             {1.,0.,0.,0.},
             {0.,1.,0.,0.},
             {0.,0.,1.,0.},
             {100.,100.,800.,1.} /* !! */
          };

#define BUILDINGMATERIAL 1
#define LANDMATERIAL     2
#define ROADMATERIAL     3
float buildingmaterial[]={SPECULAR,.5,.5,.5,
                       DIFFUSE,.57,.57,.57,
		       AMBIENT,.57,.57,.57,
                       SHININESS,30.0,
                       LMNULL},
      roadmaterial[]={ AMBIENT,.57,.57,.57,
                       LMNULL},
      landmaterial[]={DIFFUSE,0.,1.,0.,
                      AMBIENT,0.,1.,0.,
                      LMNULL},
      light2[]={LCOLOR,.477,.477,.477,
                POSITION,-600.,300.,600.,1.,
                LMNULL},
      light3[]={LCOLOR,.177,.177,.177,
                POSITION,-600.,300.,-600.,1.,
                LMNULL};

/*---------------------------------------------------------------------------*/
main(argc,argv)
int argc;
char **argv;
/*
   -demonstrates the use of spaceball in flying around a scene

-----------------------------------------------------------------------------*/
{
   init_graphics();
   define_scene();
   flyaround();

} /* end of main */



/*--------------------------------------------------------------------------*/
init_graphics()
/*
-----------------------------------------------------------------------------*/
{
   winopen("Spaceball-Fly");

   RGBmode();
   doublebuffer();
   gconfig();

   zbuffer(TRUE);
   zfunction(ZF_LESS);
   glcompat(GLC_ZRANGEMAP,1);
   lsetdepth((long)0,(long)0x7fffff);

   /* clear window so it is happening while we are waiting for events */
   RGBcolor(0x7f,0xb2,0xff);
   clear();
   zclear();

   backface(TRUE);
   mmode(MPROJECTION);
   perspective(fov,aspect,near,far);
   mmode(MVIEWING);

   lmdef(DEFMATERIAL,BUILDINGMATERIAL,15,buildingmaterial);
   lmdef(DEFMATERIAL,LANDMATERIAL,9,landmaterial);
   lmdef(DEFMATERIAL,ROADMATERIAL,5,roadmaterial);
   lmdef(DEFLIGHT,1,0,NULL);
   lmdef(DEFLIGHT,2,10,light2);
   lmdef(DEFLIGHT,3,10,light3);
   lmdef(DEFLMODEL,1,0,NULL);

   lmbind(LMODEL,1);

   /* queue devices */
   qdevice(SBTX);      qdevice(SBTY);        qdevice(SBTZ);
   qdevice(SBRX);      qdevice(SBRY);        qdevice(SBRZ); 
   qdevice(SBPERIOD);
   qdevice(SBBUT1);    qdevice(SBBUT2);      qdevice(SBBUT3);
   qdevice(SBBUT4);    qdevice(SBBUT5);      qdevice(SBBUT6);
   qdevice(SBBUT7);    qdevice(SBBUT8);      qdevice(SBPICK);
   qdevice(ESCKEY);

} /* end of init_graphics */


/*--------------------------------------------------------------------------*/
OBJ_t *OBJ_Cre_ppp(sx,sy,sz)
float sx,sy,sz;
/*
   Builds a parallelapiped at the origin.

   args:   sx,sy,sz   (r/o)   -the size of the parallelapiped (the corner
                         opposite the origin)
   returns:               -ptr to OBJ_t just created

-----------------------------------------------------------------------------*/
{
   POLY_t *pp;
   OBJ_t  *po;
   VERT_t *pv;

   /* malloc new object */
   po=(OBJ_t *)malloc(sizeof(OBJ_t));
   po->pprevo=po->pnexto=NULL;

   /* malloc polygons and vertices */
#  define DEF_VPOS(pv,sx,sy,sz)  { pv->x=(sx);   pv->y=(sy);   pv->z=(sz); }
#  define DEF_VCOL(pv,pr,pg,pb)  { pv->r=(pr);   pv->g=(pg);   pv->b=(pb); }
#  define DEF_VNORM(pv,px,py,pz) { pv->nx=(px);  pv->ny=(py);  pv->nz=(pz);}
#  define NEXT_VERTEX(p1,p2,p3,p4,p5,p6,p7,p8,p9) \
   {\
   pv->pnextv=(VERT_t *)malloc(sizeof(VERT_t));\
   pv->pnextv->pprevv=pv;\
   pv=pv->pnextv;\
   DEF_VPOS(pv,p1,p2,p3);\
   DEF_VCOL(pv,p4,p5,p6);\
   DEF_VNORM(pv,p7,p8,p9);\
   pv->pnextv=NULL;\
   }

   /* back polygon */
   pp=po->p=(POLY_t *)malloc(sizeof(POLY_t)); pp->pprevp=NULL;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,0.,0.,0.);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,0.,0.,-1.); 

   NEXT_VERTEX(0.,sy,0.,.5,.5,.5,0.,0.,-1.);
   NEXT_VERTEX(sx,sy,0.,.5,.5,.5,0.,0.,-1.);
   NEXT_VERTEX(sx,0.,0.,.5,.5,.5,0.,0.,-1.);

   /* rightside polygon */
   pp->pnextp=(POLY_t *)malloc(sizeof(POLY_t));
   pp->pnextp->pprevp=pp->pnextp; pp=pp->pnextp;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,sx,0.,0.);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,1.,0.,0.); 

   NEXT_VERTEX(sx,sy,0.,.5,.5,.5,1.,0.,0.);
   NEXT_VERTEX(sx,sy,sz,.5,.5,.5,1.,0.,0.);
   NEXT_VERTEX(sx,0.,sz,.5,.5,.5,1.,0.,0.);

   /* front polygon */
   pp->pnextp=(POLY_t *)malloc(sizeof(POLY_t));
   pp->pnextp->pprevp=pp->pnextp; pp=pp->pnextp;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,sx,0.,sz);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,0.,0.,1.); 

   NEXT_VERTEX(sx,sy,sz,.5,.5,.5,0.,0.,1.);
   NEXT_VERTEX(0.,sy,sz,.5,.5,.5,0.,0.,1.);
   NEXT_VERTEX(0.,0.,sz,.5,.5,.5,0.,0.,1.);

   /* leftside polygon */
   pp->pnextp=(POLY_t *)malloc(sizeof(POLY_t));
   pp->pnextp->pprevp=pp->pnextp; pp=pp->pnextp;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,0.,0.,sz);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,-1.,0.,0.); 

   NEXT_VERTEX(0.,sy,sz,.5,.5,.5,-1.,0.,0.);
   NEXT_VERTEX(0.,sy,0.,.5,.5,.5,-1.,0.,0.);
   NEXT_VERTEX(0.,0.,0.,.5,.5,.5,-1.,0.,0.);

   /* bottom polygon */
   pp->pnextp=(POLY_t *)malloc(sizeof(POLY_t));
   pp->pnextp->pprevp=pp->pnextp; pp=pp->pnextp;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,0.,0.,0.);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,0.,-1.,0.); 

   NEXT_VERTEX(sx,0.,0.,.5,.5,.5,0.,-1.,0.);
   NEXT_VERTEX(sx,0.,sz,.5,.5,.5,0.,-1.,0.);
   NEXT_VERTEX(0.,0.,sz,.5,.5,.5,0.,-1.,0.);

   /* top polygon */
   pp->pnextp=(POLY_t *)malloc(sizeof(POLY_t));
   pp->pnextp->pprevp=pp->pnextp; pp=pp->pnextp;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,0.,sy,0.);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,0.,1.,0.); 

   NEXT_VERTEX(0.,sy,sz,.5,.5,.5,0.,1.,0.);
   NEXT_VERTEX(sx,sy,sz,.5,.5,.5,0.,1.,0.);
   NEXT_VERTEX(sx,sy,0.,.5,.5,.5,0.,1.,0.);

   pp->pnextp=NULL;

   return po;
} /* end of OBJ_Cre_ppp */

/*---------------------------------------------------------------------------*/
OBJ_t *OBJ_Cre_flat(x1,y1,z1,x2,y2,z2)
float x1,y1,z1,x2,y2,z2;
/*
** NOTE THIS IS NOT CORRECT YET--IT ONLY DOES POLYGONS IN XZ,Y1 PLANE **
-----------------------------------------------------------------------------*/
{
   OBJ_t *po;
   POLY_t *pp;
   VERT_t *pv;

   /* malloc new object */
   po=(OBJ_t *)malloc(sizeof(OBJ_t));
   po->pprevo=po->pnexto=NULL;

   /* malloc polygons and vertices */
#  define DEF_VPOS(pv,sx,sy,sz)  { pv->x=(sx);   pv->y=(sy);   pv->z=(sz); }
#  define DEF_VCOL(pv,pr,pg,pb)  { pv->r=(pr);   pv->g=(pg);   pv->b=(pb); }
#  define DEF_VNORM(pv,px,py,pz) { pv->nx=(px);  pv->ny=(py);  pv->nz=(pz);}
#  define NEXT_VERTEX(p1,p2,p3,p4,p5,p6,p7,p8,p9) \
   {\
   pv->pnextv=(VERT_t *)malloc(sizeof(VERT_t));\
   pv->pnextv->pprevv=pv;\
   pv=pv->pnextv;\
   DEF_VPOS(pv,p1,p2,p3);\
   DEF_VCOL(pv,p4,p5,p6);\
   DEF_VNORM(pv,p7,p8,p9);\
   pv->pnextv=NULL;\
   }

   /* polygon */
   pp=po->p=(POLY_t *)malloc(sizeof(POLY_t)); pp->pprevp=NULL;
   pv=pp->v=(VERT_t *)malloc(sizeof(VERT_t)); pv->pprevv=NULL;
   DEF_VPOS(pv,x1,y1,z1);
   DEF_VCOL(pv,.5,.5,.5); 
   DEF_VNORM(pv,0.,1.,0.); 

   NEXT_VERTEX(x1,y1,z2,.5,.5,.5,0.,1.,0.);
   NEXT_VERTEX(x2,y1,z2,.5,.5,.5,0.,1.,0.);
   NEXT_VERTEX(x2,y1,z1,.5,.5,.5,0.,1.,0.);

   pp->pnextp=NULL;
   return po;
} /* end of OBJ_Cre_flat */



/*----------------------------------------------------------------------*/
static obj_Flat_poly_normal(pp)
POLY_t *pp;
/*
This function calculates flat shaded normals for the passed in
polygon.  I.e. it gives the same normal to all the vertices.
It assumes there are at least 3 vertices in the vertex list and that
the first three are non-colinear.

	args:   pp (r/o)   -ptr to a polygon (vertex list)
	returns:
		nothing
------------------------------------------------------------------------*/
{
    float       ax,ay,az,bx,by,bz,nx,ny,nz,nlen;
    VERT_t      *pv,
		*pv1=pp->v,
		*pv2=pp->v->pnextv,
		*pv3=pp->v->pnextv->pnextv;

    ax=pv2->x-pv1->x;   ay=pv2->y-pv1->y;   az=pv2->z-pv1->z;
    bx=pv2->x-pv3->x;   by=pv2->y-pv3->y;   bz=pv2->z-pv3->z;

    nx=ay*bz-az*by;     ny=az*bx-ax*bz;     nz=ax*by-ay*bx;
    nlen=sqrt(nx*nx+ny*ny+nz*nz);
    nx/=nlen;           ny/=nlen;           nz/=nlen;

    for(pv=pv1;pv;pv=pv->pnextv)
	{
	pv->nx=nx; pv->ny=ny; pv->nz=nz;
	}

} /* end of obj_Flat_poly_normal */


/*---------------------------------------------------------------------------*/
define_scene()
/*
   -build green land polygons
   -build grid of roads
   -build random height buildings in city (parks are height < 0)
-----------------------------------------------------------------------------*/
{
   float height,random_height();
   int i,j;
   int xturn;
   long tloc;
   POLY_t *pp;
   VERT_t *pv;

   /* define land */
   for(i=0;i<Num_lands;i++)
      for(j=0;j<Num_lands;j++)
         land[i][j]=OBJ_Cre_flat(
                (i-(Num_lands/2)  )*100.,0.,(j-(Num_lands/2)  )*100.,
                (i-(Num_lands/2-1))*100.,0.,(j-(Num_lands/2-1))*100.);
   land_minx= (0-Num_lands/2-1)*100.;
   land_maxx= (((Num_lands-1)-Num_lands/2)+1)*100.;
   land_minz= (0-Num_lands/2-1)*100.;
   land_maxz= (((Num_lands-1)-Num_lands/2)+1)*100.;

   /* define buildings */
   height=1000000000./time(&tloc);
   xturn=0;
   for(i=0;i<Num_buildings;i++)
      for(j=0;j<Num_buildings;j++)
         if ((height=random_height(height))>0.0)
            {
            buildings[i][j]=OBJ_Cre_ppp(
                80.*(xturn?(1-height):1.0),
                SKYLINE_SCALE*height,
                80.*((!xturn)?height:1.0));
            xturn=1-xturn;
            OBJ_MoveR(buildings[i][j],
               (i-(Num_buildings/2))*100.+10,0.0,(j-(Num_buildings/2))*100.+10);
            }

   /* define roads */
   /* could have a patterned fat line for the stripe in the center of street */
   {
   int odd,fudge;
   float lengthbeg,lengthend,widthbeg,widthend;
   odd=((Num_roads/2)*2!=Num_roads);
   fudge=odd?0:1;
   lengthbeg=((-Num_roads/2)+fudge)*100.;
   lengthend=(( Num_roads/2)      )*100.;
   for(i=0;i<(Num_roads+1);i++) /* experienced same <= problem here */
      {
      widthbeg= (i-(Num_roads/2)+fudge)*100.-10.,
      widthend= (i-(Num_roads/2)+fudge)*100.+10.;
      /* road along x */
      roads[i][0]=OBJ_Cre_flat(lengthbeg,.1,widthbeg,lengthend,.1,widthend );
      /* road along z */
      roads[i][1]=OBJ_Cre_flat(widthbeg ,.2,lengthbeg,widthend,.2,lengthend);
      }
   }

} /* end of define_scene */



/*---------------------------------------------------------------------------*/
static float random_height(seed)
float seed;
/*
-----------------------------------------------------------------------------*/
{
   char s[50];
   float r;
   while (seed<0.0) seed+=0.5;
   while (seed>1.0) seed/=10.;
   sprintf(s,"%.45f",sqrt((double)seed));
   s[3]='.';
   s[10]=0;
   sscanf(s+3,"%f",&r);
   return r-0.3;
}


/*---------------------------------------------------------------------------*/
draw_frame(redraw)
int redraw;
/*
   ...
-----------------------------------------------------------------------------*/
{
   OBJ_t *po;
   POLY_t *pp,*pp1;
   VERT_t *pv;
   int i,j;

   /* if we got a redraw event sometime after last call to draw_frame */
   if (redraw)
   {
      RGBcolor(0x7f,0xb2,0xff);
      clear();
      zclear();
   }
   loadmatrix(world_to_eye);

   lmbind(LIGHT1,2);
   lmbind(LIGHT2,3);

   /* draw land */
   lmbind(MATERIAL,LANDMATERIAL);
   backface(FALSE);
   for(i=0;i<Num_lands;i++)
      for(j=0;j<Num_lands;j++)
         for(pp=land[i][j]->p;pp;pp=pp->pnextp)
            {
            bgnpolygon();
            for(pv=pp->v;pv;pv=pv->pnextv)
               n3f(&(pv->nx)),v3f(&(pv->x));
            endpolygon();
            }
   backface(TRUE);

   /* draw roads */
   if (Draw_roads)
      {
      backface(FALSE);
      lmbind(MATERIAL,ROADMATERIAL);
      for(i=0;i<Num_roads;i++)
         for(pp=roads[i][0]->p,pp1=roads[i][1]->p;
   	  pp&&pp1;
   	  pp=pp->pnextp,pp1=pp1->pnextp)
            {
            bgnpolygon();
            for(pv=pp->v;pv;pv=pv->pnextv)
               n3f(&(pv->nx)),v3f(&(pv->x));
            endpolygon();
            bgnpolygon();
            for(pv=pp1->v;pv;pv=pv->pnextv)
               n3f(&(pv->nx)),v3f(&(pv->x));
            endpolygon();
            }
      backface(TRUE);
      }

   /* draw buildings */
   lmbind(MATERIAL,BUILDINGMATERIAL);
   for(i=0;i<Num_buildings;i++)
      for(j=0;j<Num_buildings;j++)
         if (buildings[i][j])
            for(pp=buildings[i][j]->p;pp;pp=pp->pnextp)
               {
               bgnpolygon();
               for(pv=pp->v;pv;pv=pv->pnextv)
                  n3f(&(pv->nx)),v3f(&(pv->x));
               endpolygon();
               }

   swapbuffers();

   /* clear window so it is happening while we are waiting for events */
   RGBcolor(0x7f,0xb2,0xff);
   clear();
   zclear();

} /* end of draw_frame */


/*---------------------------------------------------------------------------*/
flyaround()
/*
-----------------------------------------------------------------------------*/
{
   long dev;
   short data;
   short sbtx,sbty,sbtz,sbrx,sbry,sbrz,sbperiod;
   Matrix rot;
   int    redisplay=TRUE;
   int    redraw = FALSE;

   while(1)
      {
      /* Read all events from the queue before redisplaying anything  */
      while(qtest())
         {
         dev=qread(&data);
	 switch(dev)
	    {
	    case REDRAW:   reshapeviewport();  redraw = TRUE; redisplay=TRUE; break;
	    case ESCKEY:   if (!data) exit(0); break;
	    case SBTX: 	sbtx=data; 	    break;
	    case SBTY: 	sbty=data; 	    break;
	    case SBTZ: 	sbtz=data; 	    break;
	    case SBRX: 	sbrx=data; 	    break;
	    case SBRY: 	sbry=data; 	    break;
	    case SBRZ: 	sbrz=data; 	    break;
	    case SBPERIOD:	sbperiod=data;

	       /* rotations */
	       rotarbaxis(sbperiod*3e-8,(Coord)sbrx,(Coord)sbry,(Coord)-sbrz,rot);
	       MAT_Mult44x44(world_to_eye,world_to_eye,rot);

	       /* translations */
	       /* Use distance from origin to approximate distance from 
		  buildings as a scale factor. The function needs tuning.
	       */
	       building_dist_scale=1.0+flog(
		   eye_to_world[3][0]*eye_to_world[3][0]+
		   eye_to_world[3][1]*eye_to_world[3][1]+
		   eye_to_world[3][2]*eye_to_world[3][2]);
	       world_to_eye[3][0]-=sbtx*sbperiod*4e-7*building_dist_scale;
	       world_to_eye[3][1]-=sbty*sbperiod*4e-7*building_dist_scale;
	       world_to_eye[3][2]+=sbtz*sbperiod*1e-6*building_dist_scale;

	       /* Invert world_to_eye */
	       MAT_Invert(eye_to_world,world_to_eye);

	       /* Check to see if we have tried to drive through ground */
	       if (eye_to_world[3][1]<1.0) 
		  {
		  eye_to_world[3][1]=1.0;
		  MAT_Invert(world_to_eye,eye_to_world); /* reinvert */
		  }

               redisplay=TRUE;
	       break;
	    case SBPICK:
	       if (!data) break; /* only respond to key press */
	       IDENTITY(world_to_eye);
	       world_to_eye[3][0]= -100.0;
	       world_to_eye[3][1]= -100.0;
	       world_to_eye[3][2]= -800.0;
	       redisplay=TRUE;
	       break;
	    } /* end of switch */
         } /* end of while(qtest) */

      /* ONLY now that all the events have been read, redisplay */
      if (redisplay)
	 {
	 sbprompt();
	 draw_frame(redraw);
	 redisplay=FALSE;
	 redraw=FALSE;
	 }
      } /* end of while (forever) */
   
} /* end of flyaround */




/*---------------------------------------------------------------------------*/
OBJ_MoveR(po,dx,dy,dz)
OBJ_t *po;
float dx,dy,dz;
/*
        Moves an object a Relative distance dx,dy,dz.  Changes all the 
	coordinates in the object.

        args: po        (r/o)   -ptr to object to translate
              dx,dy,dz  (r/o)   -amount of relative movement in modelling space

        returns:
              nothing
-----------------------------------------------------------------------------*/
{
    POLY_t *pp;
    VERT_t *pv;

    for(pp=po->p;pp;pp=pp->pnextp)
       for(pv=pp->v;pv;pv=pv->pnextv)
          pv->x+=dx,pv->y+=dy,pv->z+=dz;

} /* end of OBJ_MoveR */


/*---------------------------------------------------------------------------*/
MAT_Mult44x44(c,a,b)
Matrix c,a,b;
/*
-----------------------------------------------------------------------------*/
{
   int i,j;
   Matrix t;
   for(i=0;i<4;i++)
      for(j=0;j<4;j++)
         t[i][j]=a[i][0]*b[0][j]+
                 a[i][1]*b[1][j]+
                 a[i][2]*b[2][j]+
                 a[i][3]*b[3][j];

   for(i=0;i<4;i++)
      for(j=0;j<4;j++)
         c[i][j]=t[i][j];

} /* end of MAT_mult44x44 */



/*---------------------------------------------------------------------------*/
MAT_Invert(d,s)
Matrix d,s;
/*
	Inverts a 4x4 affine xformation matrix
-----------------------------------------------------------------------------*/
{
   d[0][0]=s[0][0]; d[0][1]=s[1][0]; d[0][2]=s[2][0]; d[0][3]=0.0;
   d[1][0]=s[0][1]; d[1][1]=s[1][1]; d[1][2]=s[2][1]; d[1][3]=0.0;
   d[2][0]=s[0][2]; d[2][1]=s[1][2]; d[2][2]=s[2][2]; d[2][3]=0.0;
   d[3][0]= -s[3][0]*s[0][0]-s[3][1]*s[0][1]-s[3][2]*s[0][2];
   d[3][1]= -s[3][0]*s[1][0]-s[3][1]*s[1][1]-s[3][2]*s[1][2];
   d[3][2]= -s[3][0]*s[2][0]-s[3][1]*s[2][1]-s[3][2]*s[2][2];
} /* end of MAT_Invert */


