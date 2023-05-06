/*
 *              modeldata.c
 *
 *      defines and draws:
 *              torus
 *              two hemisphere, 
 *                      trimmed with a NURBS curve
 *                      trimmed with NURBS curves and PWL curves
 *              two sphere
 *                      created with two of the above hemispheres 
 *                      surface of revolution.
 *
 *                                  R. E. Chang - 1990
 */

#include "surf.h"

/*
 *   external NURBS parameter declarations/initializations 
 */
double bx[9] = {1., rad, 0.,-rad, -1., -rad, 0., rad, 1.};
double by[9] = {0., rad, 1.,rad, 0., -rad, -1., -rad, 0.};
double  w[9] = {1., rad, 1., rad,  1.,  rad, 1., rad, 1.};

double knots[3][12] =
	{{0.,0.,0.,1.,1.,2.,2.,3.,3.,4.,4.,4.},  /* knots for circle         */
         {0.,0.,0.,1.,1.,1.},                    /* knots for a Bezier patch */
	 {0.,0.,0.,1.,1.,2.,2.,2.}};             /* knots for a semicircle   */

int num_knots[3] = {12, 6, 8};          /* the number of knots for the above */

/*
 *   external Lighting declarations/initializations
 */
float LModel[]  =  {
        AMBIENT, .1, .11, .12,
        LOCALVIEWER, 0.0,
        ATTENUATION,1.0, 0.0,
        LMNULL,
};

float Material[] =  {
        SHININESS, 50,
        EMISSION, .0, .0, .0,
        AMBIENT, .2, .2, .2,
        SPECULAR, .8 ,.8, .8,
        DIFFUSE, 0.8, 0.0, 0.8,
        LMNULL
};

float Light1[] = {
        POSITION, 0.7071, 0.7071, 0.0, 0.0,     /*  infinite light */
        LCOLOR, 1.0, 1.0, 1.0,                  /*  white light    */
        AMBIENT, 0, 0, .5,
        LMNULL,
};

float Light2[] = {
        POSITION, -0.7071, 0.7071, 0.0, 0.0,    /*  infinite light */
        LCOLOR, 1.0, 1.0, 1.0,                  /*  white light    */
        AMBIENT, .5, 0, 0,
        LMNULL,
};

float Light3[] = {
        POSITION, 1.0, 0.0, 0.0, 0.0,           /*  infinite light */
        LCOLOR, 1.0, 1.0, 1.0,                  /*  white light    */
        AMBIENT, 0, 0, 0,
        LMNULL,
};

float Light4[] = {
        POSITION, 0.0, 0.0, 1.0, 0.0,           /*  infinite light */
        LCOLOR, 1.0, 1.0, 1.0,                  /*  white light    */
        AMBIENT, 0, 0, 0,
        LMNULL,
};




/*
 *                           define_torus
 *
 *      defines a torus with r as the radius of the revolving circle
 *      and R as the distance of its center from z, the axis of rotation.
 */
void define_torus() 
{
        int i,j;
        size_t = size_s = 9;

        for(j=0;j<size_t;j++)  {
            for(i=0;i<size_s;i++)  {
                cpts[i+j*size_s].x  =  (r * bx[j] + R * w[j]) * bx[i]; 
                cpts[i+j*size_s].y  =  (r * bx[j] + R * w[j]) * by[i];
                cpts[i+j*size_s].z  =   r * w[i] * by[j];
                cpts[i+j*size_s].w  =   w[i] * w[j];
            }
        }
}



/*
 *              define_hemisphere
 *
 *   defines a hemisphere of radius R with a single biquadratic Bezier patch
 */
void define_hemisphere() 
{
        size_t = 3;
        size_s = 3;

        cpts[0].x = cpts[3].x = cpts[6].x = -2.*R;
        cpts[1].x = cpts[4].x = cpts[7].x = 0.;
        cpts[2].x = cpts[5].x = cpts[8].x = 2.*R;

        cpts[0].y = cpts[1].y = cpts[2].y = -2.*R;
        cpts[3].y = cpts[4].y = cpts[5].y = 0.;
        cpts[6].y = cpts[7].y = cpts[8].y = 2.*R;

        cpts[0].z = cpts[2].z = cpts[6].z = cpts[8].z = R;
        cpts[1].z = cpts[3].z = cpts[5].z = cpts[7].z = -R;
        cpts[4].z = -3.*R;

        cpts[0].w = cpts[2].w = cpts[6].w = cpts[8].w = 3.;
        cpts[1].w = cpts[3].w = cpts[5].w = cpts[7].w = 1.;
        cpts[4].w =  -1.;
/*
 *              Note that there is a negative weight!  It's OK--
 *              Define the northern hemisphere by interchanging the
 *              x and y components (so normals point outward) and
 *              multiplying the z components by -1.
 *
 *              The same trimming curve is used.
 */
        if(model==3)  {  
            cpts2[0].x = cpts2[1].x = cpts2[2].x = -2.*R;
            cpts2[3].x = cpts2[4].x = cpts2[5].x = 0.;
            cpts2[6].x = cpts2[7].x = cpts2[8].x = 2.*R;

            cpts2[0].y = cpts2[3].y = cpts2[6].y = -2.*R;
            cpts2[1].y = cpts2[4].y = cpts2[7].y = 0.;
            cpts2[2].y = cpts2[5].y = cpts2[8].y = 2.*R;

            cpts2[0].z = cpts2[2].z = cpts2[6].z = cpts2[8].z = -R;
            cpts2[1].z = cpts2[3].z = cpts2[5].z = cpts2[7].z =  R;
            cpts2[4].z =  3.*R;

            cpts2[0].w = cpts2[2].w = cpts2[6].w = cpts2[8].w = 3.;
            cpts2[1].w = cpts2[3].w = cpts2[5].w = cpts2[7].w = 1.;
            cpts2[4].w =  -1.;
        }


}



/*
 *                              trim_hemis
 *
 *              Trims the NURBS patch to a hemisphere.
 *              It is done by using the standard circle defined above
 *              by bx,by and w.  The circle is translated so that the
 *              center is at (.5,.5).
 */
void trim_hemis() 
{
        int i;
        float h, theta;
/*
 *    define the circle as a NURBS curve in the surface domain
 */
        for(i=0;i<9;i++)  {
            tpts[i].s = .5*bx[i] + .5 * w[i];
            tpts[i].t = .5*by[i] + .5 * w[i];
            tpts[i].w =  w[i];
        }
}



/*
 *                              trim_hole
 *
 *              Trims a circular hole in hemis1, a NURBS patch.
 *              It is done by using the standard circle defined above
 *              by bx,by and w.  The circle is translated so that the
 *              center is at (.3,.5) and the radius is .1.
 */
void trim_hole() 
{
        int i;
        float h, theta;
/*
 *    define the circle as a NURBS curve in the surface domain.  Note the
 *    the orientation of the control points is opposite to the trim_hemis curve
 */
        for(i=0;i<9;i++)  {
            trimhole[8-i].s = .1*bx[i] + .3 * w[i];
            trimhole[8-i].t = .1*by[i] + .5 * w[i];
            trimhole[8-i].w =  w[i];
        }
}



/*
 *                          trim_hemis_pieces
 *
 *              Trims the NURBS patch to a hemisphere.
 *              A sequence of 4 trimming curves is used:
 *                      1st and 3rd quarters as NURBS curve
 *                      2nd and 4th quarters as PWL curves
 */
void trim_hemis_pieces() 
{
        int i;
        double h, theta;
/*
 *              define two quarters as Bezier curves in the surface domain
 */
        for(i=0;i<3;i++)  {
            tcpts[0][i].s = .5*bx[i] + .5 * w[i];
            tcpts[0][i].t = .5*by[i] + .5 * w[i];
            tcpts[0][i].w =  w[i];
            tcpts[1][i].s = .5*bx[i+4] + .5 * w[i+4];
            tcpts[1][i].t = .5*by[i+4] + .5 * w[i+4];
            tcpts[1][i].w =  w[i+4];
        }
/*
 *              evaluate two quarters at sample points each
 */
        h = M_PI/(2.*(sample-1.));
        for(i=0;i<sample;i++)  {
            theta = i * h;
            cir[0][i].s = .5 * cos(theta+.5*M_PI) + .5; 
            cir[0][i].t = .5 * sin(theta+.5*M_PI) + .5;
            cir[1][i].s = 1.-cir[0][i].s; 
            cir[1][i].t = 1.-cir[0][i].t;
        }
}



/*
 *              define_SR_sphere
 *
 *   defines the surface of revolution used for the Sphere 2 model
 */
void define_SR_sphere() 
{
        int i,j,k;
        size_t = 5; 
        size_s = 9;
                
        for(k=0;k<size_t;k++)  {
            j = (6+k)%8;      /*  to define a semicircle (6->12 o'clock),
                                  from a circle defined from 3-3 o'clock */
            for(i=0;i<size_s;i++)  {
                cpts[i+k*size_s].x  =  R * bx[j] * bx[i]; 
                cpts[i+k*size_s].y  =  R * bx[j] * by[i];
                cpts[i+k*size_s].z  =  R * w[i] * by[j];
                cpts[i+k*size_s].w  =  w[i] * w[j];
            }
        }

}



/*
 *                                draw_net
 *
 *      draw the control net of the model
 *      
 *      NOTE:  Although the control points are defined by the 4 homogeneous
 *      points (x ,y ,z ,w), v4d is not used to draw the control net since
 *      there is the possibility of a negative weight.  Attempts to connect
 *      a homogeneous point with a negative weight with a homogeneous point
 *      with a positive weight will result in drawing a line going through
 *      infinity.  In the hemisphere example, the central weight is negative.
 *      We therefore perform the perspective division on the control points
 *      and use v3f to draw the control net.  See accompanying notes for the
 *      discussion on why this is appropriate. 
 */
void draw_net() 
{
        int i, j;
        float pt[3];
        cpack(0xff0000);
/*
 *      draw s lines
 */
        for(j=0;j<size_t;j++)  {
            bgnline();
            for(i=0;i<size_s;i++)  {  
                pt[0] = cpts[i+j*size_s].x / cpts[i+j*size_s].w;
                pt[1] = cpts[i+j*size_s].y / cpts[i+j*size_s].w;
                pt[2] = cpts[i+j*size_s].z / cpts[i+j*size_s].w;
                v3f(pt);
            }
                endline();
        }        
/*
 *       draw t lines
 */
        cpack(0x0000ff);
        for(i=0;i<size_s;i++)  {
            bgnline();
            for(j=0;j<size_t;j++) { 
                pt[0] = cpts[i+j*size_s].x / cpts[i+j*size_s].w;
                pt[1] = cpts[i+j*size_s].y / cpts[i+j*size_s].w;
                pt[2] = cpts[i+j*size_s].z / cpts[i+j*size_s].w;
                v3f(pt);
            }
            endline();
        }

        if (model==3)  {        
            cpack(0xff0000);
/*
 *       draw s lines
 */
            for(j=0;j<size_t;j++)  {
                bgnline();
                for(i=0;i<size_s;i++)  {  
                    pt[0] = cpts2[i+j*size_s].x / cpts2[i+j*size_s].w;
                    pt[1] = cpts2[i+j*size_s].y / cpts2[i+j*size_s].w;
                    pt[2] = cpts2[i+j*size_s].z / cpts2[i+j*size_s].w;
                    v3f(pt);
                }
                endline();
            }        
/*
 *      draw t lines
 */
            cpack(0x0000ff);
            for(i=0;i<size_s;i++)  {
                bgnline();
                for(j=0;j<size_t;j++) { 
                    pt[0] = cpts2[i+j*size_s].x / cpts2[i+j*size_s].w;
                    pt[1] = cpts2[i+j*size_s].y / cpts2[i+j*size_s].w;
                    pt[2] = cpts2[i+j*size_s].z / cpts2[i+j*size_s].w;
                    v3f(pt);
                }
                endline();
            }        
        }       /* end if (model==3) */
} 



/*
 *                            NURBS_quadric
 *
 *   Draw various quadric surfaces as NURBS surfaces.  Two trimming 
 *   options for hemisphere which were defined in trim_hemis are provided.  
 */
void NURBS_quadric() 
{

    switch (model)  {

        case 0:         /* torus   */
            bgnsurface();
            nurbssurface(num_knots[0],knots[0],num_knots[0],knots[0],
                                 sizeof(Vector4d), sizeof(Vector4d) * size_s,
                                 (double *)cpts, order,order,N_V3DR);
            endsurface();
            break;

        case 1:        /* hemisphere trimmed with a NURBS curve */
            bgnsurface();
            nurbssurface(num_knots[1],knots[1],num_knots[1],knots[1],
                         sizeof(Vector4d) * size_t, sizeof(Vector4d),
                         (double *)cpts, order,order,N_V3DR);

            if(trimming)  {
		bgntrim();
                nurbscurve(num_knots[0],knots[0],sizeof(Vector3d),
                           (double *)tpts,order,N_P2DR);
                endtrim();
                bgntrim();
                nurbscurve(num_knots[0],knots[0],sizeof(Vector3d),
                           (double *)trimhole,order,N_P2DR);
                endtrim();
	    }

            endsurface();
            break;

	case 2:        /* hemisphere trimmed with NURBS&PWL curves  */
	    bgnsurface();
            nurbssurface(num_knots[1],knots[1],num_knots[1],knots[1],
                         sizeof(Vector4d) * size_t, sizeof(Vector4d),
                         (double *)cpts, order,order,N_V3DR);

            bgntrim(); /*  circle in 4 quarters  */
            nurbscurve(num_knots[1],knots[1],sizeof(Vector3d),
                       (double *)tcpts[0],order,N_P2DR);
            pwlcurve(sample,(double *)cir[0],sizeof(Vect2d),N_P2D);
            nurbscurve(num_knots[1],knots[1], sizeof(Vector3d),
                       (double *)tcpts[1],order,N_P2DR);
            pwlcurve(sample,(double *)cir[1],sizeof(Vect2d),N_P2D);

            endtrim();

            endsurface();
            break;

	case 3:        /* sphere composed of 2 hemis as in case 1 */
            bgnsurface();     /*  southern hemisphere   */
            nurbssurface(num_knots[1],knots[1],num_knots[1],knots[1],
                         sizeof(Vector4d) * size_t, sizeof(Vector4d),
                         (double *)cpts, order,order,N_V3DR);

            bgntrim(); /* trimmed with NURBS curve re: case 1*/
            nurbscurve(num_knots[0],knots[0],sizeof(Vector3d),
                       (double *)tpts,order,N_P2DR);
            endtrim();
            endsurface();

            bgnsurface();  /*  northern hemisphere  */
            nurbssurface(num_knots[0],knots[0],num_knots[0],knots[0],
                         sizeof(Vector4d) * size_t, sizeof(Vector4d),
                         (double *)cpts2, order,order,N_V3DR);
            bgntrim();  /* trimmed as in case 2 */
            nurbscurve(num_knots[1],knots[1],sizeof(Vector3d),
                       (double *)tcpts[0],order,N_P2DR);
            pwlcurve(sample,(double *)cir[0],sizeof(Vect2d),N_P2D);
            nurbscurve(num_knots[1],knots[1], sizeof(Vector3d),
                       (double *)tcpts[1],order,N_P2DR);
            pwlcurve(sample,(double *)cir[1],sizeof(Vect2d),N_P2D);
            endtrim();

            endsurface();
            break;

            case 4:         /* surface of revolution sphere */
                bgnsurface();
                nurbssurface(num_knots[0],knots[0],num_knots[2],knots[2],
                                 sizeof(Vector4d), sizeof(Vector4d) * size_s,
                                 (double *)cpts, order,order,N_V3DR);
                endsurface();
                break;
        }
}


/*
 *                      makeaxes
 *
 *              draws the x,y,z axes
 *
 */
void makeaxes() 
{
        cpack(0x000000);
        move(0.0, 0.0, 0.0);
        draw(5.0, 0.0, 0.0);
        move(0.0, 5.0, 0.0);
        draw(0.0, 0.0, 0.0);
        draw(0.0, 0.0, 5.0);
        cmov(5.0, 0.0, 0.0);
        charstr("x");
        cmov(0.0, 5.0, 0.0);
        charstr("y");
        cmov(0.0, 0.0, 5.0);
        charstr("z");
}



/*
 *              set_props
 *
 *           update any NURBS parameters that have changed
 */
void set_props() 
{
        setnurbsproperty( N_PIXEL_TOLERANCE, pix_tol);
        setnurbsproperty( N_ERRORCHECKING, 1.0);
        setnurbsproperty( N_DISPLAY, display_mode);
        setnurbsproperty( N_CULLING, 0.0);
}



/*
 *              def_light_calc
 *
 *     define lighting characteristics for the model, material and lights used
 */
def_light_calc()
{
        lmdef(DEFLMODEL, 1, 0, LModel);
        lmdef(DEFMATERIAL, 1, 0, Material);
        lmdef(DEFLIGHT, 1, 0, Light1);
        lmdef(DEFLIGHT, 2, 0, Light2);
        lmdef(DEFLIGHT, 3, 0, Light3);
        lmdef(DEFLIGHT, 4, 0, Light4);
}



/*
 *              use_light_calc
 *
 *     update light bindings for the current model, lights and material
 */
use_light_calc()
{
        lmbind(LMODEL, 1);
        lmbind(LIGHT0, 1);
        lmbind(LIGHT1, 2);
        lmbind(LIGHT3, 3);
        lmbind(MATERIAL, 1);
}
