/* 
 *  spotlight.c
 *  
 *  The interface for spotlight is similar to that of attenuation.c
 *  The 1,2,4,5,7,8 keys on the number pad will alter the attenuation
 *  factors. The x,y,z keys in conjunction with the UP/DOWNARROWKEYS
 *  will change the spotlight direction. The c,f keys will similarly
 *  change the spotlight properites.
 *
 *						Martin R. McDonald
 *						SGI
 *						JULY 1990
 *
 */ 
#include <gl.h> 
#include <device.h>
#include <stdio.h> 
Matrix idmat = {1.0,0.0,0.0,0.0, 
                0.0,1.0,0.0,0.0, 
                0.0,0.0,1.0,0.0, 
                0.0,0.0,0.0,1.0};

float  green_material[] = {DIFFUSE,  0.0, 1.0, 0.0,
			   LMNULL};

float  local_white_light[] = {LCOLOR, 1.0, 1.0, 1.0,
		              POSITION, 0.0, 0.0, 1.0, 1.0,
			      SPOTDIRECTION, 1, 1, -1,
			      SPOTLIGHT, 90, 30,
		              LMNULL};

float  light_model[] = {AMBIENT, 0.0, 0.0, 0.0,
			LOCALVIEWER, 1.0,
			ATTENUATION, 1.0, 1.0,
			ATTENUATION2, 0.0, 
			LMNULL};

/*
** draw_plate draws a flat plate covering the
** range -1.0 <= x, y <= 1.0 and z = 0. using
** n^2 rectangles.  All the normal vectors are
** perpendicular to the plate.
*/

draw_plate(n)
long n;	
{
    long i, j;
    float p0[3], p1[3], p2[3], p3[3];
    float n0[3];

    n0[0] = n0[1] = 0.0; n0[2] = 1.0;
    p0[2] = p1[2] = p2[2] = p3[2] = 0.0;

    for (i = 0; i < n; i++) {
        p0[0] = p1[0] = -1.0 + 2.0*i/n;
        p2[0] = p3[0] = -1.0 + 2.0*(i+1)/n;
        for (j = 0; j < n; j++) {
            p0[1] = p3[1] = -1.0 + 2.0*j/n;
            p1[1] = p2[1] = -1.0 + 2.0*(j+1)/n;
            bgnpolygon();
            n3f(n0); v3f(p0);
            n3f(n0); v3f(p1);
            n3f(n0); v3f(p2);
            n3f(n0); v3f(p3);
            endpolygon();
        }
    }
}


/*
** Tell the Graphics Library to DEFINE a
** lighting calculation that accounts for
** diffuse and ambient reflection.  In
** addition, this lighting calculation
** includes a local light whose emitted
** light is attenuated as a function of
** distance from the object.
*/
def_light_calc()
{
    lmdef(DEFLMODEL, 1, 0, light_model);
    lmdef(DEFMATERIAL, 1, 0, green_material);
    lmdef(DEFLIGHT, 1, 0, local_white_light);
}

/*
** Tell the Graphics Library to USE the lighting
** calculation that we defined earlier.
*/
use_light_calc()
{
    lmbind(LMODEL, 1);
    lmbind(LIGHT1, 1);
    lmbind(MATERIAL, 1);
}

main()
{
    float dist;
    float falloff = 90.0;/*exponent for falloff rate*/
    float cone=30.0;     /*angle for cone width*/
    float X= 1.0;	 /*x coordinate of spot direction*/
    float Y= 1.0;	 /*y coordinate of spot direction*/
    float Z= -1.0;	 /*z coordinate of spot direction*/
    float k0=1.0; 	 /*constant attenuation factor*/
    float k1=0.0;	 /*linear attenuation factor*/
    float k2=0.0;  	 /*square attenuation factor*/
    float zero=0.00001;  /*not exactly "0" to handle .1 decrements*/
    long  device;
    short value;
    long flag = 1;
    char str[32];

    if(!getgdesc(GD_LIGHTING_TWOSIDE)) { /* test for machine capability */
      printf("You do not have two sided light capabilities");
      exit(0);
    }
    keepaspect(1, 1);
    winopen("spots");
    RGBmode();
    doublebuffer();
    gconfig();
    subpixel(TRUE);

    qdevice(PAD1);
    qdevice(PAD2);
    qdevice(PAD4);
    qdevice(PAD5);
    qdevice(PAD7);
    qdevice(PAD8);
    qdevice(XKEY);
    qdevice(YKEY);
    qdevice(ZKEY);
    qdevice(FKEY);
    qdevice(CKEY);
    qdevice(UPARROWKEY);
    qdevice(DOWNARROWKEY);
    qdevice(LEFTMOUSE);

    /*
    ** Use mmode() to set up projection and
    ** viewing matrices for lighting.
    */    
    mmode(MVIEWING);
    perspective(400, 1.0, 0.5, 10.0);
    loadmatrix(idmat);
    lookat(0.0,0.0,6.0,0.0,0.0,0.0,0);
    rotate(-600,'x');

    def_light_calc();
    use_light_calc();

    dist = 1.0;

    while (1) {
	if(qtest()){
	   device = qread(&value);
	   switch(device){
	      case XKEY: if(getbutton(UPARROWKEY)) X+= 0.3;
			 else if(getbutton(DOWNARROWKEY)) X-= 0.3;
		   break;
	      case YKEY: if(getbutton(UPARROWKEY)) Y+= 0.3;
			 else if(getbutton(DOWNARROWKEY)) Y-= 0.3;
		   break;
	      case ZKEY: if(getbutton(UPARROWKEY)) Z+= 0.3;
			 else if(getbutton(DOWNARROWKEY)) Z-= 0.3;
		   break;
	      case FKEY: if(getbutton(UPARROWKEY) && falloff<128.1) falloff++;
			 else if(getbutton(UPARROWKEY)) falloff = 180.0;
	                 else if(getbutton(DOWNARROWKEY) && falloff>0.1) falloff--;
			 else if(getbutton(DOWNARROWKEY)) falloff = 0.0;
		   break;
	      case CKEY: if(getbutton(UPARROWKEY) && cone<90.1) cone++;
			 else if(getbutton(UPARROWKEY)) cone = 180.0;
	                 else if(getbutton(DOWNARROWKEY) && cone>0.1) cone--;
			 else if(getbutton(DOWNARROWKEY)) cone = 0.0;
		   break;
	     /*
	     case RIGHTARROWKEY:
		  if (value) n ++; break;
	     case LEFTARROWKEY:
		  if (value && n>0) n --; break;
	     */
	     case PAD1:
		  if (value) k0 += .1; break;
	     case PAD2:
		  if (value && k0>zero) k0 -= .1; break;
	     case PAD4:
		  if (value) k1 += .1; break;
	     case PAD5:
		  if (value && k1>zero) k1 -= .1; break;
	     case PAD7:
		  if (value) k2 += .1; break;
	     case PAD8:
		  if (value && k2>zero) k2 -= .1; break;
	    }
	    local_white_light[10] = X;
	    local_white_light[11] = Y;
	    local_white_light[12] = Z;
	    local_white_light[14] = falloff;
	    local_white_light[15] = cone;
	    /*
            lmdef(DEFLMODEL, 1, 0, local_white_light);
	    */
	    light_model[7] = k0;
	    light_model[8] = k1;
	    light_model[10] = k2;
            lmdef(DEFLMODEL, 1, 0, light_model);
	}
	if (flag) {
	    dist += .01;
	    if (dist > 1.5) flag = 1 - flag;
	} else {
	    dist -= .01;
	    if (dist < 0.1) flag = 1 - flag;
	}
	pushmatrix();
	ortho2(-3.0,3.0,-3.0,3.0);
        loadmatrix(idmat);
	cpack(0x404040);
        clear();
	sprintf(str, "Light Distance: %1.2f", dist);
	cpack(0xffffff);
	cmov2(-2.5, -1.5);
	charstr(str);
	sprintf(str, "Constant Factor: %1.2f", k0);
	cmov2(-2.5, -1.65);
	charstr(str);
	sprintf(str, "Linear Factor: %1.2f", k1);
	cmov2(-2.5, -1.8);
	charstr(str);
	sprintf(str, "Square Factor: %1.2f", k2);
	cmov2(-2.5, -1.95);
	charstr(str);
	/*
	sprintf(str, "Number of Polygons: %d", n*n);
	cmov2(-2.5, -2.1);
	charstr(str);
	*/
	sprintf(str, "Spot Direction X: %1.2f", X);
	cmov2(0.0, -1.5);
	charstr(str);
	sprintf(str, "Spot Direction Y: %1.2f", Y);
	cmov2(0.0, -1.65);
	charstr(str);
	sprintf(str, "Spot Direction Z: %1.2f", Z);
	cmov2(0.0, -1.8);
	charstr(str);
	sprintf(str, "Spot Falloff Rate: %1.2f", falloff);
	cmov2(0.0, -1.95);
	charstr(str);
	sprintf(str, "Spot Cone Angle: %1.2f", cone);
	cmov2(0.0, -2.1);
	charstr(str);
	popmatrix();
        perspective(400, 1.0, 0.5, 10.0);
	pushmatrix();
	    /*
	    ** Change the position of the local light
	    ** by REDEFINING and REBINDING the light.
	    ** Repositioning the light changes the
	    ** illumination of the plate for two reasons:
	    ** 1) the affect of attenuation, and 
	    ** 2) the light direction vector from a
	    ** vertex on the plate to the repositioned
	    ** light source has changed.
	    */
	    local_white_light[7] = dist;
	    lmdef(DEFLIGHT, 1, 0, local_white_light);
	    lmbind(LIGHT1, 1);
            draw_plate(25);
        popmatrix();
	swapbuffers();
    }
}
