/*
 *   upat-main.c:
 *
 *    this program demonstrates an alternative approach to LOOKAT(3G) where,
 *  instead of a viewing the scene with the default up-axis being Y, the 
 *  user can easily specify the up-axis in whatever orientation she desires.
 *
 *  NOTE:  the core routine itself--upat--exists separately in the upat.c
 *         file.  this is so that it can be employed more easily as a
 *         stand-alone module.
 *
 *                                 Thant Tessman - 1990
 *                     
 */

#include <gl/gl.h>
#include <gl/device.h>

extern void upat(float vx, float vy, float vz,
		 float px, float py, float pz,
		 float ux, float uy, float uz);

main() {

    int dev;
    short val;
    float i, j, k;


    initialize();

    /* first flyby */

    printf("\n\n    Y is up.  (Just like lookat.)\n");
    j = 2.0;
    for (i = -8.0, k=12.0; i<12.0; i+=0.05, k-=0.05) {
	color(BLACK); 
	clear();
	perspective(400, 5.0/4.0, 1.0, 50.0);
	upat(   i,   j,   k,
	      0.0, 0.0, 0.0,
	      0.0, 1.0, 0.0);
	draw_axes();
	swapbuffers();
    }


    /* second flyby */

    printf("    Z is up, but same path.\n");
    j = 2.0;
    for (i = -8.0, k=12.0; i<12.0; i+=0.05, k-=0.05) {
	color(BLACK); 
	clear();
	perspective(400, 5.0/4.0, 1.0, 50.0);
	upat(   i,   j,   k,
	      0.0, 0.0, 0.0,
	      0.0, 0.0, 1.0);
	draw_axes();
	swapbuffers();
    }


    /* third flyby */

    printf("    (1, 1, 1) is up.  Different path.\n");
    for (i = -8.0, j=10.0, k=12.0; i<12.0; i+=0.05, k-=0.05, j-=0.05) {
	color(BLACK); 
	clear();
	perspective(400, 5.0/4.0, 1.0, 50.0);
	upat(   i,   j,   k,
	      0.0, 0.0, 0.0,
	      1.0, 1.0, 1.0);
	draw_axes();
	swapbuffers();
    }
}


initialize() {

    keepaspect(5, 4);
    winopen("");

    doublebuffer();
    gconfig();

    qdevice(ESCKEY);
}


draw_axes() {

    color(RED);
    move(0.0, 0.0, 0.0);
    draw(1.0, 0.0, 0.0);

    cmov(1.2, 0.0, 0.0);
    charstr("x");

    color(GREEN);
    move(0.0, 0.0, 0.0);
    draw(0.0, 1.0, 0.0);

    cmov(0.0, 1.2, 0.0);
    charstr("y");

    color(BLUE);
    move(0.0, 0.0, 0.0);
    draw(0.0, 0.0, 1.0);

    cmov(0.0, 0.0, 1.2);
    charstr("z");

    color(WHITE);
    move(0.5, 0.5, 0.5);
    draw(0.5, 0.5, 0.0);

    move(0.5, 0.0, 0.0);
    draw(0.5, 0.5, 0.0);
    draw(0.0, 0.5, 0.0);

    move(0.5, 0.5, 0.5);
    draw(0.0, 0.5, 0.5);

    move(0.0, 0.5, 0.0);
    draw(0.0, 0.5, 0.5);
    draw(0.0, 0.0, 0.5);

    move(0.5, 0.5, 0.5);
    draw(0.5, 0.0, 0.5);

    move(0.5, 0.0, 0.0);
    draw(0.5, 0.0, 0.5);
    draw(0.0, 0.0, 0.5);
}
