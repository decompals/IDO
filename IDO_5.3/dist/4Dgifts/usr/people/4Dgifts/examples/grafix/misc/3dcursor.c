/*
 *  3dcursor.c:
 *
 *    3dcursor demonstrates using MAPW(3G) to map the X/Y screen pixel 
 *  location of the cursor into object space. 
 *
 *    This program constantly reads the X and Y location of the cursor, 
 *  then maps those coordinates (via mapw) to a line in object space, 
 *  then draws the cursor object at some specific Z location along that
 *  line.
 *
 *    Initially Z is set to zero.  The user can change the Z location 
 *  with the up and down arrow keys.  The two points returned from mapw
 *  are used to define a line.  Then based on the current Z value, the 
 *  corresponding X and Y values are calculated.  For details see the 
 *  comments above the convert() routine.
 *      
 *    Vertical motion of the mouse moves the cursor object in Y, 
 *  horizontal motion moves the cursor object in X, the up arrow key 
 *  moves the cursor object back in Z, and the down arrow key moves
 *  the cursor object forward in Z.
 *
 *                                      Momi Furuya - 1990
 */

#include <gl/gl.h>
#include <gl/device.h>

#define VIEWOBJ 6
#define CUBERAD 5.0
#define CURSRAD 1.0
#define SOLID   1
#define WIRE    2

/* coordinates of the corners of a unit cube */
float v0[3] = {-1.0, -1.0, -1.0};
float v1[3] = {-1.0, -1.0,  1.0};
float v2[3] = {-1.0,  1.0,  1.0};
float v3[3] = {-1.0,  1.0, -1.0};
float v4[3] = { 1.0, -1.0, -1.0};
float v5[3] = { 1.0, -1.0,  1.0};
float v6[3] = { 1.0,  1.0,  1.0};
float v7[3] = { 1.0,  1.0, -1.0};

main()
{
        Coord       p1[3], p2[3],                  /* endpoints of the line */
                    zval, xval, yval;              /* cursor coordinates    */
        long        dev, x, y, i,
                    trackinz;           /* if TRUE, vertical motion means Z */
        short       val;
        int         curstyle;
        Screencoord sx, sy;                        /* cursor position in 2D */


        prefposition(300, 900, 200, 800);
        winopen("3-Dimensional Cursor");
        qdevice(DOWNARROWKEY);
        qdevice(UPARROWKEY);
        qdevice(SKEY);
        qdevice(WKEY);
        qdevice(ESCKEY);
        zbuffer(TRUE);
        doublebuffer();
        gconfig();
        zclear();
        for (i=50; i<=200; i++) {        /* set up colormap for depthcueing */
                mapcolor(3000+i, i, 0, i);
        }
        lshaderange(3050, 3250, 0x0, 0x7fffff);
        linewidth(2);
        perspective(90, 1.0, 100, 200);            /* set up viewing volume */
        lookat(0.0, 0.0, 150.0, 0.0, 0.0, 0.0, 0); /* position viewing vol. */

              /*  Create an object describing the view */
        makeobj(VIEWOBJ);
                perspective(90, 1.0, 100, 200);
                lookat(0.0, 0.0, 150.0, 0.0, 0.0, 0.0, 0);
        closeobj();
        callobj(VIEWOBJ);

        getorigin(&x, &y);
        zval = 0.0;                     /* start with Z at 0 along the line */
        curstyle = WIRE;

        while (TRUE) {
                while(qtest()) {
                        dev = qread(&val);
                        switch (dev) {
                                case ESCKEY:
                                    exit(0);
                                case REDRAW:
                                    reshapeviewport();
                                    break;
                                case UPARROWKEY:
                                    zval -= 0.25;
                                    break;
                                case DOWNARROWKEY:
                                    zval += 0.25;
                                    break;
                                case SKEY:
                                    curstyle = SOLID;
                                    break;
                                case WKEY:
                                    curstyle = WIRE;
                                    break;
                        }
                }
                sx = getvaluator(MOUSEX) - x;    /* get mouse coordinates   */ 
                sy = getvaluator(MOUSEY) - y;    /*  relative to the window */

                mapw(VIEWOBJ, sx, sy, &p1[0], &p1[1], &p1[2], 
				      &p2[0], &p2[1], &p2[2]);
                convert(p1, p2, zval, &xval, &yval);
                drawscene(xval, yval, zval, curstyle);
        }
}




        
/*
 *  convert() receives two endpoint of a line (p1, and p2), a Z value 
 *  (zval), and pointers to X an Y values (xval and yval).  
 *  Representing the two points as 
 *
 *             p1 = (x1, y1, z1) 
 *  and 
 *             p2 = (x2, y2, z2),
 *
 *  we express the equation for the line they define as
 *
 *             p = p1 + t(p2 - p1),
 *
 *  where 
 *             p = (x3, y3, z3) 
 *  and 
 *             t = ((z3 - z1) / (z2 - z1)).  
 *
 *  Therefore from this we get
 *
 *             x3 = x1 + t(x2 - x1) 
 *  and 
 *             y3 = y1 + t(y2 - y1).
 *
 *  So given two points on the line, we can derive the equation of the
 *  line.  Then given the desired Z value we can derive t, and from t 
 *  and the X and Y values from the 2 points we can derive X and Y that
 *  correspond to the given Z value.
 */
convert(p1, p2, zval, xval, yval)
float p1[3], p2[3], zval, *xval, *yval;
{
        float t;

        t = ((zval - p1[2]) / (p2[2] - p1[2]));
        *xval = p1[0] + (t * (p2[0] - p1[0]));
        *yval = p1[1] + (t * (p2[1] - p1[1]));
}





/*
 *  drawscene() draws the scene by clearing the background, rotating, 
 *  scaling, and drawing the purple cube, then rotating, scaling, and
 *  translating the blue cube (attached to the cursor) to the current 
 *  cursor location, and drawing it.  After that it prints out the 
 *  values for the X, Y, and Z.  If curstyle is SOLID, a solid blue 
 *  cube is drawn.  If curstyle is WIRE, a depthcued purple wireframe
 *  cube is drawn.
 */
drawscene(x, y, z, curstyle)
float x, y, z;
int curstyle;
{
        char str[20];

        color(BLACK);
        clear();
        zclear();

        pushmatrix();
        scale(CUBERAD, CUBERAD, CUBERAD);
        rot(300.0, 'y');
        rot(300.0, 'z');
        fillpurplecube();
        popmatrix();

        pushmatrix();
        scale(CURSRAD, CURSRAD, CURSRAD);
        translate(x, y, z);
        rot(-100.0, 'y');
        rot(-160.0, 'z');
        if (curstyle == SOLID) {
                fillbluecube();
        }
        else if (curstyle == WIRE) {
                depthcue(TRUE);
                drawcube();
                depthcue(FALSE);
        }
        popmatrix();

        color(173);
        cmov(-11.0, -11.0, 0.0);
        charstr("z = ");
        cmov(-9.5, -11.0, 0.0);
        sprintf(str, "%f", z);
        charstr(str);
        cmov(-11.0, -10.5, 0.0);
        charstr("y = ");
        cmov(-9.5, -10.5, 0.0);
        sprintf(str, "%f", y);
        charstr(str);
        cmov(-11.0, -10.0, 0.0);
        charstr("x = ");
        cmov(-9.5, -10.0, 0.0);
        sprintf(str, "%f", x);
        charstr(str);

        swapbuffers();
}





drawcube()
{
        color(311);
        bgnclosedline();
           v3f(v0); v3f(v3); v3f(v2); v3f(v1);
        endclosedline();
        bgnclosedline();
           v3f(v0); v3f(v1); v3f(v5); v3f(v4);
        endclosedline();
        bgnclosedline();
           v3f(v4); v3f(v5); v3f(v6); v3f(v7);
        endclosedline();
        bgnclosedline();
           v3f(v3); v3f(v7); v3f(v6); v3f(v2);
        endclosedline();
        bgnclosedline();
           v3f(v5); v3f(v1); v3f(v2); v3f(v6);
        endclosedline();
        bgnclosedline();
           v3f(v0); v3f(v4); v3f(v7); v3f(v3);
        endclosedline();
}





fillbluecube()
{
        color(188);
        bgnpolygon();
           v3f(v0); v3f(v3); v3f(v2); v3f(v1);
        endpolygon();
        color(2);
        bgnpolygon();
           v3f(v0); v3f(v1); v3f(v5); v3f(v4);
        endpolygon();
        color(3);
        bgnpolygon();
           v3f(v4); v3f(v5); v3f(v6); v3f(v7);
        endpolygon();
        color(186);
        bgnpolygon();
           v3f(v3); v3f(v7); v3f(v6); v3f(v2);
        endpolygon();
        color(5);
        bgnpolygon();
           v3f(v5); v3f(v1); v3f(v2); v3f(v6);
        endpolygon();
        color(187);
        bgnpolygon();
           v3f(v0); v3f(v4); v3f(v7); v3f(v3);
        endpolygon();
}





fillpurplecube()
{
        color(192);
        bgnpolygon();
           v3f(v0); v3f(v3); v3f(v2); v3f(v1);
        endpolygon();
        color(193);
        bgnpolygon();
           v3f(v0); v3f(v1); v3f(v5); v3f(v4);
        endpolygon();
        color(194);
        bgnpolygon();
           v3f(v4); v3f(v5); v3f(v6); v3f(v7);
        endpolygon();
        color(195);
        bgnpolygon();
           v3f(v3); v3f(v7); v3f(v6); v3f(v2);
        endpolygon();
        color(196);
        bgnpolygon();
           v3f(v5); v3f(v1); v3f(v2); v3f(v6);
        endpolygon();
        color(197);
        bgnpolygon();
           v3f(v0); v3f(v4); v3f(v7); v3f(v3);
        endpolygon();
}
