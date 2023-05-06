/*
 *      model   
 *              construct a torus
 *
 *              construct a hemisphere 
 *                  by trimming a sterographic projection of a sphere
 *      
 *              construct a sphere 
 *                  using two hemispheres surface of revolution
 *
 *                                 R. E. Chang - 1990
 */

#include "surf.h"
 
/*
 *  external object orientation & identity matrix declarations/initializations
 */
Matrix objmat = {
    { 0.521255, -0.256092,  0.814062,  0.000000},
    { 0.734636,  0.620074, -0.275329,  0.000000},
    {-0.434268,  0.741560,  0.511355,  0.000000},
    { 0.000009, -0.000004,  0.000028,  1.000000},
};

Matrix idmat = {
    {1.0, 0.0, 0.0, 0.0},
    {0.0, 1.0, 0.0, 0.0},
    {0.0, 0.0, 1.0, 0.0},
    {0.0, 0.0, 0.0, 1.0},
};



main() {

        int dev;
        short val;
        int redrawneeded=TRUE;

/*
 *  set up all the initalization states
 */
        initialize();

/*
 *  enter the main loop of get input/process input
 */

        while (TRUE) {

            if (redrawneeded) {
                draw_scene();
                redrawneeded=FALSE;
            }

            while (qtest() || !redrawneeded) switch(dev=qread(&val)) {

                case ESCKEY:
                    if (val) 
                        break;                     /* exit on up, not down */
                    exit(0);

                case REDRAW:
                    reshapeviewport();
                    redrawneeded=1;
                    break;

                case LEFTMOUSE:
                    if (val) {
                        omx = mx;
                        omy = my;
                        mode = ORIENT;
                    } else mode = NOTHING;
                    break;

                case MOUSEX:
                    omx = mx; 
                    mx = val;
                    if (mode == ORIENT) {
                        update_scene();
                        redrawneeded=TRUE;
                    }
                    break;

                case MOUSEY:
                    omy = my; 
                    my = val;
                    if (mode == ORIENT) {
                        update_scene();
                        redrawneeded=TRUE;
                    }
                    break;

                case UPARROWKEY:
                    if (val) {
                        zpos += 0.2;
                        redrawneeded=TRUE;
                    }
                    break;

                case DOWNARROWKEY:
                    if (val) {
                        zpos -= 0.2;
                        redrawneeded=TRUE;
                    }
                    break;

                case MENUBUTTON:
                    if (val) {
                        if (dopup(mainmenu)==10) exit(1);
                        build_displaymenu();
                        set_props();
                        redrawneeded=TRUE;
                    }
                    break;
            }
        }
}



void initialize() 
{
        long xscrnsize;     /* size of screen in x used to set prefposition */


/*
 *   initialize the model parameters
 */

        model = 0;               /* start out displaying model 0, the torus */
        r = 1.0;             /* r and R are minor and major radii of torus; */
        R = 3.0;             /* R is also used as the radius of the sphere  */
        trimming = 1;        /* start out with trimming set to on           */
        prev_sample = 0;
        sample = 10;         

/*
 *   initialize the NURBS parameters
 */
        order = 3;        /* order of the surface, all surfaces are quadric */

/*
 *   initialize the display parameters
 */
        cnet=0;                   /* toggles the display of the control net */
        mode=0;                 /* initially LEFTMOUSE is not being pressed 
                                    meaning the object is not being rotated */
        BF_cull=0;                      /* backface culling default is off  */
        display_mode=N_FILL;                       /*  NURBS display param  */
        prev_pix_tol=0.;
        pix_tol=50.;
        zpos = -9.0;           /* position of the eye--looking along x axis */

/*
 *   initialize the graphics subsystem 
 */
        keepaspect(1, 1);
        xscrnsize = getgdesc(GD_XPMAX);              /* get/set screen size */
        if (xscrnsize == 1280)
            prefposition(475,1275,190,990);
        else if (xscrnsize == 1024)
            prefposition(323,1023,67,767);
        else {
            printf("Something's EXTREMELY wrong:  xscrnsize=%d\n", xscrnsize);
            exit(-1) ;
        }
        winopen("nurbs model example");

        doublebuffer();
        RGBmode();
        gconfig();
        zbuffer(TRUE);

        mmode(MVIEWING);
        /* loadmatrix(idmat); As of 3.3, this is done automatically */
        /* See the mmode() manual page */

        ortho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
        cpack(0xff5050);
        def_light_calc();
        use_light_calc();

        qdevice(ESCKEY);                                   /* exits program */
        qdevice(LEFTMOUSE);              /* when pressed, rotates the model */
        qdevice(MOUSEX);                     /* used as input to update the */
        qdevice(MOUSEY);                     /* current state of rotation   */
        qdevice(UPARROWKEY);                /* used to translate toward (UP */
        qdevice(DOWNARROWKEY);              /* or away from (DOWN) viewer   */
        qdevice(MENUBUTTON);               


        modelmenu = newpup();
        addtopup( modelmenu, "model %t");
        addtopup( modelmenu, "torus %x0 %f",set_model);
        addtopup( modelmenu, "hemisphere 1 %x1 %f",set_model); 
        addtopup( modelmenu, "hemisphere 2 %x2 %f",set_model); 
        addtopup( modelmenu, "sphere 1 %x3 %f",set_model); 
        addtopup( modelmenu, "sphere 2 %x4 %f",set_model);

        build_displaymenu();
        set_props();
        define_torus();

}



/*
 *              update_scene
 *
 *      update_scene *if* LEFTMOUSE is currently being pressed.  if not,
 *      don't do anything
 */ 
void update_scene() 
{
        switch (mode) {

            case ORIENT:
                orient();
                break;
        }
}



/*
 *              orient
 *
 *      perform post-multiplication on the object's orientation matrix
 *
 *      orient "fools" the pre-multiply matrix system used in the graphics
 *      pipeline by saving the current viewing projection, then loading an
 *      identiy matrix onto the stack, multiplying into this identity matrix
 *      the new rotation angles in x and y, multiplying these newest 
 *      rotations into the object orientation matrix, then immediately 
 *      saving out the cummulatively altered new matrix (for the next time),
 *      and finally restoring the current viewing projection.
 */
void orient () 
{
        pushmatrix();

        loadmatrix(idmat);

        rotate(mx-omx, 'y');
        rotate(omy-my, 'x');

        multmatrix(objmat);
        getmatrix(objmat);

        popmatrix();
}



/*
 *              draw_scene
 *
 *      draw_scene draws the next frame of the current model being displayed
 */ 
void draw_scene() 
{
        RGBcolor(0, 0, 0);
        cpack(0xf0f0f0);
        clear();
        zclear();
        backface(BF_cull);

        perspective(400, 1.0, 5.0, 13.0);
        loadmatrix(idmat);
        translate(0.0, 0.0, zpos);
        multmatrix(objmat);
        scale(0.6, 0.6, 0.6);
        if (cnet) draw_net();
        NURBS_quadric();
        makeaxes();

        swapbuffers();
}



/*
 *              set_model
 *
 *      called when a different model is selected from the models menu
 */
int set_model(int n) 
{

        model = n;
        switch (model)  {
            case 0:         /* torus   */
                define_torus();
                break;
            case 1:         /* hemisphere trimmed with a NURBS curve */
                define_hemisphere();
                trim_hemis();
                trim_hole();
                break;
            case 2:         /* hemisphere trimmed with NURBS&PWL curves  */
                define_hemisphere();
                trim_hemis_pieces();
                break;
            case 3:         /* sphere composed of 2 hemis as in case 1 */
                define_hemisphere();
                trim_hemis();
                trim_hemis_pieces();
                break;
            case 4:         /* surface of revolution sphere */
                define_SR_sphere();
                break;
        }
        return -1;
}



/*
 *              set_display
 *
 *      called when one of the "display" menu items have been selected
 */
int set_display(n)
int n;
{
        switch(n)  {
            case 5:
                display_mode = N_FILL;
                break;
            case 6:
                display_mode = N_OUTLINE_POLY;
                break;
            case 7:
                display_mode = N_OUTLINE_PATCH;
                break;
            case 9:
                cnet = cnet ? 0 : 1;
                break;
            case 10:
                BF_cull = BF_cull ? 0 : 1;
                break;
            case 11:
                trimming = trimming ? 0 : 1;
                break;
        }
        return -1;
}



/*
 *              set_pixtol
 *
 *   called when one of the "pixel tolerance" menu items have been selected
 */
int set_pixtol(n)
int n;
{
        switch(n)  {
            case 1:
                pix_tol = 5;
                break;
            case 2:
                pix_tol = 10;
                break;
            case 3:
                pix_tol = 15;
                break;
            case 4:
                pix_tol = 20;
                break;
            case 5:
                pix_tol = 25;
                break;
            case 6:
                pix_tol = 30;
                break;
            case 7:
                pix_tol = 40;
                break;
            case 8:
                pix_tol = 50;
                break;
            case 9:
                pix_tol = 75;
                break;
            case 10:
                pix_tol = 100;
                break;
        }
        return -1;
}



/*
 *              set_trim_sample_rate
 *
 *   called when one of the "trim sample rate" menu items have been selected
 */
int set_trim_sample_rate(n)
int n;
{
        switch(n)  {
            case 1:
                sample = 3;
                trim_hemis_pieces();
                break;
            case 2:
                sample = 5;
                trim_hemis_pieces();
                break;
            case 3:
                sample = 10;
                trim_hemis_pieces();
                break;
            case 4:
                sample = 20;
                trim_hemis_pieces();
                break;
        }
        return -1;

}



/*
 *              build_displaymenu
 *
 *   called when any of the "display" menu items states needs to be updated
 */
void build_displaymenu() 
{

        if (prev_pix_tol != pix_tol) {

            pixelmenu = newpup();
            addtopup( pixelmenu, "pixel tolerance %t");

            if (((int) pix_tol) == 5) 
                addtopup( pixelmenu, ">  5 <%f",set_pixtol);
            else
                addtopup( pixelmenu, " 5 %f",set_pixtol);

            if (((int) pix_tol) == 10) 
                addtopup( pixelmenu, "> 10 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "10 %f",set_pixtol);

            if (((int) pix_tol) == 15) 
                addtopup( pixelmenu, "> 15 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "15 %f",set_pixtol);

            if (((int) pix_tol) == 20) 
                addtopup( pixelmenu, "> 20 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "20 %f",set_pixtol);

            if (((int) pix_tol) == 25) 
                addtopup( pixelmenu, "> 25 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "25 %f",set_pixtol);

            if (((int) pix_tol) == 30) 
                addtopup( pixelmenu, "> 30 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "30 %f",set_pixtol);

            if (((int) pix_tol) == 40) 
                addtopup( pixelmenu, "> 40 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "40 %f",set_pixtol);

            if (((int) pix_tol) == 50) 
                addtopup( pixelmenu, "> 50 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "50 %f",set_pixtol);

            if (((int) pix_tol) == 75) 
                addtopup( pixelmenu, "> 75 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "75 %f",set_pixtol);

            if (((int) pix_tol) == 100) 
                addtopup( pixelmenu, "> 100 <%f",set_pixtol);
            else
                addtopup( pixelmenu, "100 %f",set_pixtol);
        }

        if (prev_sample != sample) {
            sample_rate_menu = newpup();
            addtopup( sample_rate_menu, "trim sample rate %t");

            if (sample == 3) 
                addtopup( sample_rate_menu,">  3 <%f", set_trim_sample_rate);
            else
                addtopup( sample_rate_menu, " 3 %f", set_trim_sample_rate);

            if (sample == 5) 
                addtopup( sample_rate_menu,">  5 <%f", set_trim_sample_rate);
            else
                addtopup( sample_rate_menu, " 5 %f", set_trim_sample_rate);

            if (sample == 10) 
                addtopup( sample_rate_menu,"> 10 <%f", set_trim_sample_rate);
            else
                addtopup( sample_rate_menu, "10 %f", set_trim_sample_rate);

            if (sample == 20) 
                addtopup( sample_rate_menu,"> 20 <%f", set_trim_sample_rate);
            else
                addtopup( sample_rate_menu, "20 %f", set_trim_sample_rate);
        }


        displaymenu = newpup();
        addtopup( displaymenu, "display %t");
        addtopup( displaymenu, "shaded polygons %x5 %f",set_display); 
        addtopup( displaymenu, "outline polygons %x6 %f",set_display); 
        addtopup( displaymenu, "outline patches %x7 %f",set_display); 
        addtopup( displaymenu, "pixel tolerance %m",pixelmenu); 
        addtopup( displaymenu, "toggle net %x9 %f",set_display);
        addtopup( displaymenu, "toggle backface culling %x10 %f",set_display);
        addtopup( displaymenu, "toggle hemis1 trim %x11 %f",set_display);
        addtopup( displaymenu, "trim sample rate %m",sample_rate_menu);

        mainmenu = newpup();
        addtopup( mainmenu, "NURBS Quadrics %t");
        addtopup( mainmenu, "models %m",modelmenu);
        addtopup( mainmenu, "display %m",displaymenu);
        addtopup( mainmenu, "quit %x10");
}
