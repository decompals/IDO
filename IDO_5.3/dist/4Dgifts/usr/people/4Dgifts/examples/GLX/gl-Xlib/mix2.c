/*
 * mix2.c:
 *
 *    Example mixed model program.  To compile execute:
 *
 *      cc mix2.c glxh2.c -o mix2 -lgutil -lsphere -lgl_s -lm -lX11_s
 *
 *    This program opens up a top level (parent) X window, then creates 
 *    six GL windows as children of the X window.  Each GL window runs 
 *    a different display mode and is positioned in the parent X window 
 *    as follows:
 *
 *  +---------------------------------------------------------------------+
 *  | wins[win1] - cmap-singlebuffer   wins[win2] - cmap-doublebuffer     |
 *  |                                                                     |
 *  | wins[win3] - rgb-singlebuffer    wins[win4] - rgb-doublebuffer      |
 *  |                                                                     |
 *  | wins[win5] - zbuffered-rgb-      wins[win6] - overlay[/popup]-cmap- |
 *  |              doublebuffer                     doublebuffer          |
 *  +---------------------------------------------------------------------+
 *
 *  - Pressing the SPACEBAR rotates the objects in the first 5 GL windows.
 *  - Pressing either ALT key runs the bouncing ball in the 6th GL window.
 *  - Pressing 1KEY or 2KEY or .. 5KEY rotates the object in that window.
 *  - Pressing the b key toggles the bell in the 6th window.
 *  - Pressing ESCAPE exits the program.
 *
 *    There exist various X code-segment implementations in this prog
 *    program which perform the same operations that used to be done
 *    with the GL.  The GL functions being replaced are:
 *
 *   - mapcolor()    see "w6colorsetup()" and "w7colorsetup()", below,
 *                   which implement XStoreColor.  Note these are only
 *                   called once at the beginning of main (before the
 *                   infinite loop starts) to define the colors we want
 *                   to associate with the contexts for windows 6 and 7
 *                   the way it used to be done w/mapcolor before.  Once
 *                   these are defined, simply call color(index) in the
 *                   body of the drawing function for that window just
 *                   like you'd always do in GL land.  
 *
 *   - keepaspect()  in main (bottom of this file) see the initial 
 *                   assignment of the struct, "Waspecthints".  Then, 
 *                   after a ConfigureNotify event has been recognized
 *                   inside the infinite loop, the event struct "ev" 
 *                   (ev.xconfigure.width/height), provides us with the
 *                   new X parent window dimensions.  After manually 
 *                   re-calculating the new size and position of each GL
 *                   window's origin, we call XMoveResizeWindow(...) for
 *                   each window explicitly.  At this point they have
 *                   been resized according to the aspect ratio specified
 *                   and assigned to the various elements of Waspecthints.
 *
 *    Notice also the "do { ... } while (XPending(dpy))" loop at the top
 *    of the infinite loop in main.  This piece grabs up X events and
 *    processes them only so long as the KEYBD keys ("ALT" or "SPACEBAR") 
 *    are pressed.  Once released, the drawing stops.  This was not the 
 *    case in a prior version where the sections that processed the 
 *    incoming events (all the GL drawing parts) were included in the
 *    same part recognizing the events.  This "recognizing" part has now
 *    been separated out into the "do while" section which ensures the
 *    processing of events does not occur until the event queue has been
 *    completely drained.  We *strongly* encourage all programmers and 
 *    developers to adopt this style of not processing events until the
 *    event queue is empty if you are not already doing so.  It's more
 *    efficient, and much cleaner to read and run.
 *
 *                                           ratmandu -- july, 1991
 */

#include <stdio.h>
#include <gl/glws.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "glxh2.h"

#define radius 50.0       /* radius for sphere in wins[win3] & wins[win4]  */
#define WINMAX 7        /* 6 GL windows w/the 7th being w6's overlay/popup */

#define win1   0                                  /* use these defines so */
#define win2   1                                  /* the indices for each */
#define win3   2                                  /* of the GL windows is */ 
#define win4   3                                  /* more clearly         */
#define win5   4                                  /* delineated           */
#define win6   5
#define win7   6

Display* dpy;                              /*  define our current display  */
Window top, wins[WINMAX];                 /* the X & GL window identifiers */
unsigned long w6cmap, w7cmap;          /* stores OVERLAY/POPUP win6/7 cmap */
unsigned int width;              /* one var for width/height (square wins) */
float angle;                            /* rotates polygons in windows 1-2 */
unsigned char bell;                 /* for setbell()'s state in wins[win6] */

/*
 *  A little helper wrapper for GLXwinset.  
 *  It passes the global variable "dpy" which contains the display, and it 
 *  checks the return value.  This makes the call to begin GL drawing a 
 *  little simpler.  Building in such automatic error checking is always a 
 *  "smooth move" (*not* like the cancerously-mutant human with the big 
 *  proboscis who is plastered all over the place urging people to be 
 *  likewise smoothly cancerous and hardly cool...).
 */
void Winset(Window w)
{
    int rv = GLXwinset(dpy, w);
    if (rv < 0) {
        printf("GLXWinset returned %d\n", rv);
        exit(-1);
    }
}




/*
 * the GL drawing stuff follows, 
 * skip down to "main" to see how the program works.
 */

/* ------------- stuff for GL windows wins[win1] & wins[win2] ------------- */

static float vs[4][3] = {                     /* the coords for the polygon */
    { -1, -1, 0, },
    {  1, -1, 0, },
    {  1,  1, 0, },
    { -1,  1, 0, },
};

/* routine which draws into GL windows wins[win1] & wins[win2] */
void colorIndexDraw(int c)
{
    viewport(0,width-1,0,width-1);
    perspective(450, 1.0, 1.0, 200.0);
    color(0);
    clear();
    translate(0, 0, -150);
    rot(angle, 'x');
    rot(angle+1, 'y');
    scale(45, 45, 0);
    color(c);
    bgnpolygon();
        v3f(vs[0]);
        v3f(vs[1]);
        v3f(vs[2]);
        v3f(vs[3]);
    endpolygon();
}



/* ------------ stuff for GL windows wins[win3] & wins[win4] ------------- */

        /* following declarations set up the lighting properties */

static float material[] = {
    SHININESS, 30.0,
    SPECULAR, 0.2, 0.2, 0.2,
    LMNULL,
};

static float model[] = {
    AMBIENT, 1.0, 1.0, 1.0,
    LOCALVIEWER, 0.0,
    LMNULL
};


static float light[] = {
    AMBIENT, 0.1, 0.1, 0.1,
    LCOLOR, 0.5, 1.0, 1.0,
    POSITION, 90.0, 90.0, 150.0, 0.0,
    LMNULL
};

static float lightPos[] = {
    POSITION, 0.0, 0.0, 1.0, 0.0,
    LMNULL
};

Matrix IdentityMat = {
    { 1, 0, 0, 0 },
    { 0, 1, 0, 0 },
    { 0, 0, 1, 0 },
    { 0, 0, 0, 1 },
};
Matrix RotLightMat = {
    { 1, 0, 0, 0 },
    { 0, 1, 0, 0 },
    { 0, 0, 1, 0 },
    { 0, 0, 0, 1 },
};


/* specify the lighting & projection states for GL wins[win3] & wins[win4] */
void rgbSetup()
{
    backface(TRUE);

    lmdef(DEFLIGHT, 1, 0, light);
    lmdef(DEFMATERIAL, 1, 0, material);
    lmdef(DEFLMODEL, 1, 0, model);

    mmode(MVIEWING);
    viewport(0,width-1,0,width-1);
    perspective(450, 1.0, 1.0, 200.0);

    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
    lmbind(MATERIAL, 1);
}


/* GL drawing part for GL windows wins[win3] & wins[win4] */
void rgbDraw()
{
    float vec[4];


    czclear(0x000, 0);

    pushmatrix();
        loadmatrix(IdentityMat);
        rot(3, 'x');
        rot(4, 'y');
        multmatrix(RotLightMat);
        lmdef(DEFLIGHT, 1, 0, lightPos);
        getmatrix(RotLightMat);
    popmatrix();

    pushmatrix();
        translate(0, 0, -150);
        RGBcolor(255, 255, 255);
        vec[0] = 0;
        vec[1] = 0;
        vec[2] = 0;
        vec[3] = radius;
        sphdraw(vec);
    popmatrix();
}



/* -------------------- stuff for GL window wins[win5] ------------------- */

Matrix objmat = {                /* matrix for compound rotations allowing */
    {1.0, 0.0, 0.0, 0.0},        /* continuous screen-oriented rotations   */
    {0.0, 1.0, 0.0, 0.0},
    {0.0, 0.0, 1.0, 0.0},
    {0.0, 0.0, 0.0, 1.0},
};


void zrgbDraw()
{
    long zfar;
    zfar = getgdesc(GD_ZMAX);


    pushmatrix();
        loadmatrix(IdentityMat);
        rotate(8, 'y');
        rotate(3, 'x');
        rotate(-2, 'z');
        multmatrix(objmat);
        getmatrix(objmat);
    popmatrix();

    czclear(0x00C86428, zfar);

    viewport(0,width-1,0,width-1);
    perspective(400, 1.0, 30.0, 60.0);
    translate(0.0, 0.0, -40.0);
    multmatrix(objmat);
    rotate(-580, 'y');         /* skews original view to show all polygons */
    draw_polys();
}

              /* coords for the three intersecting polygons */
float polygon1[3][3] = { {-10.0, -10.0,   0.0,},
                         { 10.0, -10.0,   0.0,},
                         {-10.0,  10.0,   0.0,} };

float polygon2[3][3] = { {  0.0, -10.0, -10.0,},
                         {  0.0, -10.0,  10.0,},
                         {  0.0,   5.0, -10.0,} };

float polygon3[4][3] = { {-10.0,   6.0,   4.0,},
                         {-10.0,   3.0,   4.0,},
                         {  4.0,  -9.0, -10.0,},
                         {  4.0,  -6.0, -10.0,} };

draw_polys() 
{
    bgnpolygon();
    cpack(0x00000000);
    v3f(&polygon1[0][0]);
    cpack(0x007F7F7F);
    v3f(&polygon1[1][0]);
    cpack(0x00FFFFFF);
    v3f(&polygon1[2][0]);
    endpolygon();

    bgnpolygon();
    cpack(0x0000FFFF);
    v3f(&polygon2[0][0]);
    cpack(0x007FFF00);
    v3f(&polygon2[1][0]);
    cpack(0x00FF0000);
    v3f(&polygon2[2][0]);
    endpolygon();

    bgnpolygon();
    cpack(0x0000FFFF);
    v3f(&polygon3[0][0]);
    cpack(0x00FF00FF);
    v3f(&polygon3[1][0]);
    cpack(0x00FF0000);
    v3f(&polygon3[2][0]);
    cpack(0x00FF00FF);
    v3f(&polygon3[3][0]);
    endpolygon();
}




/* --------------- stuff for GL window wins[win6]/wins[win7] ------------- */

/*   the sixth window demos using overlay (popup in the case of 8-bit PI
 *   or hollywood) bitplanes.  to do this in the mixed model paradigm, we
 *   need to define a separate window to do the overlay(/popup) drawing 
 *   into.  this will be what GL window wins[win7] is used for.
 */


unsigned short mycolortbl[83];  /* rgb() (from libgutil) in rampup() (below), 
                                   stores colors here from the default GL cmap 
				   so we don't need to write into this cmap */
long run6_7;                    /* used so the ball doesn't run *except* 
                                   when the user presses either Alt key     */
w6colorsetup()
{
    XColor colorstruct;

    /* use the following segment with XStoreColor to define the same RGB
     * triplets you would do before using mapcolor().  (w6cmap gets set
     * down below in main after wins[win6] gets created.)  defining colors 
     * in this manner will avoid the ugly "video streaking" effect that
     * dances across the screen which repeated calls to mapcolor will 
     * create inside a mixed model application.
     */
    colorstruct.pixel = 1; 
    colorstruct.red   = 255; colorstruct.green = 255; colorstruct.blue  =   0;
    colorstruct.pixel = 10;
    colorstruct.red   = 255; colorstruct.green = 255; colorstruct.blue  = 255;
    colorstruct.flags = DoRed | DoGreen | DoBlue;
    XStoreColor(dpy, w6cmap, &colorstruct);
}


ball () 
{
    Colorindex heat;
    int        rampup();
    long       sx, sy;
    int        i, j, xpos = .9, ypos = .9;
    float      xspd = 0.0, yspd = 0.0, 
               yaccel = -1.0, yacc = -.8, yreflect = -0.6;



    viewport(0,width-1,0,width-1);
    ortho2(0.0, 1.0, 0.0, 1.0);

    setbell(bell);

    if (run6_7) {/* execute the following only after Alt has been pressed */

        for (i = j = 0.1; i < 990 && j < 990; i = i + 17, j = j + 17) {   
            color (BLUE);            /* roll the ball up and to the right */
            clear (); 
            if ((i==204) || (i==510) || (i==816)) 
                ringbell();                    /* ring the bell everytime */
            if ((i>250) && (i<550))            /* the ball gets to the    */
                color (9);                     /* next rectangle, as well */
            else if ((i>550) && (i<850))       /* as chng the ball's color*/
                color (5);
            else if (i>850)
                color (7);
            else
                color (3);
            circf (i/990.0, j/990.0, .035);
            swapbuffers ();
        }

        yspd = 0.0;
        for (heat=82, ypos=990; ypos>=5; ypos+=yspd) {
            color (BLUE);             /* drop the ball back to the bottom */
            clear ();                 
            color (mycolortbl[heat--]);        /* change the ball's color */
            yspd += yacc;                      /* as it falls             */
            circf (.95, ypos/990.0, .035);
            swapbuffers ();
        }

        yspd = -60.0;
        for (xpos=990, ypos=10; xpos >= 0; xpos -= 6) {
            if (ypos <= 10)        /* roll the ball back to the beginning */
                 yspd *= yreflect;            /* and keep updating its'   */
            color (BLUE);                     /* bounce-ability per frame */
            clear (); 
            color (1);
            ypos += yspd;
            yspd += yaccel;
            circf (xpos/990.0, ypos/790.0, .035);
            swapbuffers ();
        }
        color (3);
        circf (0.03, 0.028, .035);

    } else {/* Alt hasn't been pressed so just redraw the background & ball */

        color (BLUE);            
        clear (); 
        color (3);
        circf (0.03, 0.028, .035);

    }
}


w7colorsetup()
{
    XColor colorstruct;

    /* use the following segment with XStoreColor to define the same RGB
     * triplets you would do before using mapcolor(3G).  (w7cmap gets set
     * down below in main after wins[win7] gets created.)  defining colors 
     * in this manner will avoid the ugly "video streaking" effect that  
     * dances across the screen which repeated calls to mapcolor() will 
     * create inside a mixed model application.
     */
    colorstruct.pixel = 1;
    colorstruct.red   = 255; colorstruct.green = 255; colorstruct.blue  =   0;
    colorstruct.pixel = 2;
    colorstruct.red   =   0; colorstruct.green = 255; colorstruct.blue  =   0;
    colorstruct.pixel = 3;
    colorstruct.red   =   0; colorstruct.green = 255; colorstruct.blue  = 255;
    colorstruct.flags = DoRed | DoGreen | DoBlue;
    XStoreColor(dpy, w7cmap, &colorstruct);
}


ballover()                        /* this is wins[win7]--the OVERLAY[/POPUP] */
{                                       /* GL drawing part of the 6th window */
    viewport(0,width-1,0,width-1);
    ortho2(0.0, 1.0, 0.0, 1.0);

    color (0);
    rectf (0.0, 0.0, width, width);
    color (1);
    rectf (.215, .215, .285, .285);       /* draw some rectangles for the  */
    color (2);                            /* ball to roll underneath       */
    rectf (.515, .515, .585, .585);
    color (3);
    rectf (.815, .815, .885, .885);
    color (2);
    rectf (.877, .527, .947, .597);
    rectf (.733, .527, .805, .597);
    rectf (.949, .455,1.019, .525);
    rectf (.805, .455, .875, .525);
    rectf (.661, .455, .731, .525);
    rectf (.877, .383, .947, .453);
    rectf (.733, .383, .805, .453);   
    rectf (.589, .383, .659, .453);
    rectf (.949, .311,1.019, .381);
    rectf (.877, .239, .947, .309);
    rectf (.805, .311, .875, .381);
    rectf (.733, .239, .803, .309);
    rectf (.661, .311, .731, .381);
    rectf (.589, .239, .659, .309);
    rectf (.519, .311, .587, .381);
    rectf (.447, .239, .517, .309);
    rectf (.661, .167, .731, .237);
    rectf (.519, .167, .587, .237);
    rectf (.375, .167, .445, .237);
    rectf (.447, .095, .517, .165);
    rectf (.589, .095, .659, .165);
    rectf (.661, .023, .731, .093);
    rectf (.519, .023, .587, .093);
    rectf (.375, .023, .445, .093);
}


/*
 * rampup:
 *
 *   make a color ramp of already-defined color map indices and store them
 *   in mycolortbl[].  in this way, you don't need to overwrite the GL cmap
 *   and so can be a "responsible citizen" in the new X world order...
 *
 *   rampup makes an interpolated ramp from the 1st arguement's index to 
 *   the 2nd.  3rd & 4th are red's low & hi indices (5&6 green's, 7&8 blue's) 
 */

rampup(first_lutv,last_lutv,minR,maxR,minG,maxG,minB,maxB)
unsigned short first_lutv, last_lutv,          /* start & end ramp values */
               minR, maxR, minG, maxG, minB, maxB;      /* lo/hi rgb vals */
{
    unsigned short len_red, len_green, len_blue,  /* length of each color */
                   i;                      /* counter for number of steps */
    float red, gre, blu;                                    /* lut values */
    float rdx, gdx, bdx,                       /* sizes of rgb increments */
          r, g, b,                              /* a position on the ramp */
          steps;                     /* # of steps along the ramp @ which */
                                    /* intensity assignments will be made */


    steps = (float) (last_lutv-first_lutv + 1);/* determine length of ramp*/

    len_red   = (maxR - minR);                 /* determine length of red */
    len_green = (maxG - minG);               /* determine length of green */
    len_blue  = (maxB - minB);                /* determine length of blue */

    rdx = (float) len_red   / steps;                      /* compuke step */
    gdx = (float) len_green / steps;                      /* sizes of r g */
    bdx = (float) len_blue  / steps;                      /* and b values */
    r = minR;                                          /* assign starting */
    g = minG;                                         /* indices for each */
    b = minB;                                         /* color value      */

    for (i = first_lutv; i <= last_lutv; i++) {
        red = r/255.0;                                       /* round off */
        gre = g/255.0;                                       /* given r g */
        blu = b/255.0;                                       /* b value   */
                                          /* assign next color into mytbl */
        mycolortbl[i] = (unsigned short) rgb(red,gre,blu);
        r += rdx;                                            /* increment */
        g += gdx;                                            /* color in- */
        b += bdx;                                            /* dices     */
    }    
}



/* --------------- end of wins[win1]-[win7] code segments --------------- */




/*
 * Called when the SPACEBAR key is pressed, rotate a little, then redraw 
 * everything in windows wins[win1]-wins[win5].
 */

void updateAll()
{
    angle += 3.0;

    Winset(wins[win1]);
    colorIndexDraw(1);

    Winset(wins[win2]);
    colorIndexDraw(2);
    swapbuffers();


    Winset(wins[win3]);
    rgbDraw();

    Winset(wins[win4]);
    rgbDraw();
    swapbuffers();


    Winset(wins[win5]);
    zbuffer(TRUE);
    zrgbDraw();
    swapbuffers();
    zbuffer(FALSE);
}


#define round(x)  ((int)((x)+0.5)) /* used to compuke new window positions */

void 
main()
{
    XSetWindowAttributes swa;    /* needed for XCreateColor(3X11)          */
    XColor gray;                 /* stores the color for X window backgrnd */
    GLXconfig*  config;          /* pointer to current GLX window created 
				    via GLXCreateWindow defined in glxh2.c */
    XSizeHints *Waspecthints;    /* used for min-size & aspect ratio manip */
    int w, h;                /* save new resized width/height for X parent */
    int wx[6], wy[6];      /* 6 x/y top-left origin points for the GL wins */
    float xunit, yunit;                   /* used to calc chnged win sizes */
    int myExpose, myConfigure, myKeyPress; /* indicates which events occur */
    int upAndRunning = 0;                  /* don't set this to true until
					      we get our first Expose event*/



    dpy = XOpenDisplay(0);              /* rule # 0: establish our display */
    if (!dpy) {
        fprintf(stderr, "cannot open display\n");
        exit(-1);
    }
    myExpose = myConfigure = myKeyPress = 0;     /* init our own notifiers */
    run6_7 = FALSE;                           
    bell = FALSE;

    /*
     * First create the top level X window.  The background will be gray,
     * and *this* (X parent) window is only interested in key press events.
     */
    gray.red = 0x7fff; gray.green = 0x7fff; gray.blue = 0x7fff;
    XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &gray);
    swa.background_pixel = gray.pixel;
    swa.event_mask = KeyPressMask|StructureNotifyMask;
    top = XCreateWindow(dpy, RootWindow(dpy, DefaultScreen(dpy)),
                             100, 100, 230, 340,
                             0, CopyFromParent, InputOutput, CopyFromParent,
                             CWEventMask|CWBackPixel, &swa);
    XStoreName(dpy, top, "Six GL Windows");

    /*
     *  Now specify the values for the Window Aspect Hints we want to 
     *  enforce so that the aspect ratio of 2 to 3 will always be 
     *  maintained when the parent X window is resized.  Also specify
     *  the minimum size (46x68) we will allow this window to shrink to.
     */
    Waspecthints = XAllocSizeHints();
    Waspecthints->min_aspect.x = 2;
    Waspecthints->max_aspect.x = 2;
    Waspecthints->min_aspect.y = 3;
    Waspecthints->max_aspect.y = 3;
    Waspecthints->min_width  = 230;
    Waspecthints->min_width  = 46;
    Waspecthints->min_height = 340;
    Waspecthints->min_height = 68;
    Waspecthints->flags = PAspect|PMinSize;
    XSetWMNormalHints(dpy, top, Waspecthints);

    /* define the initial sizes and placement of the 6 GL window origins.
     */
    width = 100;
    wx[win1] =  10;     wy[win1] =  10;
    wx[win2] = 120;     wy[win2] =  10;
    wx[win3] =  10;     wy[win3] = 120;
    wx[win4] = 120;     wy[win4] = 120;
    wx[win5] =  10;     wy[win5] = 230;
    wx[win6] = 120;     wy[win6] = 230;

    /*
     * Now create the six GL windows.  Each one is interested in "Expose" 
     * events (sort of like "REDRAW" in gl-speak in terms of when a window 
     * becomes visible or a previously invisible part becomes visible) as 
     * well as "ConfigNotify" (like "REDRAW" in terms of changes to a 
     * window's size or position) events. (GLXCreateWindow() is defined in 
     * glxh2.c)
     */

    /* create the upper-left colormap-singlebuffer'd GL window.
     *  note in this most default of default cases, the "arg_list"--the 
     *  eighth, and any subsequent, entrie(s)--in this case is nothing but 
     *  a zero.  This/these is/are eaten up correctly every time by the 
     *  STDARG(5) functions employed at the beginning of GLXCreateWindow().  
     *  In this case there's only the one arg of 0 (always has to be null
     *  terminated).
     */
    config = GLXCreateWindow(dpy, top, wx[win1], wy[win1], width, width, 0, 0);
    /* now scan through the config struct, looking for the proper argument
     *  value associated with the following buffer(arg2)-mode(arg3) pair.
     */
    wins[win1] = GLXgetvalue(config, GLX_NORMAL, GLX_WINDOW);
    /* we're going to reuse config for each GL window so free it up now */
    free(config);

    /* create the upper-right colormap-doublebuffer'd GL window */
    config = GLXCreateWindow(dpy, top, wx[win2], wy[win2], width, width, 0,
                             GLX_NORMAL, GLX_DOUBLE, TRUE,
                             0, 0, 0);
    wins[win2] = GLXgetvalue(config, GLX_NORMAL, GLX_WINDOW);
    free(config);

    /* create the middle-left rgb-singlebuffer'd GL window */
    config = GLXCreateWindow(dpy, top, wx[win3], wy[win3], width, width, 0,
                             GLX_NORMAL,GLX_RGB, TRUE,
                             0, 0, 0);
    wins[win3] = GLXgetvalue(config, GLX_NORMAL, GLX_WINDOW);
    free(config);

    /* create the middle-right rgb-doublebuffer'd GL window */
    config = GLXCreateWindow(dpy, top, wx[win4], wy[win4], width, width, 0,
                             GLX_NORMAL,GLX_DOUBLE, TRUE,
                             GLX_NORMAL,GLX_RGB, TRUE,
                             0, 0, 0);
    wins[win4] = GLXgetvalue(config, GLX_NORMAL, GLX_WINDOW);
    free(config);

    /* note the use of GLX_NOCONFIG for the zbuffer'd and overlay'd/popup'd
     *  5th and 6th/7th windows below.  this "arg" indicates that we want
     *  the largest number of bitplanes available returned to use.  This
     *  arg is always more modular that using a fixed/hard-wired number.
     */
    /* create the lower-left rgb-doublebuffer'd GL window */
    config = GLXCreateWindow(dpy, top, wx[win5], wy[win5], width, width, 0,
                             GLX_NORMAL,GLX_DOUBLE, TRUE,
                             GLX_NORMAL,GLX_RGB, TRUE,
                             GLX_NORMAL,GLX_ZSIZE, GLX_NOCONFIG,
                             0, 0, 0);
    wins[win5] = GLXgetvalue(config, GLX_NORMAL, GLX_WINDOW);
    free(config);

    /* now create the lower-right colormap-doublebuffer'd GL window */

    /* first we need to figure out if we've got enough overlay bitplanes on
     *  the machine we're currently running on to run GLXgetconfig using
     *  GLX_OVERLAY or not.  if not, then we'll use GLX_POPUP to "fake it".
     */
    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2) {/* test for overlay planes */

        config = GLXCreateWindow(dpy, top, wx[win6], wy[win6], width, width, 0,
                                 GLX_NORMAL,GLX_DOUBLE, TRUE,
                                 GLX_POPUP,GLX_BUFSIZE, GLX_NOCONFIG,
                                 0, 0, 0);
        wins[win7] = GLXgetvalue(config, GLX_POPUP, GLX_WINDOW);

    /* in this instance, we will need to have a handle to the colormap for
     *  this window because we're going to define some of our own colors.
     *  (see w7colorsetup(), above.)
     */
        w7cmap =  GLXgetvalue(config, GLX_POPUP, GLX_COLORMAP);

    } else {

        config = GLXCreateWindow(dpy, top, wx[win6], wy[win6], width, width, 0,
                                 GLX_NORMAL,GLX_DOUBLE, TRUE,
                                 GLX_OVERLAY,GLX_BUFSIZE, GLX_NOCONFIG,
                                 0, 0, 0);
        wins[win7] = GLXgetvalue(config, GLX_OVERLAY, GLX_WINDOW);
    /* in this instance, we will need to have a handle to the colormap for
     *  this window because we're going to define some of our own colors.
     *  (see w7colorsetup(), above.)
     */
        w7cmap =  GLXgetvalue(config, GLX_OVERLAY, GLX_COLORMAP);
    }
    /* now we need to scan through the config struct we've just put together
     *  for the POPUP or OVERLAY window that will get linked in with win6, 
     *  and this time find the proper argument value that is associated with
     *  the GLX_NORMAL,GLXWINDOW pair.
     */
    wins[win6] = GLXgetvalue(config, GLX_NORMAL, GLX_WINDOW);

    /* in this instance, we will need to have a handle to the colormap for
     *  this window because we're going to define some of our own colors.
     *  (see w6colorsetup(), above.)
     */
    w6cmap =  GLXgetvalue(config, GLX_NORMAL, GLX_COLORMAP);
    free(config);

    /*
     * Now that all the windows exist, cause them to appear by requesting
     *  the Xserver report all Expose events that occur in these 6 windows.
     */
    XSelectInput(dpy, wins[win1], ExposureMask);
    XSelectInput(dpy, wins[win2], ExposureMask);
    XSelectInput(dpy, wins[win3], ExposureMask);
    XSelectInput(dpy, wins[win4], ExposureMask);
    XSelectInput(dpy, wins[win5], ExposureMask);
    XSelectInput(dpy, wins[win6], ExposureMask);
    XSelectInput(dpy, wins[win7], ExposureMask);

    /* now map (make visible) the 6 GL windows */
    XMapWindow(dpy, wins[win7]);
    XMapWindow(dpy, wins[win6]);
    XMapWindow(dpy, wins[win5]);
    XMapWindow(dpy, wins[win4]);
    XMapWindow(dpy, wins[win3]);
    XMapWindow(dpy, wins[win2]);
    XMapWindow(dpy, wins[win1]);
 
    /* now install these window's colormaps so they'll show their true colors */
    XSetWMColormapWindows(dpy, top, wins, WINMAX);

    /* and now, finally, map the big mama of 'em all, the X parent */
    XMapWindow(dpy, top);

    /* make the color table for window six's bouncing ball */
    rampup(12, 82, 255, 255, 0, 255, 0, 0);
    /* define the colors we want to set for GL wins[win6] 
     * (which includes wins[win7]) */
    w6colorsetup();
    w7colorsetup();


    while (1){         /* standard logic:  get event(s), process event(s) */

        XEvent ev;
        KeySym keysym;
        char buf[4];


    /* this "do while" loop does the `get events' half of the "get events,
     *  process events" action of the infinite while.  this is to ensure 
     *  the event queue is always drained before the events that have come
     *  in are processed.
     */
        do {

            XNextEvent(dpy, &ev);
            switch (ev.type) {

            /* "Expose" events are sort of like "REDRAW" in gl-speak in 
	     *  terms of when a window becomes visible, or a previously 
	     *  invisible part becomes visible.
	     */
                case Expose:

		{
		    int i;
            /* turn upAndRunning on only after we get our first Expose event */
                    upAndRunning = 1;
		    for (i = 0; i < WINMAX; i++) 
    			if (ev.xexpose.window == wins[i])
				myExpose |= (1 << i);
			break;
		}
 
            /* "ConfigNotify" events are like "REDRAW" in terms of changes 
	     *   to a window's size or position. 
	     */
                case ConfigureNotify:

                    if (ev.xconfigure.window == top) {
            /* save the changed width/height of the parent X window */
                        w = ev.xconfigure.width;
                        h = ev.xconfigure.height;
                        myConfigure = TRUE;
                    }
                    break;


            /* "KeyPress" events are those that would be generated before
	     *   whenever queueing up any KEYBD key via qdevice.
	     */
                case KeyPress:

            /* save out which unmodified (unmodified being that the key was
	     *  not modified w/something like "Shift", "Ctrl", or "Alt") key 
	     *  got pressed for use below.
	     */     
		    XLookupString((XKeyEvent*)&ev, buf, 4, &keysym, 0);
                    myKeyPress = TRUE;
                    break;
              }

    /* end of "do while".  XPending() is like qtest()--it only tells you
     *  if there are any events presently in the queue.  it does not disturb
     *  the contents of the queue in any way.
     */
        } while (XPending(dpy));


    /*
     * On an "Expose" event, redraw the affected window
     */
        if (myExpose & 1) {
            Winset(wins[win1]);
            colorIndexDraw(1);
        } 
        if (myExpose & 2) {
            Winset(wins[win2]);
            colorIndexDraw(2);
            swapbuffers();
        } 

        if (myExpose & 4) {
            Winset(wins[win3]);
            rgbDraw();

        } 
        if (myExpose & 8) {
            Winset(wins[win4]);
            rgbDraw();
            swapbuffers();

        } 
        if (myExpose & 16) {
            Winset(wins[win5]);
            zbuffer(TRUE);
            zrgbDraw();
            swapbuffers();
            zbuffer(FALSE);

        } 
        if (myExpose & 32) {
            Winset(wins[win6]);
            ball();
            swapbuffers();
            Winset(wins[win7]);
            ballover();
        }
        if (myExpose & 64) {
            Winset(wins[win7]);
            ballover();
        }
    /* now reset our "Expose" flag to indicate the queue is empty */
        myExpose = 0;


    /*
     * On a "ConfigureNotify" event, resize all windows (XMoveResizeWindow),
     * and then redraw their contents accordingly.
     */
        if (myConfigure) {

    /* compuke our current "unit" of size.  since we started w/2 windows of
     *  100 pixels square (in x which is being treated here as width) and the 
     *  borders between each (i.e. the background of the X parent window) are 
     *  10 pixels, the xunit is divied up into 23, 10-pixels units.  the yunit
     *  is 34 10-pixel units because of the 100*3 (3 GL windows high) plus
     *  4*10 pixel borders in the height direction.
     */
            xunit = w/23.0;
            yunit = h/34.0;
            width = round(10*xunit);

    /* compuke the new top-left origins of the 6 windows */
            wx[win1] = round(xunit);      wy[win1] = round(yunit);
            wx[win2] = round(xunit*12);   wy[win2] = round(yunit);
            wx[win3] = round(xunit);      wy[win3] = round(yunit*12);
            wx[win4] = round(xunit*12);   wy[win4] = round(yunit*12);
            wx[win5] = round(xunit);      wy[win5] = round(yunit*23);
            wx[win6] = round(xunit*12);   wy[win6] = round(yunit*23);

    /* tell X to resize these 6 windows */
            XMoveResizeWindow(dpy, wins[win1], wx[win1], wy[win1], width, width);
            XMoveResizeWindow(dpy, wins[win2], wx[win2], wy[win2], width, width);
            XMoveResizeWindow(dpy, wins[win3], wx[win3], wy[win3], width, width);
            XMoveResizeWindow(dpy, wins[win4], wx[win4], wy[win4], width, width);
            XMoveResizeWindow(dpy, wins[win5], wx[win5], wy[win5], width, width);
            XMoveResizeWindow(dpy, wins[win6], wx[win6], wy[win6], width, width);
            XSync(dpy, False);

    /* and now redraw their contents in their new size and/or positions */
            Winset(wins[win1]);
            reshapeviewport();
            if (upAndRunning)
                colorIndexDraw(1);

            Winset(wins[win2]);
            reshapeviewport();
            if (upAndRunning) {
                colorIndexDraw(2);
                swapbuffers();
            }

            Winset(wins[win3]);
            reshapeviewport();
            rgbSetup();
            if (upAndRunning) {
                rgbDraw();
            }

            Winset(wins[win4]);
            reshapeviewport();
            rgbSetup();
            if (upAndRunning) {
                rgbDraw();
                swapbuffers();
            }

            Winset(wins[win5]);
            reshapeviewport();
            if (upAndRunning) {
                zbuffer(TRUE);
                zrgbDraw();
                swapbuffers();
                zbuffer(FALSE);
            }

            Winset(wins[win6]);
            reshapeviewport();
            if (upAndRunning) {
                run6_7 = FALSE;
                ball();
                swapbuffers();
                Winset(wins[6]);
                ballover();
            }
            myConfigure = FALSE;
        }


        /*
         * On a keypress of
         *  - SPACEBAR: rotate the first 5 GL windows and re-display. 
         *  - either ALT key:  run the bouncing ball in GL window 6.
         *  - 1KEY or 2KEY or .. 5KEY rotates the object in that window.
         *  - b key toggles the bell in the 6th window.
         *  - Esc key:  exit.
         */
        if (myKeyPress) {

            if (keysym == XK_space) {
                updateAll();

            } else if (keysym == XK_Alt_L || keysym == XK_Alt_R) {
                if (!run6_7) {
                    run6_7 = TRUE;
                    Winset(wins[win6]);
                    reshapeviewport();
                    ball();
                    swapbuffers();
                    Winset(wins[win7]);
                    ballover();
                    run6_7 = FALSE;
                }

            } else if (keysym == XK_1) {
                angle -= 8.0;
                Winset(wins[win1]);
                colorIndexDraw(1);

            } else if (keysym == XK_2) {
                angle -= 14.0;
		Winset(wins[win2]);
		colorIndexDraw(2);
		swapbuffers();

            } else if (keysym == XK_3) {
		Winset(wins[win3]);
		rgbDraw();

            } else if (keysym == XK_4) {
		Winset(wins[win4]);
		rgbDraw();
		swapbuffers();

            } else if (keysym == XK_5) {
		Winset(wins[win5]);
		zbuffer(TRUE);
		zrgbDraw();
		swapbuffers();
		zbuffer(FALSE);

            } else if (keysym == XK_b) {
		bell = (bell == TRUE) ? FALSE : TRUE;

            } else if (keysym == XK_Escape) {
                Winset(wins[win6]);
                color (0);
                clear();
                exit(0);
            }
            myKeyPress = FALSE;
        }
    }
}
