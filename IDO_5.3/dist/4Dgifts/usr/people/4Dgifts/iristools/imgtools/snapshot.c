/*
 *        snapshot - 
 *             create a snapshot image from the screen.
 *
 *                        dave ratcliffe and Paul Haeberli - 1989
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

   /* function modes */
#define STARTING         0
#define STRETCHINGCORNER 1
#define STRETCHINGXSIDE  2
#define STRETCHINGYSIDE  3
#define CHANGING         4
#define MOVING           5
#define UPRIGHT          6
#define UPLEFT           7
#define LORIGHT          8
#define LOLEFT           9
#define HORIZONTAL      10
#define VERTICAL        11
#define CAMERA          12

   /* popup menu items */
#define SAVE             1
#define NEWFILENAME      2
#define IPASTEFILENAME   3
#define RUBBERBAND	 4
#define SAVE_EXIT        5
#define EXIT             6

  /* define all the different cursors for current functionality-feedback */
#define X_LLEFT    1
#define Y_LLEFT    3
Cursor loleft =  {0x0000, 0x0000, 0xFFF0, 0xFFF0, 
                  0xC000, 0xC000, 0xCF00, 0xCC00, 
                  0xCA00, 0xC900, 0xC080, 0xC000, 
                  0xC000, 0xC000, 0x0000, 0x0000};

#define X_LRIGHT  14
#define Y_LRIGHT   3
Cursor loright = {0x0000, 0x0000, 0x0FFF, 0x0FFF, 
                  0x0003, 0x0003, 0x00F3, 0x0033, 
                  0x0053, 0x0093, 0x0103, 0x0003, 
                  0x0003, 0x0003, 0x0000, 0x0000};
 
#define X_ULEFT    1
#define Y_ULEFT   12
Cursor upleft =  {0x0000, 0x0000, 0xC000, 0xC000, 
                  0xC000, 0xC080, 0xC900, 0xCA00, 
                  0xCC00, 0xCF00, 0xC000, 0xC000, 
                  0xFFF0, 0xFFF0, 0x0000, 0x0000};

#define X_URIGHT  14
#define Y_URIGHT  12
Cursor upright = {0x0000, 0x0000, 0x0003, 0x0003, 
                  0x0003, 0x0103, 0x0093, 0x0053, 
                  0x0033, 0x00F3, 0x0003, 0x0003, 
                  0x0FFF, 0x0FFF, 0x0000, 0x0000};

#define X_SIDE     8
#define Y_SIDE     8
Cursor horiz =   {0x0000, 0x0000, 0x0000, 0x0000, 
                  0x0000, 0x73C7, 0x0000, 0x7FFE, 
                  0x7FFE, 0x0000, 0x73C7, 0x0000, 
                  0x0000, 0x0000, 0x0000, 0x0000};
 
Cursor verti =   {0x0000, 0x05A0, 0x05A0, 0x05A0, 
                  0x0180, 0x0180, 0x05A0, 0x05A0, 
                  0x05A0, 0x05A0, 0x0180, 0x0180, 
                  0x05A0, 0x05A0, 0x05A0, 0x0000};

#define X_MOVE     8
#define Y_MOVE     7
Cursor moveme =  {0x0080, 0x01C0, 0x02A0, 0x0080, 
                  0x0080, 0x1084, 0x2082, 0x7FFF, 
                  0x2082, 0x1084, 0x0080, 0x0080, 
                  0x02A0, 0x01C0, 0x0080, 0x0000};

#ifdef small
#define X_CAMRA    8
#define Y_CAMRA    7
Cursor camera =  {0x0000, 0x0000, 0xFFFF, 0x89CD, 
                  0xBA2D, 0xAC19, 0xAC99, 0xBC19, 
                  0x8A29, 0x89CB, 0xFFFF, 0xE7F0, 
                  0x43E0, 0x0000, 0x0000, 0x0000};
#else
#define X_CAMRA    14
#define Y_CAMRA    7
unsigned short camera[] = 
                 {0x7FFF, 0xFFC0, 0x408F, 0x8840,
                  0x40B8, 0xEE40, 0x43E0, 0x3E40,
                  0x42C0, 0x1E40, 0x42C2, 0x1E40,
                  0x4282, 0x0840, 0x428F, 0x8840,
                  0x4382, 0x0840, 0x40C2, 0x1840,
                  0x40C0, 0x1840, 0x40E0, 0x3840,
                  0x40B8, 0xE840, 0x408F, 0x8840,
                  0x7FFF, 0xFFC0, 0x3C3F, 0xE000,
                  0x1818, 0xC000, 0x000F, 0x8000,
                     0x0,    0x0,    0x0,    0x0,
                     0x0,    0x0,    0x0,    0x0,
                     0x0,    0x0,    0x0,    0x0,
                     0x0,    0x0,    0x0,    0x0,
                     0x0,    0x0,    0x0,    0x0,
                     0x0,    0x0,    0x0,    0x0,
                     0x0,    0x0,    0x0,    0x0};

#endif


long xmaxscrn, ymaxscrn;          /* maximum size of screen in x and y       */
int sx=110,sy=40;                 /* fixed size of snapshot button window    */
int limit = 10;                   /* tolerance to one side of any edge       */
int xin, xout, yin, yout;         /* pinpoints current mouse/cursor location */
long curxorig, curyorig;          /* snapshot button's current origin        */
int x1 = 0, y1 = 0, x2, y2,       /* the current corners to be scrsave'd     */
                                  /* these values are initially set to       */
                                  /* the entire screen                       */
    sx1, sx2, sy1, sy2;           /* stores val 1 pixel "inside" each edge   */
int menu, menuval;                /* pop-up menu storage vars                */
short mx, my;                     /* current mouse location                  */
int nowindowyet = TRUE;           /* used for menu manip & "reseting" cusror */
int madeimgyet = FALSE;           /* used for the "ipaste" menu option       */
int grabbing_image = FALSE;       /* flag indicates when image is being made */
char aString[40];                 /* "pseudo" textport's string array        */
Boolean MACHMODE;                 /* ==OVERDRAW on systems with >= 24 bp's   */
                                  /*   PUPDRAW on systems with only 8 bp's   */
short  bell;                      /* to ring or not to ring the bell         */


main(argc,argv)
int argc;
char **argv;
{
    Device dev;                  /* event queue's current dev[ice] number    */
    short val;                   /*   and returned state or data value       */
    int *ax, *ay, *bx, *by;      /* alterable x/y "stretching" components    */
    int px1, py1, px2, py2;      /* previous x/y vals in case button is pop'd*/
    int xo, yo;                  /* "old" x/y position used to STRETCH/MOVE  */
    int xc, yc;                  /* current x/y position to update "MOVING"  */
    int xd, yd;                  /* delta x/y value to compute new moved pos */
    int winid;                   /* button window's ID used to pop window    */
    int downmx, downmy,          /* these 4 vars used to detect when popping */
        upmx, upmy;              /* the button window is desired             */
    int mode = STARTING;         /* LEFTMOUSE's current mode of operation    */
    int prevmode = mode;         /* hold previous mode while not INPUT FOCUS */
    char cmd[256],               /* holds cmd string sent on to scrsave      */
         filename[40];           /* contains current image output filename   */
    long numplanes;              /* used to confirm system has >= 8 bit plns.*/
    long xscrnsize;              /* size of screen in x used to set globals  */

/* check the screen size */
    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    x2 = xmaxscrn;                
    y2 = ymaxscrn;               

    if (argc == 2) {                             /* has user started up prog */
        if(strcmp(argv[1],"-b") == 0)            /* w/the "-b" option as a   */
            bell = 1;                            /* second argument or not?  */
        else {
            fprintf(stderr,"usage: snapshot [-b]\n");
            exit(1);
        }
    } else
        bell = 0;
    strcpy(filename,"snap.rgb");      /* default output filename is assigned */

    glcompat(GLC_SOFTATTACH, TRUE);
    noborder();
    prefsize(sx, sy);                         /* setup the window attributes */
    keepaspect(sx, sy);
    winid = winopen("snapshot");
    figure_machine();           /* figure out number of bitplanes to then go */
                                /* ahead & assign the "overlay" drawing mode */


    drawmode(MACHMODE);
    mapcolor(RED, 255, 0, 0);
    drawmode(NORMALDRAW);

    curstype(C16X1);                    /* define/load the different cursors */ 
    defcursor(MOVING,moveme);
    curorigin(MOVING,X_MOVE,Y_MOVE);
    curstype(C16X1);
    defcursor(UPRIGHT,upright);
    curorigin(UPRIGHT,X_URIGHT,Y_URIGHT);
    curstype(C16X1);
    defcursor(UPLEFT,upleft);
    curorigin(UPLEFT,X_ULEFT,Y_ULEFT);
    curstype(C16X1);
    defcursor(LORIGHT,loright);
    curorigin(LORIGHT,X_LRIGHT,Y_LRIGHT);
    curstype(C16X1);
    defcursor(LOLEFT,loleft);
    curorigin(LOLEFT,X_LLEFT,Y_LLEFT);
    curstype(C16X1);
    defcursor(HORIZONTAL,horiz);
    curorigin(HORIZONTAL,X_SIDE,Y_SIDE);
    curstype(C16X1);
    defcursor(VERTICAL,verti);
    curorigin(VERTICAL,X_SIDE,Y_SIDE);
#ifdef small
    curstype(C16X1);
#else                       
    curstype(C32X1);            /* go ahead and use the LARGER camera cursor */
#endif
    defcursor(CAMERA,camera);
    curorigin(CAMERA,X_CAMRA,Y_CAMRA);
    setcursor(CAMERA, 0, 0);                       

    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(LEFTMOUSE);                              /* queue up yer buttons */
    qdevice(RIGHTMOUSE);
    qdevice(MOUSEX);       /*Normally, you don't want to qdevice MOUSEX and */
    qdevice(MOUSEY);       /* MOUSEY. snapshot, however is so small, it */
			   /* serves our purpose here. */


    buildmenu(filename);                          /* create the initial menu */

    draw_button();                            /* draw snapshot button window */

    fullscrn();                               /* enter fullscrn mode,        */
    drawmode(MACHMODE);                       /* enter "overlay" drawmode    */
    gflush();
    color(RED);                               /* then draw the initial "area */
    recti(x1, y1, x2, y2);                    /* of interest" (the entire    */
    drawmode(NORMALDRAW);                     /* console screen)             */

    while(TRUE) {

  /* A qread is here so we block the process until ready to do something. */

            dev = qread(&val);
            switch(dev) {

		case WINQUIT:
                    drawmode(MACHMODE);          /* Be sure to only clear */   
                    color(BLACK);                /* the currently defined */   
                    recti(x1, y1, x2, y2);       /* RED rubberband--don't */   
                    drawmode(NORMALDRAW);        /* disturb any other area*/   
                    gexit();                     /* of the overlay bit-   */   
                    exit(0);                     /* planes before exiting */   
                    break;
		    
		case MOUSEX:
		case MOUSEY:
		    if (mode != STARTING)
			getcursloc(); /* update current cursor cue */
		    break;


                case LEFTMOUSE:     /* either we are making our first outline
                                       --STARTING--or we are changing one of 
                                       the corners/sides--CHANGING--or we are 
                                       moving the current outline--MOVING  */
                    mx = getvaluator(MOUSEX);
                    my = getvaluator(MOUSEY);

                    if (val) {

                        downmx = getvaluator(MOUSEX);   /* save current      */
                        downmy = getvaluator(MOUSEY);   /* mouse position    */
			/* Don't update previous rectangle 
			** if LEFTMOUSE was clicked and not
			** moved.
			*/
			if(x1 != x2){
                           px1 = x1; py1 = y1;   /* and area of inter-*/
                           px2 = x2; py2 = y2;   /* est in case of winpop */
			}

                       switch(mode) {

                            case STARTING:      /* starting a new rubberband */
                                x2 = x1 = mx;    y2 = y1 = my;
                                ax = &x1;        ay = &y1;
                                bx = &x2;        by = &y2;
                                mode = STRETCHINGCORNER;
                                setcursor(LORIGHT, 0, 0);
                                drawmode(MACHMODE);
                                color(BLACK);
                                recti(0, 0, xmaxscrn, ymaxscrn);
                                drawmode(NORMALDRAW);
				qenter (LEFTMOUSE, 1);
                                break;

                            case CHANGING:   /* changing current rubberband */
                                getlocation();
                                if (abs(mx-x1) < abs(mx-x2)) {
                                    if ((x1-limit<=mx)&&(mx<=x1+limit)) {
                                        bx = &x1;
                                        ax = &x2;
                                    }
                                } else if (abs(mx-x2) < abs(mx-x1)) {
                                    if ((x2-limit<=mx)&&(mx<=x2+limit)) {
                                        ax = &x1;
                                        bx = &x2;
                                    }
                                } 
                                if (abs(my-y1) < abs(my-y2)) {
                                    if ((y1-limit<=my)&&(my<=y1+limit)) {
                                        by = &y1;
                                        ay = &y2;
                                    }
                                } else if (abs(my-y2) < abs(my-y1)) {
                                    if ((y2-limit<=my)&&(my<=y2+limit)) {
                                        ay = &y1;
                                        by = &y2;
                                    }
                                } 
                                if (xout && yout && !xin && !yin) {
                                    mode = STRETCHINGCORNER;
				    qenter (LEFTMOUSE, 1);
                                } else if (!xin && xout && yin && yout) {
                                    mode = STRETCHINGXSIDE;
				    qenter (LEFTMOUSE, 1);
                                } else if (!yin && yout && xin && xout) {
                                    mode = STRETCHINGYSIDE;
				    qenter (LEFTMOUSE, 1);
                                } else if (xin&&yin&&xout&&yout) { 
                                    xc = getvaluator(MOUSEX);
                                    yc = getvaluator(MOUSEY);
                                    mode = MOVING;
				    qenter (LEFTMOUSE, 1);
                                } else if ((!xin&&!yin&&!xout&&!yout) ||
                                           (!xin&& yin&&!xout&& yout) ||
                                           ( xin&&!yin&& xout&&!yout) ||
                                           (!xin&&!yin&& xout&&!yout) ||
                                           (!xin&&!yin&&!xout&& yout)) {
     /* LEFTMOUSE has been   */     drawmode(MACHMODE);
     /* pressed down outside */     color(BLACK);
     /* the current rubber-  */     recti(x1, y1, x2, y2);
     /* band so erase the    */     drawmode(NORMALDRAW);
     /* old and set up things*/     x2 = x1 = mx;    y2 = y1 = my;
     /* to begin again at    */     ax = &x1;        ay = &y1;
     /* this  new corner     */     bx = &x2;        by = &y2;
                                    mode = STRETCHINGCORNER;
				    qenter (LEFTMOUSE, 1);
                                    setcursor(LORIGHT, 0, 0);
                                }
                                break;
            		    case MOVING:
			    	while(getbutton(LEFTMOUSE)){
                	    	     xo = xc;  
				     yo = yc;
                	    	     xc = getvaluator(MOUSEX);  
				     yc = getvaluator(MOUSEY);
                	    	     if((xo != xc) || (yo != yc)) {
                    	    	          drawmode(MACHMODE);
					  /* erase the previous rectangle */
                    	    	          color(BLACK); 
                    	    	          recti(x1, y1, x2, y2);
					  /* compuke the delta x and y */
                    	    	          xd = xc - xo;
                    	    	          x1 += xd; x2 += xd;
                    	    	          yd = yc - yo;
                    	    	          y1 += yd; y2 += yd;
					  /* draw the current rectangle */
                    	    	          color(RED);
                    	    	          recti(x1, y1, x2, y2);
                    	    	          drawmode(NORMALDRAW);
			    	     }
			    	}
                	    	break;

            		    case STRETCHINGXSIDE:
		 	    	while(getbutton(LEFTMOUSE)){
                 	    	     xo = *bx;                   
                	    	     *bx = getvaluator(MOUSEX); 
                	    	     if(xo != *bx) {
                    	    	          drawmode(MACHMODE);
					  /* erase the previous x side */
                    	    	          color(BLACK); 
                    	    	          recti(*ax, *ay, xo, *by);
					  /* draw the new x side */
                    	    	          color(RED);    
                    	    	          recti(*ax, *ay, *bx, *by);
                    	    	          drawmode(NORMALDRAW);
                	    	     }
			    	}
                	    	break;
			
            		    case STRETCHINGYSIDE:
		 	    	while(getbutton(LEFTMOUSE)){
                 	    	     yo = *by;
                	    	     *by = getvaluator(MOUSEY);
                	    	     if(yo != *by) {
                    	    	          drawmode(MACHMODE);
					  /* erase the previous y side */
                    	    	          color(BLACK);   
                    	    	          recti(*ax, *ay, *bx, yo);
					  /* draw the new y side */
                    	    	          color(RED);  
                    	    	          recti(*ax, *ay, *bx, *by);
                    	    	          drawmode(NORMALDRAW);
                	    	     }
			    	}
                	    	break;
			
            		    case STRETCHINGCORNER:
		 	    	while(getbutton(LEFTMOUSE)){
                 	    	     xo = *bx;    
				     yo = *by;
                	    	     *bx = getvaluator(MOUSEX);
				     *by = getvaluator(MOUSEY);
                	    	     if((xo != *bx) || (yo != *by)) {
                    	    	          drawmode(MACHMODE);
					  /* erase the previous rectangle */
                    	    	          color(BLACK); 
                    	    	          recti(*ax, *ay, xo, yo);
					  /* draw the new rectangle */
                    	    	          color(RED);
                    	    	          recti(*ax, *ay, *bx, *by);
                    	    	          drawmode(NORMALDRAW);
                	    	     }

			    	}
                	    	break;
			
                            default:
                                break;
                        }

                    } else {

                               /* LEFTMOUSE has now been released so
                                * we need to 
                                * A: get new mouse position
                                * B: test to see if mouse is currently "in-
                                *    side" snapshot button window and if it was
                                *    < 1 pixel away from where it was pressed 
                                *    down.  if yes, this means we need need to 
                                *    pop the snapshot button and restore the 
                                *    previous rubberband and its attributes.
                                * C: current position indicates something other
                                *    than a winpop is desired which implies 
                                *    that a rubberband is already specified, so
                                *    nowindowyet is no longer true and mode 
                                *    becomes changing.
                                */

              /* A */   upmx = getvaluator(MOUSEX);
                        upmy = getvaluator(MOUSEY);

              /* B */   if (((abs(downmx-upmx) < 1) && (abs(downmy-upmy) < 1))
                         &&  (curxorig<=downmx && downmx<=curxorig+sx)
                         &&  (curyorig<=downmy && downmy<=curyorig+sy)
                         &&  (curxorig<=upmx && upmx<=curxorig+sx)
                         &&  (curyorig<=upmy && upmy<=curyorig+sy)) {

                            if (nowindowyet) {     /* no explicit rubberband */
                                mode = STARTING;      /* yet so return to    */
                                setcursor(CAMERA, 0, 0); /* STARTING mode    */
                            } else
                                mode = CHANGING;
                            winpop();
                            qreset();
                            draw_button();     
                            x1 = px1; y1 = py1;     /* since we only want to */
                            x2 = px2; y2 = py2;     /* pop the button window,*/
                            ax = &x1; ay = &y1;     /* restore the previous  */
                            bx = &x2; by = &y2;     /* rubberband position   */
                            drawmode(MACHMODE);     /* and then redraw it    */
			    color(BLACK);
                            recti(x1, y1, upmx, upmy);
                            recti(upmx, upmy, x2, y2);
                            recti(x1, upmy, mx, y2);
                            recti(upmx, y1, x2, upmy);
                            color(RED);
                            recti(x1, y1, x2, y2);
                            drawmode(NORMALDRAW);

              /* C */   } else {
                            nowindowyet = FALSE;
                            mode = CHANGING;
                        }
                    }

                    break;

                case MIDDLEMOUSE:
                    if (val) {
                        mx = getvaluator(MOUSEX);
                        my = getvaluator(MOUSEY);
                        getlocation();
                        if (xin&&yin&&xout&&yout) { 
                            xc = getvaluator(MOUSEX); 
                            yc = getvaluator(MOUSEY);
                            mode = MOVING;
                	    while(getbutton(MIDDLEMOUSE)){
                	        xo = xc;              
				yo = yc;
                	        xc = getvaluator(MOUSEX); 
				yc = getvaluator(MOUSEY);
                	        if((xo != xc) || (yo != yc)) {
                    	            drawmode(MACHMODE);
				    /* erase the previous rectangle */
                    	            color(BLACK);    
                    	            recti(x1, y1, x2, y2);
				    /* compuke the delta x and y */
                    	            xd = xc - xo;  
                    	            x1 += xd; x2 += xd;
                    	            yd = yc - yo;
                    	            y1 += yd; y2 += yd;
				    /* draw the current rectangle */
                    	            color(RED);  
                    	            recti(x1, y1, x2, y2);
                    	            drawmode(NORMALDRAW);
                	        }
                	    }
			}
                    } else 
                        mode = CHANGING;
                    break;

                case RIGHTMOUSE:
                    if (val) {
                        buildmenu(filename);
                        menuval = dopup(menu);
                        switch(menuval) {
                            case SAVE:
                                makescreenvals();
                                sprintf(cmd,"scrsave %s %d %d %d %d",
                                                 filename,sx1,sx2,sy1,sy2);
                                curstype(C16X1);
                                percentdone(50.0);
                                grabbing_image = TRUE;
                                draw_button();
                                if (nowindowyet) {
  /* this is the one case     */    drawmode(MACHMODE);
  /* where, since the full    */    color(BLACK);
  /* screen is to snapshoted, */    recti(0,0,xmaxscrn,ymaxscrn);
  /* we will first erase the  */    drawmode(NORMALDRAW);
  /* red rubberband, and then */    sprintf(cmd,"scrsave %s %d %d %d %d",
  /* call scrsave with 0-MAX  */            filename,0,xmaxscrn,0,ymaxscrn);
  /* screen values to make a  */    system(cmd);
  /* complete screen image, & */    drawmode(MACHMODE);
  /* restore the rubberband   */    color(RED);
  /* to its RED color         */    recti(0,0,xmaxscrn,ymaxscrn);
  /*    else  we will just    */    drawmode(NORMALDRAW);
  /* snap what is exactly in- */} else 
  /* side the red rubberband  */    system(cmd);
                                if (bell)  ringbell();
                                grabbing_image = FALSE;
                                draw_button();
                                percentdone(100.0);
                                madeimgyet = TRUE;
                                if (nowindowyet)
                                    setcursor(CAMERA, 0, 0);
                                break;

                            case NEWFILENAME:
                                    /* prompt to get file name */
                                getUserString("File: ",aString,sizeof(aString));
                                if  (*aString) { /* if *aString actually has 
                                                  * something in it (i.e. more
                                                  * was done than simply press-
                                                  * ing carriage return) then 
                                                  * go ahead and update 
                                                  * filename.
                                                  */
                                    strcpy(filename, aString);
                                    madeimgyet = FALSE; /* this is so the
                                                         * Ipaste menu selec-
                                                         * tion is correctly
                                                         * accessible or not
                                                         */
                                }
                                break;

                            case IPASTEFILENAME:
                                sprintf(cmd,"ipaste %s -n", filename);
                                system(cmd);
                                break;

                            case RUBBERBAND:
				/* give the popup menu a
				chance to go away first */
				sginap(20);
                    		drawmode(MACHMODE);
                    		color(RED);
                    		recti(x1, y1, x2, y2);
                    		drawmode(NORMALDRAW);
                                break;

                            case SAVE_EXIT:
                                makescreenvals();
                                sprintf(cmd,"scrsave %s %d %d %d %d",
                                                filename,sx1,sx2,sy1,sy2);
                                percentdone(50.0);
                                grabbing_image = TRUE;
                                draw_button();
                                system(cmd);
                                if (bell)  ringbell();
                                percentdone(100.0);
                                grabbing_image = FALSE;
                                draw_button();

                            case EXIT:
  /* Be sure to only clear */   drawmode(MACHMODE);
  /* the currently defined */   color(BLACK);
  /* RED rubberband--don't */   recti(x1, y1, x2, y2);
  /* disturb any other area*/   drawmode(NORMALDRAW);
  /* of the overlay bit-   */   gexit();   
  /* planes before exiting */   exit(0);
                                break;
                        }
                    } else
                        setcursor(CAMERA, 0, 0);
                    break;

                case INPUTCHANGE:
                    if (val) {
                        fullscrn();
                        mode = prevmode;  /* restore those magic mode things */
                    } else {
                        endfullscrn();
                        prevmode = mode;     /* save those magic mode things */
                        mode = STARTING;   /* don't busy-process that input! */
                    }
                    break;

                case REDRAW:
                    draw_button();
                    drawmode(MACHMODE);
                    color(RED);
                    recti(x1, y1, x2, y2);
                    drawmode(NORMALDRAW);
                    break;

                default:
                    break;
            }


    }   /* end while(TRUE) */
}     /* end main */



/*
 * figure machine:  
 *
 *	we need to determine if this machine has only POPUP planes, this
 *	means we can't do our red rubberbanding in non-existent OVERLAY
 *	bitplanes, but must instead confine ourselves to the PUPDRAW
 *	drawmode and deal with a limited architecture as best as we can.
 */

figure_machine() {
    char str[256];

    gversion(str);
    if ((getgdesc(GD_BITS_OVER_SNG_CMODE) < 2) ||
	 (strncmp(str,"GL4DGT",6) == 0))
	MACHMODE = PUPDRAW;
    else {
	MACHMODE = OVERDRAW;
	overlay(2);  
	gconfig();
    }
}


/*
 * makescreenvals:  
 *
 *    now that it has been decided that the under/overlay bitplanes are 
 *    "readable", it is necessary to "shrink" every edge of the current 
 *    "area of interest" inside the red rubberband, or else this red 
 *    border will also show up in the image output file.
 *    the only case this doesn't occur in is when the user has initially
 *    fired up snapshot, and wants to make an image of the entire screen.
 *    in this case, the red rectangle that marks the edge of the screen
 *    is first erased, the full screen is read, and then that red rectangle
 *    is again re-painted.
 */
makescreenvals() {  
    if (x1 < x2) {
         sx1 = x1 + 1;
         sx2 = x2 - 1;
    } else {
         sx1 = x2 + 1;
         sx2 = x1 - 1;
    }
    if (y1 < y2) {
         sy1 = y1 + 1;
         sy2 = y2 - 1;
    } else {
         sy1 = y2 + 1;
         sy2 = y1 - 1;
    }
}




/* 
 *   buildmenu:
 *
 *     build a new menu each time the user presses RIGHTMOUSE to reflect 
 *     any change in the output file name, as well as the current status
 *     of the "paste-ability" of the image, but using setpup.
 */

buildmenu(curname)
char curname[];
{
    char buf[256], filenmbuf[40];

    freepup(menu);
    if (nowindowyet)
        sprintf(buf,"snapshot %%t|Save scrn as %s|New file name|Ipaste %s|Redraw Rubberband|Save and Exit|Exit",curname, curname);
    else 
        sprintf(buf,"snapshot %%t|Save as %s|New file name|Ipaste %s|Redraw Rubberband|Save and Exit|Exit",curname, curname);
    menu = defpup(buf);
    if (!madeimgyet)
        setpup(menu, 3, PUP_GREY);               /* show ipaste not enabled */
    else
        setpup(menu, 3, PUP_NONE);                    /* ipaste now enabled */
#ifdef foo
    if (!madeimgyet)
        greypup(menu, 3, 0);               /* show ipaste not enabled */
    else
        greypup(menu, 3, 1);                    /* ipaste now enabled */
#endif
}




/*  
 *  getlocation:
 *
 *       determines where the mouse currently is.  at the time this function
 *       is invoked, the LEFTMOUSE has been pressed down and now the position
 *       of the mouse relative to the currently defined "area of interest"
 *       must be determined so that the appropriate mode may be assigned.
 *       below is a diagram of the possible 
 *                               .     .               .     .
 *                               |<---------- xout --------->|
 *                               .     .               .     .
 *                               .     |<---- xin ---->|     .
 *                               .     .               .     .
 *                      ............................................._.
 *                               .     .               .     .       ^
 *         STRETCHINGCORNER ==   .   _____________________   .       |
 *    ((xout&&yout) && (!xin&&!yin))|  .               .  |  .       |
 *                               .  |  .               .  |  .       |
 *                      ............|.....................|......._....
 *                               .  |  .               .  |  .    ^  |
 *                               .  |  .               .  |  .    |  |
 *          STRETCHINGXSIDE ==   .  |  .               .  |  .    |  |
 *     ((!xin&&xout) && (yin&&yout))|  .               .  |  .    |  |
 *                               .  |  .   MOVING ==   .  |  .    | yout
 *                               .  |  .((xin&&xout) &&.  |  .    |  |
 *                               .  |  . (yin&&yout))  .  |  .   yin |
 *  (!xin&& yin&&!xout&& yout)   .  |  .               .  |  .    |  |
 *               ^               .  |  .               .  |  .    |  |
 *               |               .  |  .               .  |  .    |  |
 * ---------------               .  |  .               .  |  .    V  |
 * |  ..............................|.....................|......._....
 * |                             .  |  .               .  |  .       |
 * |(!xin&&!yin&&!xout&& yout)   .  |_____________________|  .       |
 * |             ^               .    STRECTCHINGYSIDE ==    .       V
 * |  ...........|...............((!yin&&yout) && (xin&&xout))......._.
 * |             |               .     .               .     .
 * | -------------               .     .               .     .
 * | |                           .     .               .     .
 * | |(!xin&&!yin&&!xout&&!yout) .     .               .(!xin.
 * | |  ^                        .     .               . &&  . 
 * | |  |                        .     .               . xout. 
 * | |  |                        .     . (xin&&!yin&&  . &&  .
 * | |  |                        .     .  xout&&!yout) .!yout.
 * | |  |                        .     .     ^         . &&  .
 * | |  |                        .     .     |         .!yin).
 * | |  |                        .     . -----         .  ^  .
 * | |  |                        .     . |             .  |  .
 * | |  |                                |  ---------------
 * | |  |  STRETCHINGCORNER ==           |  |
 * | | ((!xin&&!yin&&!xout&&!yout) ||    |  |  in each of these five cases,
 * | ---(!xin&&!yin&&!xout&& yout) ||    |  |  if the LEFTMOUSE has been
 * -----(!xin&& yin&&!xout&& yout) ||    |  |  pressed, this means that the
 *      ( xin&&!yin&& xout&&!yout) || ----  |  user now wishes to start or
 *      (!xin&&!yin&& xout&&!yout)) ---------  define an entirely new "area 
 *                                             of interest".  any of the five 
 *                                             cases possible provide the 
 *                                             clue needed to implement the 
 *                                             logic that goes with
 *                                             STRECTHINGCORNER.
 */

getlocation() {

    if (x1<x2) {
        if((x1-limit<mx)&&(mx<x2+limit)) {
            xout = TRUE;
            if ((x1+limit<mx)&&(mx<x2-limit))
                xin = TRUE;
            else
                xin = FALSE;
        } else {
            xout = FALSE;
            xin  = FALSE;
        }
    } else {
        if((x2-limit<mx)&&(mx<x1+limit)) {
            xout = TRUE;
            if ((x2+limit<mx)&&(mx<x1-limit))
                xin = TRUE;
            else
                xin = FALSE;
        } else {
            xout = FALSE;
            xin  = FALSE;
        }
    }
    if (y1<y2) {
        if((y1-limit<my)&&(my<y2+limit)) {
            yout = TRUE;
            if ((y1+limit<my)&&(my<y2-limit))
                yin = TRUE;
            else
                yin = FALSE;
        } else {
            yout = FALSE;
            yin  = FALSE;
        }
    } else {
        if((y2-limit<my)&&(my<y1+limit)) {
            yout = TRUE;
            if ((y2+limit<my)&&(my<y1-limit))
                yin = TRUE;
            else
                yin = FALSE;
        } else {
            yout = FALSE;
            yin  = FALSE;
        }
    }
}




/*  
 *  getcursloc:  
 *
 *    determine where the cursor currently is and then sets the current 
 *    cursor based on this location, as well as queueing and unqueueing
 *    MIDDLEMOUSE so that one can move the red rectangle via this pro-
 *    gram.
 */

getcursloc() {

    int leftx, rightx, lowy, uppery, midx, midy;
    static int curxcurs, curycurs;
    int xo, yo;

    leftx = rightx = lowy = uppery = midx = midy = 0;
    xo = curxcurs;  yo = curycurs;
    curxcurs = getvaluator(MOUSEX);
    curycurs = getvaluator(MOUSEY);
    if ((xo != curxcurs) || (yo != curycurs)) {

        if (x1<x2) {
            if((x2-limit<curxcurs)&&(curxcurs<x2+limit))
                rightx = TRUE;
            else if((x1-limit<curxcurs)&&(curxcurs<x1+limit))
                leftx = TRUE;
            else if((x1+limit<=curxcurs)&&(curxcurs<=x2-limit))
                midx = TRUE;
        } else {
            if((x1-limit<curxcurs)&&(curxcurs<x1+limit))
                rightx = TRUE;
            else if((x2-limit<curxcurs)&&(curxcurs<x2+limit))
                leftx = TRUE;
            else if((x2+limit<=curxcurs)&&(curxcurs<=x1-limit))
                midx = TRUE;
        }
        if (y1<y2) {
            if((y1-limit<curycurs)&&(curycurs<y1+limit))
                lowy = TRUE;
            else if((y2-limit<curycurs)&&(curycurs<y2+limit))
                uppery = TRUE;
            else if((y1+limit<=curycurs)&&(curycurs<=y2-limit))
                midy = TRUE;
        } else {
            if((y2-limit<curycurs)&&(curycurs<y2+limit))
                lowy = TRUE;
            else if((y1-limit<curycurs)&&(curycurs<y1+limit))
                uppery = TRUE;
            else if((y2+limit<=curycurs)&&(curycurs<=y1-limit))
                midy = TRUE;
        }
        if (rightx && lowy)
            setcursor(LORIGHT, 0, 0);
        else if (leftx && lowy)
            setcursor(LOLEFT, 0, 0);
        else if (rightx && uppery)
            setcursor(UPRIGHT, 0, 0);
        else if (leftx && uppery)
            setcursor(UPLEFT, 0, 0);
        else if (midx && midy) {
            setcursor(MOVING, 0, 0);
            qdevice(MIDDLEMOUSE);
        }
        else if (midx && !midy) {
            if(lowy || uppery)
                setcursor(HORIZONTAL, 0, 0);
            else {
                setcursor(CAMERA, 0, 0);
                unqdevice(MIDDLEMOUSE);
            }
        } else if (!midx && midy) {
            if (leftx || rightx) 
                setcursor(VERTICAL, 0, 0);
            else {
                setcursor(CAMERA, 0, 0);
                unqdevice(MIDDLEMOUSE);
            }
        }
        else {
            setcursor(CAMERA, 0, 0);
            unqdevice(MIDDLEMOUSE);
        }
    }
}


/*  the points and lines arrays that follow are the hack that makes the
 *  use of the font library libfm_s.a no longer necessary.  this way,
 *  the executable is smaller which is, of course, very swank.
 */

#define NUMPOINTS 67
static long points[NUMPOINTS][2] = {
 {23,14},{23,17},{23,21},{24,13},{24,21},{25,13},{25,17},{25,20},
 {28,13},{28,21},{31,13},{31,19},{32,20},{33,20},{33,21},{34,21},{36,13},
 {40,13},{40,17},{41,13},{41,14},{41,17},{42,14},{42,18},{44,13},{45,13},
 {47,9},{47,21},{50,9},{50,14},{50,20},{51,13},{52,13},{52,14},{52,20},{53,14},
 {57,14},{57,17},{57,21},{58,13},{58,21},{59,13},{59,17},{59,20},
 {62,13},{62,25},{65,13},{65,19},{66,20},{67,20},{67,21},{68,21},{70,13},
 {73,14},{73,20},{74,14},{74,20},{77,14},{77,20},{78,14},{78,20},
 {81,21},{83,23},{84,14},{84,21},{85,21},{86,14},
};

#define NUMLINEVERTS 86
#define NUMLINES 43
static long lines[NUMLINEVERTS][2] = {
 {22,13},{22,15}, {22,18},{22,20}, {23,18},{23,20}, {24,16},{24,18}, 
                  {25,14},{25,16}, {26,14},{26,16}, {26,19},{26,21}, 
 {29,13},{29,21}, {30,13},{30,21}, {34,13},{34,20}, {35,13},{35,20},
 {39,14},{39,16}, {39,19},{39,20}, {40,14},{40,16}, {40,19},{40,20}, 
                  {40,21},{43,21}, {43,14},{43,20}, {44,14},{44,20},
  {48,9},{48,21},  {49,9},{49,21}, {51,21},{53,21}, {53,15},{53,20}, 
                  {54,15},{54,20},
 {56,13},{56,15}, {56,18},{56,20}, {57,18},{57,20}, {58,16},{58,18}, 
                  {59,14},{59,16}, {60,14},{60,16}, {60,19},{60,21},
 {63,13},{63,25}, {64,13},{64,25}, {68,13},{68,20}, {69,13},{69,20},
 {72,15},{72,19}, {73,15},{73,19}, {74,13},{77,13}, {74,21},{77,21},
                  {78,15},{78,19}, {79,15},{79,19},
 {82,14},{82,22}, {83,14},{83,22}, {83,13},{85,13},
};


/*
 *  draw_button:
 *
 *     get out of fullscreen mode, and draw a bulbous_rectfi in the whole
 *     area of our snapshot button noborder window, and then put the 
 *     program title in the center of this window.
 */

draw_button() {

    int i;


    endfullscrn();
    reshapeviewport();
    getorigin(&curxorig, &curyorig);
    viewport(0, sx, 0, sy);
    ortho2(-0.5,sx+0.5,-0.5,sy+0.5);

    color(BLACK);
    bulbous_rectfi(0, 0, sx, sy);

    if (grabbing_image)             /* if an image is currently being made, */
        color(RED);                 /* paint the string "snapshot" as red   */
    else                            /* to indicate "all systems STOPPED",   */
        color(WHITE);               /* else paint it white to signal ready  */

/* now draw the word "snapshot" in the button */
    for (i=0; i<NUMPOINTS; i++) {
        bgnpoint();
            v2i(points[i]);
        endpoint();
    }
    for (i=0; i<NUMLINEVERTS; i+=2) {
        bgnline();
            v2i(lines[i]);
            v2i(lines[i+1]);
        endline();
    }

    fullscrn();
}




/*
 *    getUserString:
 *
 *       This is a very cheap, and very rudimentary one-line "textport"
 *   that employs one basic entry-point routine called
 *
 *                getUserString(prompt, result, maxlength) 
 *                char *prompt, *userStr;
 *                int maxlen;
 *
 *   which leaves things as it found them and has a fairly general interface.
 *   getUserString clears the prompt, moves to the start of the prompt box, 
 *   and output the requested (or typed-in) prompt.
 *
 *   It drops all events on the floor if they are not keyboard events. 
 *   (i.e. it loses REDRAW and RIGHTMOUSE and anything else that comes down 
 *   the pipe before the user responds.)
 *
 *   It is also very modal:  once you call it you are stuck till you give it 
 *   what it wants (a carriage return). This is very bad, but perhaps can be 
 *   lived with in certain situations.
 *
 *                  Peter Broadwell & dave ratcliffe   -  1989
 *
 */

getUserString(prompt,userStr,maxlen)                  
    char *prompt, *userStr;
    int maxlen;
{

/* lower left corner of prompt box */
#define FILEX (xoffset+5)
#define FILEY (curyorig+7)
#define FILEYHI (curyorig+37)                /* 30 pixels hi */
#define TEXTX (xoffset+8)
#define TEXTY (curyorig+18)
#define clearprompt(aprmpt)                                                \
    color(PUP_WHITE); clear(); color(PUP_BLACK); linewidth(2);                \
    recti((FILEX-3),(FILEY+2),(xoffset+198),(FILEYHI-2));               \
    linewidth(1); cmov2i(TEXTX, TEXTY); charstr(aprmpt); 

    int cur_str_len;
    short c; 
    Device dev;
    long maxwidth;        /* max length of window's width in pixels */
    char *str;
    char *prmpt = prompt, keyBoardWasQueued;
    long oldmode, xoffset;
    Screencoord mask1, mask2, mask3, mask4;
    char currentstr[40];



                /* save old state to restore latter */
    pushmatrix();
    if ((35 < curxorig) && (curxorig < 1145))
         xoffset = curxorig - 42;
    else if (curxorig > 1080)
         xoffset = curxorig - 77;
    else
         xoffset = 0;

    oldmode = getdrawmode();
    getscrmask(&mask1, &mask2, &mask3, &mask4);
    keyBoardWasQueued = isqueued(KEYBD);
                /* enable overlay, and be sure rubber band box will be red */
    drawmode(PUPDRAW);
                /* figure out how big we are this time around */
    ortho2(-0.5,xmaxscrn+0.5,-0.5,ymaxscrn+0.5);

    userStr[0] = '\0';
    maxwidth = 200; /*(wxsize-11) - (FILEX + strwidth(prompt));*/
    scrmask((Screencoord) xoffset, (Screencoord) (xoffset+200), FILEY, FILEYHI);
        
    /* display prompt */
    cur_str_len = strlen(userStr);
    clearprompt(prmpt);

    qdevice(KEYBD);
    /* read till eof ('\n' or '\r') */
    while(dev = qread(&c)) {
        if(dev != KEYBD)
            continue;        /* don't care */
        switch(c) {
            case '\027':        /* ^W sets cursor back to start */
                cur_str_len = 0;
                clearprompt(prmpt);
                break;
            case '\n':
            case '\r':
                goto done;
            case '\b':
                if(cur_str_len) {
                    userStr[--cur_str_len] = '\0';
                    clearprompt(prmpt);
                    /* display rightmost portion */
                    for(str=userStr; *str && strwidth(str) > maxwidth; str++)
                        ;
                    charstr(str);
                }
                break;
            default:
                if(cur_str_len < (maxlen -1)) {
                    str = &userStr[cur_str_len];
                    userStr[cur_str_len++] = c;
                    userStr[cur_str_len] = '\0';
                    charstr(str);
                }
                else {
                    if (bell)  ringbell();
                }
                break;
        }
    }
done:
    if(!keyBoardWasQueued) unqdevice(KEYBD);
    scrmask(mask1, mask2, mask3, mask4);              /* restore old */
    drawmode(PUPDRAW);
    color(PUP_CLEAR);
    clear();
    drawmode(oldmode);
    popmatrix();
    userStr[maxlen] = '\0';
}
