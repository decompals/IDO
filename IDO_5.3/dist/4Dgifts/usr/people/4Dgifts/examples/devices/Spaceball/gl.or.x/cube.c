/*
 * cube.c - use Spaceball interaction to manipulate a cube in space.
 *
 * If this C file is compiled with USE_XINPUT defined, the resulting
 * program will be "mixed mode".  That is, it will use the GL for
 * drawing, but it will get its events from X.  (Specifically, the X
 * input extension will used for getting Spaceball events.)
 *
 * Version 2.1   25-APR-91
 */
#ifdef USE_XINPUT
#include <X11/Xlib.h>
#include <X11/extensions/XI.h>
#include <X11/extensions/XInput.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <gl/glws.h>
#include "glxhelper.h"
#endif /* USE_XINPUT */

#include <stdio.h>
#include <string.h>

#include <gl.h>
#include <device.h>
#include <gl/spaceball.h>

void Init(void), Cleanup(void);
void process_spaceball_data(short []);
void draw_geom(void);

#ifdef USE_XINPUT
int process_input(XEvent *);
#else
int process_input(Device, short);
#endif /* USE_XINPUT */

/* GLOBAL DATA USED THROUGHOUT */
#ifdef USE_XINPUT

Display *display;
Window root,mywindow;
XDeviceInfoPtr list;
XInputClassInfo *ip;
XEvent event;
int major_code,
	spaceball_ball_event_type,
	spaceball_button_press_event_type,
	spaceball_button_release_event_type,
	first_err,
	error_code,
	expect = Success;
XDevice *spaceball_device = NULL;

char *ext_errors[] = {
    "BadDevice",
    "BadEvent",
    "BadMode",
    "DeviceBusy",
    "BadClass"
};

char *std_errors[] = {
    "Success",
    "BadRequest",
    "BadValue",
    "BadWindow",
    "BadPixmap",
    "BadAtom",
    "BadCursor",
    "BadFont",
    "BadMatch",
    "BadDrawable",
    "BadAccess",
    "BadAlloc",
    "BadColor",
    "BadGC",
    "BadIDChoice",
    "BadName",
    "BadLength",
    "BadImplementation"
};
char *extfuncs[] = {
    "undefined",
    "GetExtensionVersion",
    "ListInputDevices",
    "OpenDevice", 
    "CloseDevice",
    "SetDeviceMode",
    "SelectExtensionEvent",
    "GetSelectedExtensionEvents", 
    "ChangeDeviceDontPropagateList",
    "GetDeviceDontPropagateList",
    "GetDeviceMotionEvents",
    "ChangeKeyboardDevice",
    "ChangePointerDevice",
    "GrabDevice",
    "UngrabDevice",
    "GrabDeviceKey",
    "UngrabDeviceKey",
    "GrabDeviceButton",
    "UngrabDeviceButton",
    "AllowDeviceEvents",
    "GetDeviceFocus",
    "SetDeviceFocus",
    "GetFeedbackControl",
    "ChangeFeedbackControl",
    "GetDeviceKeyMapping",
    "ChangeDeviceKeyMapping",
    "GetDeviceModifierMapping",
    "SetDeviceModifierMapping",
    "GetDeviceButtonMapping",
    "SetDeviceButtonMapping",
    "QueryDeviceState"
};

#else /* USE_GL_INPUT */

int wid;	/* Window IDentifier */

#endif /* USE_XINPUT */

int redisplay;

float object_scale = 1.0;
float current_matrix[4][4] = {
    { 1.,0.,0.,0.},
    { 0.,1.,0.,0.},
    { 0.,0.,1.,0.},
    { 0.,0.,0.,1.},
};

float identity_matrix[4][4] = {
    { 1.,0.,0.,0.},
    { 0.,1.,0.,0.},
    { 0.,0.,1.,0.},
    { 0.,0.,0.,1.},
};

float	translation_rate =  0.000001;
float	rotation_rate 	 =  0.000001;
float	zoom_rate 	 = -0.0000001;

/* GEOMETRY */
struct v_s {
    float verts[3];
} verts[] = {
     { 1, 1, 1},
     {-1, 1, 1},
     {-1,-1, 1},
     { 1,-1, 1},
     { 1, 1,-1},
     {-1, 1,-1},
     {-1,-1,-1},
     { 1,-1,-1}
};

struct	geom_st	{
    struct v_s *pvertex[4];
    int color;
} geom[6] = {
    /* cube = 6 sides */
    {{ &verts[0], &verts[1], &verts[2], &verts[3]}, 2}, /* front      */
    {{ &verts[0], &verts[3], &verts[7], &verts[4]}, 3}, /* right side */
    {{ &verts[4], &verts[7], &verts[6], &verts[5]}, 4}, /* back       */
    {{ &verts[5], &verts[6], &verts[2], &verts[1]}, 5}, /* left side  */
    {{ &verts[0], &verts[4], &verts[5], &verts[1]}, 6}, /* top        */
    {{ &verts[2], &verts[6], &verts[7], &verts[3]}, 7}  /* bottom     */
};

char *pname;

main(int argc, char *argv[])
{
    int done = FALSE;
    char *slashp;

    /* set pname = program name */
    pname = argv[0];
    slashp = strrchr(pname, '/');
    if (slashp && slashp[1] != '\0')
	    pname = slashp + 1;

    Init();

#ifdef USE_XINPUT
    /*
     * With X input, init redisplay = FALSE, so that drawing is not
     * attempted until the first Expose event is received.
     * That way, we don't draw until the MapWindow was successful.
     */
    redisplay = FALSE;
    while (!done) {
       /* Prompting not needed with USE_XINPUT */

       if (redisplay) {
	    redisplay = FALSE;
	    draw_geom();
       }

       /* loop to process all pending data before redisplaying image */
       do {
	  /* if no input is ready XNextEvent will wait */
	    XNextEvent(display, &event);
	    done = process_input(&event);
       } while (!done && XPending(display) > 0);   /* any more input? */
    }
#else /* USE_GL_INPUT */
    redisplay = TRUE;
    while (!done) {
       Device dev;
       short data;

       if (redisplay) {
	    redisplay = FALSE;
	    draw_geom();
       }

       /* loop to process all pending data before redisplaying image */
       do {
	    /* if no input is ready, qread() will wait */
	    dev = qread(&data);
	    done = process_input(dev, data);
       } while (!done && qtest());		/* any more input? */
    }
#endif /* USE_XINPUT */

    Cleanup();
    exit(0);
} /* end of main */


void Init(void)		/* Initializes gl and the Spaceball */    
{
#ifdef USE_XINPUT
    XDeviceInfoPtr list;
    XDevice *dev, *XOpenDevice();
    XEventClass     ListOfEventClass[3];
    int i, ndevices,
		spaceball_ball_event_class,
		spaceball_button_press_event_class,
		spaceball_button_release_event_class;

    /* init X */
        
    /* OPEN THE DISPLAY */
    if (!(display = XOpenDisplay(""))) {
	printf("No connection to Xserver - aborting\n");
	exit(1);
    }
    root = RootWindow(display,0);

    /* CREATE A WINDOW */
    mywindow = GLXCreateWindow(display,root,100,600,500,500,0,
				GLXcolorIndexDoubleBuffer);
    XSelectInput(display,mywindow,
	ButtonPressMask | KeyPressMask | KeyReleaseMask | ExposureMask);
        
    XMapWindow(display,mywindow);
    XStoreName(display,mywindow, pname);
        
    /* GET THE LIST OF INPUT DEVICES THAT ARE ATTACHED TO THE DISPLAY NOW */
    list = (XDeviceInfoPtr) XListInputDevices(display, &ndevices);
    for(i = 0; i < ndevices; i++) {
#if 0
	printf ("device id   = %d\n",list[i].id);
	printf ("device type = %d\n",list[i].type);
	printf ("device name = %s\n",list[i].name);
	printf ("number_of_classes = %d\n",list[i].num_classes);
#endif
	/* OPEN THE SPACEBALL DEVICE */
	if (strcmp(list[i].name, "spaceball") == NULL) {
	    /*
	     * Note that the device_id is not guaranteed to be the
	     * same all the time
	     */
	    spaceball_device = XOpenDevice(display, list[i].id);
	}
    }
    if (!spaceball_device) {
	printf("Sorry there is no Spaceball attached to this display\n");
	printf("Aborting demo\n");
	exit(1);
    }

    /* SELECT EXTENSION EVENTS FOR THE SPACEBALL */
    DeviceMotionNotify(spaceball_device,
                           
			spaceball_ball_event_type,
			spaceball_ball_event_class);
    ListOfEventClass[0] = spaceball_ball_event_class;
    DeviceButtonPress(spaceball_device,
			spaceball_button_press_event_type,
			spaceball_button_press_event_class);
    ListOfEventClass[1]=spaceball_button_press_event_class;
    DeviceButtonRelease(spaceball_device,
			spaceball_button_release_event_type,
			spaceball_button_release_event_class);
    ListOfEventClass[2]=spaceball_button_release_event_class;
    XSelectExtensionEvent(display,mywindow,ListOfEventClass,3);
        
    GLXwinset(display,mywindow);
    backface(TRUE);

#else /* USE_GL_INPUT */

    /* INIT GL */
    prefsize(500, 500);
    wid = winopen(pname);

    doublebuffer();
    gconfig();
    backface(TRUE);

    qdevice(ESCKEY);	/* used to exit program */
    qdevice(REDRAW);

    /* INIT Spaceball */
    qdevice(SBTX);
    qdevice(SBTY);
    qdevice(SBTZ);
    qdevice(SBRX);
    qdevice(SBRY);
    qdevice(SBRZ);
    qdevice(SBRZ);
    qdevice(SBPERIOD);

    qdevice(SBBUT1);
    qdevice(SBBUT2);
    qdevice(SBBUT3);
    qdevice(SBBUT4);
    qdevice(SBBUT5);
    qdevice(SBBUT6);
    qdevice(SBBUT7);
    qdevice(SBBUT8);
    qdevice(SBPICK);
#endif /* USE_XINPUT */

    printf("\nEXIT by pressing the ESC key\n\n");

} /* end of Init */


void Cleanup(void)
{
#ifdef USE_XINPUT
    XCloseDevice(display, spaceball_device);
    XDestroyWindow(display, mywindow);
    XSync(display, 0);
#else /* USE_GL_INPUT */
    winclose(wid);
#endif /* USE_XINPUT */
} /* end of Cleanup */


/*
 * draw_geom - draws the current geometry and swaps
 */
void draw_geom(void)
{
    int i;

#ifdef USE_XINPUT
    GLXwinset(display,mywindow);
#else
    winset(wid);
#endif /* USE_XINPUT */
    reshapeviewport();
    color(BLACK);
    clear();

    /* REPLACE THE TRANSFORMATION MATRIX WITH LATEST ONE FROM Spaceball */
    ortho(-5,5,-5,5,-10,10);
    multmatrix(current_matrix);
    scale( object_scale, object_scale, object_scale );

    for (i=0;i<6;i++) {
	color(geom[i].color);
	bgnpolygon();
	v3f((float *) geom[i].pvertex[0]);
	v3f((float *) geom[i].pvertex[1]);
	v3f((float *) geom[i].pvertex[2]);
	v3f((float *) geom[i].pvertex[3]);
	endpolygon();
    }
    swapbuffers();

} /* end of draw_geom */


/*
 * process_input - Waits for and processes input from the Spaceball.
 *
 * args:	dev - device number
 *		data - device data
 * returns TRUE if its time to quit (right mouse button pressed)
 */
#ifdef USE_XINPUT 
int process_input(XEvent *event)
{
    if (event->type == spaceball_ball_event_type) {
	XDeviceMotionEvent *M = (XDeviceMotionEvent *) event;
	if (M->deviceid == spaceball_device->device_id) {
	    int i;
	    static short sbdata[7]={0};
	    static long last_time=0, guess_time=TRUE;

	    for(i=0;i<M->axes_count;i++)
		*(sbdata+M->first_axis+i)=M->axis_data[i];

	    /* Calculate the PERIOD (16th's of a millisecond) */
	    sbdata[6]=16*(guess_time?50:M->time-last_time);
	    last_time=M->time;

	    guess_time=(sbdata[0]==0 &&sbdata[1]==0 &&sbdata[2]==0 
		    &&sbdata[3]==0 &&sbdata[4]==0 &&sbdata[5]==0);

	    process_spaceball_data(sbdata);

	    redisplay = TRUE;
	}
    } else if (event->type == spaceball_button_press_event_type) { 
	/* THESE ARE SPACEBALL BUTTON PRESSES */
	XDeviceButtonEvent *B = (XDeviceButtonEvent *)event;
	int butno = B->button;
	if (butno == 9)
	    printf("Spaceball PICK button pressed\n");
	else
	    printf("Spaceball button %d pressed\n", butno);
	sbbeep("c");
    } else if (event->type == spaceball_button_release_event_type) {
	/* THESE ARE SPACEBALL BUTTON RELEASES */
	XDeviceButtonEvent *b = (XDeviceButtonEvent *) event;
	int butno = b->button;
	if (butno == 9)
	    printf("Spaceball PICK button released\n");
	else
	    printf("Spaceball button %d released\n", butno);
	sbbeep("c");

	/* Reset viewing matrix on any button release */
	memcpy( current_matrix, identity_matrix, sizeof( Matrix ));
	object_scale = 1.0;
	redisplay = TRUE;
    } else if (event->type == KeyPress) {
	/* THESE ARE X KEYBOARD KEY PRESSES */
	XKeyPressedEvent *K = (XKeyPressedEvent *) event;
	KeySym sym = XKeycodeToKeysym(display, K->keycode, 0);
	if (sym == XK_Escape) {
	    printf("ESC pressed\n");
	} else
	    printf("key pressed\n");
    } else if (event->type == KeyRelease) {
	/* THESE ARE X KEYBOARD KEY RELEASES */
	XKeyReleasedEvent *k = (XKeyReleasedEvent *) event;
	KeySym sym = XKeycodeToKeysym(display, k->keycode, 0);
	if (sym == XK_Escape) {
	    printf("ESC released\n");
	    return TRUE;
	} else
	    printf("key released\n");
    } else if (event->type == ButtonPress) {
	XButtonPressedEvent *B = (XButtonPressedEvent *) event;
	printf("Button %d pressed \n", B->button);
    } else if (event->type == Expose) {
	redisplay = TRUE;
    } else {
	printf("unclassified event type = %d\n",event->type);
    }

   return FALSE;

} /* end of process_input */

#else /* USE_GL_INPUT */

int process_input(Device dev, short data)
{
    static short sbdata[7];

    if (ISSBALLBUT(dev)) {
	char *stateStr = data ? "pressed" : "released";
	if (dev == SBPICK)
	    printf("Spaceball PICK button %s\n", stateStr);
	else
	    printf("Spaceball button %d %s\n", dev - SBBASE, stateStr);
	sbbeep("c");
	memcpy( current_matrix, identity_matrix, sizeof( Matrix ));
	object_scale = 1.0;
	redisplay = TRUE;
    } else if (ISSBALL(dev)) {
       /*
	* The spaceball events are put in the queue in the
	* following order: SBTX,SBTY,SBTZ,SBRX,SBRY,SBRZ,SBPERIOD.
	*/
	if (dev == SBPERIOD) {
	    sbdata[6] = data;
	    process_spaceball_data(sbdata);
	    redisplay = TRUE;
	} else {
	    sbdata[dev - SBTX] = data;
	}
    } else if (dev == ESCKEY) {
	if (data) {
	    printf("ESC pressed\n");
	} else {
	    printf("ESC released\n");
	    return TRUE;
	}
    } else if (dev == REDRAW) {
	redisplay = TRUE;
    }
    return FALSE;
} /* end of process_input */


#endif /* USE_XINPUT */


matrix_mult_3x3_only(Matrix mat1, Matrix mat2, Matrix result)
{
    int i,j;
    Matrix tmp;

    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    tmp[i][j] = mat1[i][0] * mat2[0][j]
			+ mat1[i][1] * mat2[1][j]
			+ mat1[i][2] * mat2[2][j];
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    result[i][j] = tmp[i][j];

} /* end of matrix_mult_3x3_only */

void process_spaceball_data(short sbdata[])
{
    int i;
    Matrix mat;
    Coord rx, ry, rz;
    double exp();	/* occasionally missing in math.h */
    char sbstring[80];

    /* convert to floats */
    rx = -sbdata[3]; ry = -sbdata[4]; rz =  sbdata[5];

    /* perform orthographic projection algorithm */
    current_matrix[3][0] += sbdata[6] * translation_rate * sbdata[0];
    current_matrix[3][1] += sbdata[6] * translation_rate * sbdata[1];
    object_scale *= exp(sbdata[6] * zoom_rate * sbdata[2]);
    rotarbaxis(sbdata[6] * rotation_rate, rx, ry, rz, mat);
    matrix_mult_3x3_only(current_matrix, mat, current_matrix);
}
