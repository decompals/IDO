/*
 * xinput.c  $Revision: 1.1 $
 *
 * Sample code which uses X, not GL, to receive mouse and keyboard input. 
 * This is useful when you want to get these events but not open a GL window 
 * to do so. This code was written specifically for full-screen SkyWriter 
 * HyperPipe applications. 
 *
 * Although it is possible to create an 'input-only' GL window(e.g.-noport), 
 * performance and cleanliness reasons prompt us to recommend the method
 * given below.
 *
*/

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
#include <gl/device.h>

#define X_to_GL_Y(y)	(the_X_y_max - (y))

Display	*theDisplay;
Window	theRoot, theWin;
int	the_X_y_max;

static Bool	glButtons[BUTCOUNT];
static int	glValuators[VALCOUNT];

static Bool	keycodeToDevice[256];

#define	NUM_BUTTONS 170

static unsigned char deviceToKeycode[NUM_BUTTONS];

static KeySym	deviceToKeysym[NUM_BUTTONS] = {
    NoSymbol,
    XK_Break,
    XK_Select,
    XK_Control_L,
    XK_Caps_Lock,
    XK_Shift_R,
    XK_Shift_L,
    XK_Escape,
    XK_1,
    XK_Tab,
    XK_Q,
    XK_A,
    XK_S,
    NoSymbol,
    XK_2,
    XK_3,
    XK_W,
    XK_E,
    XK_D,
    XK_F,
    XK_Z,
    XK_X,
    XK_4,
    XK_5,
    XK_R,
    XK_T,
    XK_G,
    XK_H,
    XK_C,
    XK_V,
    XK_6,
    XK_7,
    XK_Y,
    XK_U,
    XK_J,
    XK_K,
    XK_B,
    XK_N,
    XK_8,
    XK_9,
    XK_I,
    XK_O,
    XK_L,
    XK_semicolon,
    XK_M,
    XK_comma,
    XK_0,
    XK_minus,
    XK_P,
    XK_bracketleft,
    XK_apostrophe,
    XK_Return,
    XK_period,
    XK_slash,
    XK_equal,
    XK_grave,
    XK_bracketright,
    XK_backslash,
    XK_KP_1,
    XK_KP_0,
    XK_Linefeed,
    XK_BackSpace,
    XK_Delete,
    XK_KP_4,
    XK_KP_2,
    XK_KP_3,
    XK_KP_Decimal,
    XK_KP_7,
    XK_KP_8,
    XK_KP_5,
    XK_KP_6,
    XK_KP_F2,
    XK_KP_F1,
    XK_Left,
    XK_Down,
    XK_KP_9,
    XK_KP_Subtract,
    XK_KP_Separator,
    XK_KP_F4,
    XK_KP_F3,
    XK_Right,
    XK_Up,
    XK_KP_Enter,
    XK_space,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,

    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,

    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,

    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,

    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,

    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,
    NoSymbol,

    NoSymbol,
    NoSymbol,
    XK_Alt_L,
    XK_Alt_R,
    XK_Control_R,
    XK_F1,
    XK_F2,
    XK_F3,
    XK_F4,
    XK_F5,
    XK_F6,
    XK_F7,
    XK_F8,
    XK_F9,
    XK_F10,
    XK_F11,
    XK_F12,
    XK_Print,
    XK_Scroll_Lock,
    XK_Pause,
    XK_Insert,
    XK_Home,
    XK_Prior,
    XK_End,
    XK_Next,
    XK_Num_Lock,
    XK_KP_Divide,
    XK_KP_Multiply,
    XK_KP_Add,
};

static void flushQueue(void);

/*
 * Open up full-screen X input window. 
 * Note that ALL input events specified by the event_mask go to ONLY 
 * this window.
*/

void
openXinput()
{
    int                 theScreen_num, i, j, k;
    unsigned long       valuemask;
    unsigned long       theEventmask;
    XSetWindowAttributes attributes;

    if ( ! (theDisplay = XOpenDisplay(NULL)) ) {
	printf( "initXinput: Can't open display\n");
	exit(0);
    }

    theScreen_num = DefaultScreen( theDisplay );
    theRoot = RootWindow(theDisplay, theScreen_num);

    valuemask = CWOverrideRedirect | CWEventMask;
    theEventmask = (KeyPressMask | KeyReleaseMask | 
		    ButtonPressMask | ButtonReleaseMask | PointerMotionMask);
    attributes.event_mask =  theEventmask;
    attributes.override_redirect = True;

    theWin = XCreateWindow(theDisplay, theRoot, 0, 0, DisplayWidth(theDisplay,
	theScreen_num), DisplayHeight(theDisplay, theScreen_num), 0,
	CopyFromParent, InputOnly, CopyFromParent, valuemask, &attributes);

    the_X_y_max = DisplayHeight(theDisplay, theScreen_num) - 1;

    for(i=0; i<BUTCOUNT; i++) glButtons[i] = False;
    for(i=0; i<VALCOUNT; i++) glValuators[i] = 0;

    /*
     * Figure out mapping between X keycodes and GL device id's
    */
    bzero(&keycodeToDevice[0], sizeof(keycodeToDevice));
    bzero(&deviceToKeycode[0], sizeof(deviceToKeycode));
    for (i = 0; i < 256; i++) {
	for (j = 0; j < 2; j++) {
	    KeySym ks = XKeycodeToKeysym(theDisplay, i, j);
	    for (k = BUT0; k < NUM_BUTTONS; k++) {
		if (ks == deviceToKeysym[k]) {
		    if (keycodeToDevice[i]) {
			/*
			 * Collision between some other keysym and this
			 * device.  This means that a single keysym can
			 * decode to two (or more) keycodes, a naughty
			 * thing in my opinion.
			 */
		    } else {
			keycodeToDevice[i] = k;
			deviceToKeycode[k] = i;
			goto winner;
		    }
		}
	    }
	}
        winner:;
    }

    XMapWindow(theDisplay, theWin);

    XAutoRepeatOff(theDisplay); 
    XGrabKeyboard(theDisplay, theWin, True, 
		GrabModeAsync, GrabModeAsync, CurrentTime);
    XFlush(theDisplay);
}

void
closeXinput() 
{
    XAutoRepeatOn(theDisplay); 
    XUngrabKeyboard(theDisplay, CurrentTime);
    XFlush(theDisplay);
}

/*--------------------------------------------------------------------------*/

int
Xgetbutton(int button)
{
    if(!ISBUTTON(button)) return -1;

    if (XEventsQueued(theDisplay, QueuedAfterReading)) {
	flushQueue();
    } 
    return (glButtons[button - BUTOFFSET]);
}

int
Xgetvaluator(int val) 
{
    if(!ISVALUATOR(val)) return -1;

    if (XEventsQueued(theDisplay, QueuedAfterReading)) {
	flushQueue();
    } 
    return (glValuators[val - VALOFFSET]);
}

/*
 * Flush input queue and update glValuators and glButtons accordingly.
*/
static void
flushQueue() 
{
    XMotionEvent motion_event;
    XEvent	 event;

    while (XEventsQueued(theDisplay, QueuedAfterReading)) {
	XNextEvent(theDisplay, &event);
	switch (event.type) {
	   case MotionNotify: {
		    XMotionEvent *motion_event = (XMotionEvent *) &event;

		    glValuators[MOUSEX - VALOFFSET] = motion_event->x;
		    glValuators[MOUSEY - VALOFFSET] = X_to_GL_Y(motion_event->y);
		}
		break;
	   case ButtonPress:  {
		    XButtonEvent *button_event = (XButtonEvent *) &event;

		    switch (button_event->button) {
			case Button1:
		    	    glButtons[LEFTMOUSE - BUTOFFSET] = True;
			    break;
			case Button2:
		    	    glButtons[MIDDLEMOUSE - BUTOFFSET] = True;
			    break;
			case Button3:
		    	    glButtons[RIGHTMOUSE - BUTOFFSET] = True;
			    break;
		    }
		}
		break;
	   case ButtonRelease:  {
		    XButtonEvent *button_event = (XButtonEvent *) &event;

		    switch (button_event->button) {
			case Button1:
		    	    glButtons[LEFTMOUSE - BUTOFFSET] = False;
			    break;
			case Button2:
		    	    glButtons[MIDDLEMOUSE - BUTOFFSET] = False;
			    break;
			case Button3:
		    	    glButtons[RIGHTMOUSE - BUTOFFSET] = False;
			    break;
		    }
		}
		break;
	   case KeyPress:  {
		    XKeyEvent *key_event = (XKeyEvent *) &event;
		    unsigned char dev;

		    dev = keycodeToDevice[key_event->keycode];
		    glButtons[dev - BUTOFFSET] = True;
		}
		break;
	   case KeyRelease:  {
		    XKeyEvent *key_event = (XKeyEvent *) &event;
		    unsigned char dev;

		    dev = keycodeToDevice[key_event->keycode];
		    glButtons[dev - BUTOFFSET] = False;
		}
		break;
	}
    }
}

