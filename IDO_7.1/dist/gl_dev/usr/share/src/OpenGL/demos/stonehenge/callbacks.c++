#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>

#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "atmosphere.h"
#include "scene.h"

#include "callbacks.h"

int cb_demo_mode = 0;
float demo_time;

extern Widget glw;
extern XtAppContext app_context;
GLXContext glx_context;

static int needs_wp = 0;
static XtWorkProcId workproc = NULL;

static int winx, winy;

static int button_down = 0;
int mousex, mousey;
/* What's moving */
GLint target;

/* Location of the telescope */
GLfloat tx, ty;

/* Movements of the camera position to be applied at the next redraw */
float rot_pendx = 0, rot_pendz = 0, trans_pend = 0;

/* How fast the camera is moving */
float trans_speed = 0;

/* This is how fast demo time moves relative to real time */
GLfloat time_scale = 0;

TimeDate last_update;

float last_time = 0;
const float time_fudge = 1000;
inline unsigned long current_time()
{
  struct timeval time;
  gettimeofday(&time, NULL);
  return (time.tv_sec * 1000000 + time.tv_usec);
}

inline float clamp(float x, float min, float max)
{
  if (x < min) return min;
  else if (x > max) return max;
  else return x;
}

static void add_workproc()
{
  needs_wp++;
  if (workproc == NULL)
    workproc = XtAppAddWorkProc(app_context, drawWP, NULL);
}

static void remove_workproc()
{
  needs_wp--;
  if (needs_wp == 0) {
    XtRemoveWorkProc(workproc);
    workproc = NULL;
  } else if (needs_wp < 0) {
    fprintf(stderr, "Internal Error:  No workproc to remove!\n");
    needs_wp = 0;
    workproc = NULL;
  }
}

static void reset_viewer()
{
  scene_viewer_center();
}

void intToggleCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int *data;
  XmToggleButtonCallbackStruct *ptr;
  ptr = (XmToggleButtonCallbackStruct *)call_data;
  data = (int *)client_data;
  *data = ptr->set;
  // This redraw may or may not be needed - do it to be safe
  drawWP(NULL);
}


void initCB(Widget w)
{
  Arg args[1];
  XVisualInfo *vi;

  glw = w;

  XtSetArg(args[0], GLwNvisualInfo, &vi);
  XtGetValues(w, args, 1);
 
  glx_context = glXCreateContext(XtDisplay(w), vi, 0, GL_FALSE);
  GLwDrawingAreaMakeCurrent(w, glx_context);

  scene_init();

  last_update.read_time();

  resetViewerCB(NULL, NULL, NULL);
}

void exposeCB(Widget w)
{
  drawWP(NULL);
}

void resizeCB(Widget w, XtPointer client_data, XtPointer call)
{
  GLwDrawingAreaCallbackStruct *call_data;

  GLwDrawingAreaMakeCurrent(w, glx_context);

  call_data = (GLwDrawingAreaCallbackStruct *)call;

  winx = call_data->width;
  winy = call_data->height;
  
  glViewport(0, 0, winx, winy);
  
  aspect = (GLfloat)winx / (GLfloat)winy;
}

void inputCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  GLwDrawingAreaCallbackStruct *call;

  char buffer[5];
  int bufsize = 5;
  KeySym key;
  XComposeStatus compose;

  float dx, dy, r1, r2;

  GLwDrawingAreaMakeCurrent(w, glx_context);

  call = (GLwDrawingAreaCallbackStruct *)call_data;

  switch(call->event->type) {
  case ButtonPress:
    last_time = current_time();
    button_down = call->event->xbutton.button;
    mousex = call->event->xbutton.x;
    mousey = call->event->xbutton.y;
    /* Determine if the target should be the camera position 
     * or the telescope */
    if (use_telescope) {
      scene_get_position_telescope(&tx, &ty);
      scene_get_radius_telescope(&r1);
      dx = (tx + .5) - ((GLfloat)(winx - mousex)/(GLfloat)winx);
      dy = (ty + .5) - ((GLfloat)(winy - mousey)/(GLfloat)winy);
      r2 = sqrt(dx*dx + dy*dy);
      if (r2 < r1) target = name_telescope;
      else target = name_background;
    } else target = name_background;
    add_workproc();
    break;
  case ButtonRelease:
    if (call->event->xbutton.button == Button3) {
      /* Use Button3 to stop */
      if (trans_speed) remove_workproc();
      trans_speed = 0;
    }
    remove_workproc();
    button_down = 0;
    break;
  case MotionNotify:
    switch(button_down) {
    case Button1:
      /* Use Button1 to control the way in which the viewer is looking 
       * or to move the telescope around */
      if (target == name_background) {
	dx = (float)(call->event->xmotion.x - mousex) / (float)winx;
	dy = (float)(call->event->xmotion.y - mousey) / (float)winy;
	rot_pendx -= dy * fov;
	rot_pendz -= dx * fov;
      } else {
	dx = (float)(mousex - call->event->xmotion.x) / (float)winx;
	dy = (float)(mousey - call->event->xmotion.y) / (float)winy;
	tx += dx;
	ty += dy;
	tx = clamp(tx, -.5, .5);
	ty = clamp(ty, -.5, .5);
	scene_position_telescope(tx, ty);
      }
      break;
    case Button2:
      /* Use Button2 to change speed */
      dx = (float)(mousex - call->event->xmotion.x) / 
	(float)winx;
      if (dx && !trans_speed) add_workproc();
      else if (!dx && trans_speed) remove_workproc();
      trans_speed += dx;
      break;
    }
    mousex = call->event->xmotion.x;
    mousey = call->event->xmotion.y;
    break;
  case KeyPress:
    XLookupString(&(call->event->xkey), buffer, bufsize, &key, &compose);
    if (key == XK_Escape) exit(0);
    break;                            
  default:
    break;
  }
}

const float speed_t = .5;
const float speed_r = 15.;
const float speed_rx = 10.;
void demo_mode_update(float dt)
{
  float t;
  
  if (!cb_demo_mode) return;
  
  dt /= 1000000;
  demo_time += dt;
  
  t = demo_time;
  
  if (t < 10.5) {
    trans_speed = speed_t;
    return;
  } else t -= 10.5;
  
  if (t < 1) {
    trans_speed = 0;
    rot_pendz = 0;
    return;
  } else t -= 1;
  
  if (t < 3.) {
    trans_speed = 0.;
    rot_pendz = dt * speed_r;
    return;
  } else t -= 3.;
  
  if (t < 2.) {
    rot_pendx = -dt * speed_rx;
    return;
  } else t -= 2.;

  if (t < 2.) {
    return;
  } else t -= 2.;

  if (t < 2.) {
    rot_pendx = dt * speed_rx;
    return;
  } else t -= 2.;
  

  if (t < 3.) {
    trans_speed = 0.;
    rot_pendz = dt * speed_r;
    return;
  } else t -= 3.;

  if (t < 30.) {
    trans_speed  = speed_t;
    rot_pendz = 0;
    return;
  } else t -= 30.;
  
  if (t < 1.) {
    trans_speed = 0;
    return;
  } else t -= 1.;

  if (t < 1.3) {
    rot_pendz = -dt * speed_r;
    return;
  } else t -= 1.3;

  // Pan back to see entire thing
  if (t < 23) {
    trans_speed = speed_t;
    return;
  } else t -= 23;

  // Hold before starting over
  if (t < 20) {
    trans_speed = 0;
    return;
  }
  else t -= 20;

  demo_time = 0.;
  reset_viewer();
}

Boolean drawWP(XtPointer data)
{
  /* Right now, there's two completely independent time measurements:
   * one for the time of day in the demo and one for changing the camera
   * position */
  TimeDate t, dt;
  float elapsed_time, time;

  if (time_scale != 0.0) {
    t.read_time();
    dt = (t - last_update) * time_scale;
    scene_inc_time(dt);
    last_update = t;
  }

  time = current_time();
  if (time - last_time > time_fudge) {
    elapsed_time = time - last_time;
    demo_mode_update(elapsed_time);
    trans_pend = trans_speed * (elapsed_time / 1000000);
    last_time = time;
  }

  scene_viewer_rotatex(rot_pendx);
  scene_viewer_rotatez(rot_pendz);
  scene_viewer_translate(trans_pend);
  rot_pendx = 0;
  rot_pendz = 0;
  trans_pend = 0;

  GLwDrawingAreaMakeCurrent(glw, glx_context);
  scene_render();
  /* This is a total hack */
//  if (!use_antialias)
GLwDrawingAreaSwapBuffers(glw);

  return FALSE;
}

void weatherCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  Weather *data;
  XmToggleButtonCallbackStruct *ptr;
  ptr = (XmToggleButtonCallbackStruct *)call_data;
  if (ptr->set) scene_set_weather(*((Weather *)client_data));
  drawWP(NULL);
}

void currentTimeCB(Widget w)
{
  scene_set_time(TimeDate().read_time());
  drawWP(NULL);
}

void time10amCB(Widget w)
{
  scene_set_time(TimeDate(10, 0));
  drawWP(NULL);
}

void time12pmCB(Widget w)
{
  scene_set_time(TimeDate(12, 0));
  drawWP(NULL);
}

void time4pmCB(Widget w)
{
  scene_set_time(TimeDate(16, 0));
  drawWP(NULL);
}

void timeSpeedCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  
  if (!((XmToggleButtonCallbackStruct *)call_data)->set) return;
  time_scale =  *(int *)client_data;
  if (time_scale == 0.0) remove_workproc();
  else add_workproc();
  last_update.read_time();
}

void demo_modeCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int val  = ((XmToggleButtonCallbackStruct *)call_data)->set;
  if (!val) {
    remove_workproc();
    resetViewerCB(NULL, NULL, NULL);
    last_time = current_time();
  } else {
    reset_viewer();
    add_workproc();
    trans_speed = 0;
    demo_time = 0;
    rot_pendx = -5;
  }
  cb_demo_mode = val;
  drawWP(NULL);
}

void resetViewerCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  trans_speed = 0;
  rot_pendx = rot_pendz = trans_pend = 0;
  rot_pendx = -5;
  reset_viewer();
  return;
}
     

void exitCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  exit(0);
}
