/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996.  */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* for XA_RGB_DEFAULT_MAP atom */
#include <X11/Xmu/StdCmap.h>  /* for XmuLookupStandardColormap */

#include <GL/glut.h>
#include "glutint.h"

GLUTwindow *__glutCurrentWindow = NULL;
GLUTwindow **__glutWindowList = NULL;
int __glutWindowListSize = 0;

static GLUTwindow *__glutWindowCache = NULL;

GLUTwindow *
__glutGetWindow(Window win)
{
  int i;

  if (__glutWindowCache && win == __glutWindowCache->win) {
    return __glutWindowCache;
  }
  for (i = 0; i < __glutWindowListSize; i++) {
    if (__glutWindowList[i]) {
      if (win == __glutWindowList[i]->win) {
        __glutWindowCache = __glutWindowList[i];
        return __glutWindowCache;
      }
    }
  }
  return NULL;
}

/* CENTRY */
int
glutGetWindow(void)
{
  if (__glutCurrentWindow) {
    return __glutCurrentWindow->num + 1;
  } else {
    return 0;
  }
}
/* ENDCENTRY */

void
__glutSetWindow(GLUTwindow * window)
{
  /* It is tempting to try to short-circuit the call to
     glXMakeCurrent if we "know" we are going to make current
     to a window we are already current to.  In fact, this
     assumption breaks when GLUT is expected to integrated with
     other OpenGL windowing APIs that also make current to
     OpenGL contexts.  Since glXMakeCurrent short-circuits the
     "already bound" case, GLUT avoids the temptation to do so
     too. */
  __glutCurrentWindow = window;
  glXMakeCurrent(__glutDisplay, __glutCurrentWindow->win,
    __glutCurrentWindow->ctx);

  /* We should be careful to force a finish between each
     iteration through the GLUT main loop if indirect OpenGL 
     contexts are in use; indirect contexts tend to have  much
     longer latency because lots of OpenGL extension requests
     can queue up in the X protocol stream.  We accomplish this 

     by posting GLUT_FINISH_WORK to be done. */
  if (!__glutCurrentWindow->isDirect)
    __glutPutOnWorkList(__glutCurrentWindow, GLUT_FINISH_WORK);

  /* If debugging is enabled, we'll want to check this window
     for any OpenGL errors every iteration through the GLUT
     main loop.  To accomplish this, we post the
     GLUT_DEBUG_WORK to be done on this window. */
  if (__glutDebug)
    __glutPutOnWorkList(__glutCurrentWindow, GLUT_DEBUG_WORK);
}

/* CENTRY */
void
glutSetWindow(int win)
{
  GLUTwindow *window;

  if (win < 1 || win > __glutWindowListSize) {
    __glutWarning("glutWindowSet attempted on bogus window.");
    return;
  }
  window = __glutWindowList[win - 1];
  if (!window) {
    __glutWarning("glutWindowSet attempted on bogus window.");
    return;
  }
  __glutSetWindow(window);
}
/* ENDCENTRY */

static int
getUnusedWindowSlot(void)
{
  int i;

  /* Look for allocated, unused slot. */
  for (i = 0; i < __glutWindowListSize; i++) {
    if (!__glutWindowList[i]) {
      return i;
    }
  }
  /* Allocate a new slot. */
  __glutWindowListSize++;
  if (__glutWindowList) {
    __glutWindowList = (GLUTwindow **)
      realloc(__glutWindowList,
      __glutWindowListSize * sizeof(GLUTwindow *));
  } else {
    /* XXX Some realloc's do not correctly perform a malloc
       when asked to perform a realloc on a NULL pointer,
       though the ANSI C library spec requires this. */
    __glutWindowList = (GLUTwindow **)
      malloc(sizeof(GLUTwindow *));
  }
  if (!__glutWindowList)
    __glutFatalError("out of memory.");
  __glutWindowList[__glutWindowListSize - 1] = NULL;
  return __glutWindowListSize - 1;
}

XVisualInfo *
__glutGetVisualInfo(unsigned long mode)
{
  int list[32];
  int i = 0;

  /* XXX Would a caching mechanism to minize the calls to
     glXChooseVisual? You'd have to reference count
     XVisualInfo* pointers. */

  if (GLUT_WIND_IS_DOUBLE(mode)) {
    list[i++] = GLX_DOUBLEBUFFER;
  }
#if (GLUT_API_VERSION >= 2)
#if defined(GLX_VERSION_1_1) && defined(GLX_SGIS_multisample)
  if (GLUT_WIND_IS_MULTISAMPLE(mode)) {
    if (!__glutIsSupportedByGLX("GLX_SGIS_multisample"))
      return NULL;
    list[i++] = GLX_SAMPLES_SGIS;
    /* XXX Is 4 a reasonable minimum acceptable number of
       samples? */
    list[i++] = 4;
  }
  if (GLUT_WIND_IS_STEREO(mode)) {
    list[i++] = GLX_STEREO;
  }
#endif
#endif
  if (GLUT_WIND_IS_DOUBLE(mode)) {
    list[i++] = GLX_DOUBLEBUFFER;
  }
  if (GLUT_WIND_IS_RGB(mode)) {
    list[i++] = GLX_RGBA;
    list[i++] = GLX_RED_SIZE;
    list[i++] = 1;
    list[i++] = GLX_GREEN_SIZE;
    list[i++] = 1;
    list[i++] = GLX_BLUE_SIZE;
    list[i++] = 1;
    if (GLUT_WIND_HAS_ALPHA(mode)) {
      list[i++] = GLX_ALPHA_SIZE;
      list[i++] = 1;
    }
    if (GLUT_WIND_HAS_ACCUM(mode)) {
      list[i++] = GLX_ACCUM_RED_SIZE;
      list[i++] = 1;
      list[i++] = GLX_ACCUM_GREEN_SIZE;
      list[i++] = 1;
      list[i++] = GLX_ACCUM_BLUE_SIZE;
      list[i++] = 1;
      if (GLUT_WIND_HAS_ALPHA(mode)) {
        list[i++] = GLX_ACCUM_ALPHA_SIZE;
        list[i++] = 1;
      }
    }
  } else if (GLUT_WIND_IS_INDEX(mode)) {
    list[i++] = GLX_BUFFER_SIZE;
    list[i++] = 1;
  }
  if (GLUT_WIND_HAS_DEPTH(mode)) {
    list[i++] = GLX_DEPTH_SIZE;
    list[i++] = 1;
  }
  if (GLUT_WIND_HAS_STENCIL(mode)) {
    list[i++] = GLX_STENCIL_SIZE;
    list[i++] = 1;
  }
  list[i] = (int) None; /* terminate list */

  return glXChooseVisual(__glutDisplay,
    __glutScreen, list);
}

static void
setupColormap(GLUTwindow * win)
{
  XVisualInfo *vi;
  Status status;
  XStandardColormap *standardCmaps;
  int i, numCmaps;

  vi = win->vis;
  switch (vi->class) {
  case PseudoColor:
    if (GLUT_WIND_IS_RGB(__glutDisplayMode)) {
      /* Mesa might return a PseudoColor visual for RGB mode. */
      win->colormap = NULL;
      if (MaxCmapsOfScreen(DefaultScreenOfDisplay(__glutDisplay)) == 1
        && vi->visual == DefaultVisual(__glutDisplay, __glutScreen)) {
        char *private = getenv("MESA_PRIVATE_CMAP");

        if (private) {
          /* User doesn't want to share colormaps. */
          win->cmap = XCreateColormap(__glutDisplay, __glutRoot,
            vi->visual, AllocNone);
        } else {
          /* Share the root colormap. */
          win->cmap = DefaultColormap(__glutDisplay, __glutScreen);
        }
      } else {
        /* Get our own PseudoColor colormap. */
        win->cmap = XCreateColormap(__glutDisplay, __glutRoot,
          vi->visual, AllocNone);
      }
    } else {
      /* CI mode, real GLX never returns a PseudoColor visual
         for RGB mode. */
      win->colormap = __glutAssociateColormap(win->vis);
      win->cmap = win->colormap->cmap;
    }
    break;
  case TrueColor:
  case DirectColor:
    win->colormap = NULL;  /* NULL if RGBA */
    status = XmuLookupStandardColormap(__glutDisplay,
      vi->screen, vi->visualid, vi->depth, XA_RGB_DEFAULT_MAP,
      /* replace */ False, /* retain */ True);
    if (status == 1) {
      status = XGetRGBColormaps(__glutDisplay, __glutRoot,
        &standardCmaps, &numCmaps, XA_RGB_DEFAULT_MAP);
      if (status == 1)
        for (i = 0; i < numCmaps; i++)
          if (standardCmaps[i].visualid == vi->visualid) {
            win->cmap = standardCmaps[i].colormap;
            XFree(standardCmaps);
            return;
          }
    }
    /* If no standard colormap but TrueColor, just make a
       private one. */
    /* XXX Should do a better job of internal sharing for
       privately allocated TrueColor colormaps. */
    /* XXX DirectColor probably needs ramps hand initialized! */
    win->cmap = XCreateColormap(__glutDisplay, __glutRoot,
      vi->visual, AllocNone);
    break;
  case StaticColor:
  case StaticGray:
  case GrayScale:
    /* Mesa supports these visuals */
    win->colormap = NULL;
    win->cmap = XCreateColormap(__glutDisplay, __glutRoot,
      vi->visual, AllocNone);
    break;
  default:
    __glutFatalError(
      "could not allocate colormap for visual type: %d.",
      vi->class);
  }
  return;
}

void
__glutDefaultReshape(int width, int height)
{
  glViewport(0, 0, (GLsizei) width, (GLsizei) height);
}

GLUTwindow *
__glutCreateWindow(GLUTwindow * parent,
  int x, int y, int width, int height)
{
  GLUTwindow *window;
  XSetWindowAttributes wa;
  int winnum;
  Window win;
  int i;
  unsigned long displayMode;

  if (!__glutDisplay)
    __glutOpenXConnection(NULL);
  winnum = getUnusedWindowSlot();
  window = (GLUTwindow *) malloc(sizeof(GLUTwindow));
  if (!window)
    __glutFatalError("out of memory.");
  window->num = winnum;
  window->fakeSingle = False;
  displayMode = __glutDisplayMode;
  window->vis = __glutGetVisualInfo(displayMode);
  if (!window->vis) {
    /* Fallback cases when can't get exactly what was asked
       for... */
    if (GLUT_WIND_IS_SINGLE(displayMode)) {
      /* If we can't find a single buffered visual, try looking
         for a double buffered visual.  We can treat a double
         buffered visual as a single buffer visual by changing
         the draw buffer to GL_FRONT and treating any swap
         buffers as no-ops. */
      displayMode |= GLUT_DOUBLE;
      window->vis = __glutGetVisualInfo(displayMode);
      window->fakeSingle = True;
    }
    if (!window->vis && GLUT_WIND_IS_MULTISAMPLE(displayMode)) {
      /* If we can't seem to get multisampling (ie, not Reality
         Engine class graphics!), go without multisampling.  It 
         is up to the application to query how many multisamples
         were allocated (0 equals no multisampling) if the
         application is going to use multisampling for more than
         just anti-aliasing. */
      displayMode &= ~GLUT_MULTISAMPLE;
      window->vis = __glutGetVisualInfo(displayMode);
    }
  }
  if (!window->vis) {
    __glutFatalError(
      "visual with necessary capabilities not found.");
  }
  window->ctx = glXCreateContext(__glutDisplay, window->vis,
    None, __glutTryDirect);
  window->isDirect = glXIsDirect(__glutDisplay, window->ctx);
  if (__glutForceDirect) {
    if (!window->isDirect)
      __glutFatalError("direct rendering not possible.");
  }
  setupColormap(window);
  window->eventMask = StructureNotifyMask;

  wa.colormap = window->cmap;
  wa.background_pixmap = None;
  wa.border_pixel = 0;
  wa.event_mask = window->eventMask;
  win = XCreateWindow(__glutDisplay,
    parent == NULL ? __glutRoot : parent->win,
    x, y, width, height, 0,
    window->vis->depth, InputOutput, window->vis->visual,
    CWBackPixmap | CWBorderPixel | CWEventMask | CWColormap,
    &wa);

  window->width = width;
  window->height = height;
  window->forceReshape = True;

  window->win = win;
  window->parent = parent;
  if (parent) {
    window->siblings = parent->children;
    parent->children = window;
  } else {
    window->siblings = NULL;
  }
  window->children = NULL;
  window->display = NULL;
  window->reshape = __glutDefaultReshape;
  window->mouse = NULL;
  window->motion = NULL;
  window->visibility = NULL;
  window->passive = NULL;
  window->entry = NULL;
#if (GLUT_API_VERSION >= 2)
  window->special = NULL;
  window->buttonBox = NULL;
  window->dials = NULL;
  window->spaceMotion = NULL;
  window->spaceRotate = NULL;
  window->spaceButton = NULL;
  window->tabletMotion = NULL;
  window->tabletButton = NULL;
  window->tabletPos[0] = -1;
  window->tabletPos[1] = -1;
#endif
  window->keyboard = NULL;
  window->mapState = False;
  window->visState = -1;  /* not VisibilityUnobscured,
                             VisibilityPartiallyObscured, or
                             VisibilityFullyObscured */
  window->workMask = GLUT_MAP_WORK;
  window->desiredMapState = NormalState;
  window->desiredConfMask = 0;
  window->buttonUses = 0;
  window->prevWorkWin = __glutWindowWorkList;
  __glutWindowWorkList = window;
  for (i = 0; i < GLUT_MAX_MENUS; i++) {
    window->menu[i] = 0;
  }
  __glutWindowList[winnum] = window;
  __glutSetWindow(window);
  if (window->fakeSingle)
    glDrawBuffer(GL_FRONT);
  return window;
}

static int
findColormaps(GLUTwindow * window,
  Window * winlist, Colormap * cmaplist, int num, int max)
{
  GLUTwindow *child;
  int i;

  /* do not allow more entries that maximum number of
     colormaps! */
  if (num >= max)
    return num;
  /* is cmap for this window already on the list? */
  for (i = 0; i < num; i++) {
    if (cmaplist[i] == window->cmap)
      goto alreadyListed;
  }
  /* not found on the list; add colormap and window */
  winlist[num] = window->win;
  cmaplist[num] = window->cmap;
  num++;
  /* recursively search children */
alreadyListed:
  child = window->children;
  while (child) {
    num = findColormaps(child, winlist, cmaplist, num, max);
    child = child->siblings;
  }
  return num;
}

void
__glutEstablishColormapsProperty(GLUTwindow * window)
{
  Window *winlist;
  Colormap *cmaplist;
  Atom atom;
  Status status;
  int maxcmaps, num;

  assert(!window->parent);
  maxcmaps = MaxCmapsOfScreen(ScreenOfDisplay(__glutDisplay,
      __glutScreen));
  /* For portability reasons we don't use alloca for winlist
     and cmaplist, but we could. */
  winlist = (Window *) malloc(maxcmaps * sizeof(Window));
  cmaplist = (Colormap *) malloc(maxcmaps * sizeof(Colormap));
  num = findColormaps(window, winlist, cmaplist, 0, maxcmaps);
  if (num < 2) {
    /* property no longer needed; remove it */
    atom = XInternAtom(__glutDisplay,
      "WM_COLORMAP_WINDOWS", False);
    if (atom == None) {
      /* XXX warning? */
      return;
    }
    XDeleteProperty(__glutDisplay, window->win, atom);
  } else {
    status = XSetWMColormapWindows(__glutDisplay, window->win,
      winlist, num);
    /* XSetWMColormapWindows should always work unless the
       WM_COLORMAP_WINDOWS property cannot be intern'ed.  We
       check to be safe. */
    if (status == False)
      __glutFatalError("XSetWMColormapWindows returned False");
  }
  /* For portability reasons we don't use alloca for winlist
     and cmaplist, but we could. */
  free(winlist);
  free(cmaplist);
}

GLUTwindow *
__glutToplevelOf(GLUTwindow * window)
{
  while (window->parent) {
    window = window->parent;
  }
  return window;
}

/* CENTRY */
int
glutCreateWindow(char *title)
{
  static int firstWindow = 1;
  GLUTwindow *window;
  XWMHints *wmHints;
  Window win;
  XTextProperty textprop;

  window = __glutCreateWindow(NULL,
    __glutSizeHints.x, __glutSizeHints.y,
    __glutInitWidth, __glutInitHeight);
  win = window->win;
  /* setup ICCCM properties */
  textprop.value = (unsigned char *) title;
  textprop.encoding = XA_STRING;
  textprop.format = 8;
  textprop.nitems = strlen(title);
  wmHints = XAllocWMHints();
  wmHints->initial_state =
    __glutIconic ? IconicState : NormalState;
  wmHints->flags = StateHint;
  XSetWMProperties(__glutDisplay, win, &textprop, &textprop,
  /* only put WM_COMMAND property on first window */
    firstWindow ? __glutArgv : NULL,
    firstWindow ? __glutArgc : 0,
    &__glutSizeHints, wmHints, NULL);
  firstWindow = 0;
  XFree(wmHints);
  XSetWMProtocols(__glutDisplay, win, &__glutWMDeleteWindow, 1);
  return window->num + 1;
}

int
glutCreateSubWindow(int win, int x, int y, int width, int height)
{
  GLUTwindow *window, *toplevel;

  window = __glutCreateWindow(__glutWindowList[win - 1],
    x, y, width, height);
  toplevel = __glutToplevelOf(window);
  if (toplevel->cmap != window->cmap) {
    __glutPutOnWorkList(toplevel, GLUT_COLORMAP_WORK);
  }
  return window->num + 1;
}
/* ENDCENTRY */

void
__glutDestroyWindow(GLUTwindow * window,
  GLUTwindow * initialWindow)
{
  GLUTwindow **prev, *cur, *parent, *siblings;

  /* Recursively destroy any children. */
  cur = window->children;
  while (cur) {
    siblings = cur->siblings;
    __glutDestroyWindow(cur, initialWindow);
    cur = siblings;
  }
  /* Remove from parent's children list (only necessary for
     non-initial windows and subwindows!). */
  parent = window->parent;
  if (parent && parent == initialWindow->parent) {
    prev = &parent->children;
    cur = parent->children;
    while (cur) {
      if (cur == window) {
        *prev = cur->siblings;
        break;
      }
      prev = &(cur->siblings);
      cur = cur->siblings;
    }
  }
  /* destroy window itself */
  if (window == __glutCurrentWindow) {
    glXMakeCurrent(__glutDisplay, None, NULL);
    __glutCurrentWindow = NULL;
  }
  XDestroyWindow(__glutDisplay, window->win);
  glXDestroyContext(__glutDisplay, window->ctx);
  if (window->colormap) {
    /* Only color index windows have colormap data structure. */
    __glutFreeColormap(window->colormap);
  }
  /* NULLing the __glutWindowList helps detect is a window
     instance has been destroyed, given a window number. */
  __glutWindowList[window->num] = NULL;

  /* Remove window from "window work list" if it is there.  */
  prev = &__glutWindowWorkList;
  cur = __glutWindowWorkList;
  while (cur) {
    if (cur == window) {
      *prev = cur->prevWorkWin;
      break;
    }
    prev = &(cur->prevWorkWin);
    cur = cur->prevWorkWin;
  }
  /* Remove window from the "get window cache" if it is there. */
  if (__glutWindowCache == window)
    __glutWindowCache = NULL;
  XFree(window->vis);
  free(window);
}

/* CENTRY */
void
glutDestroyWindow(int win)
{
  GLUTwindow *window = __glutWindowList[win - 1];

  if (__glutMappedMenu && __glutMenuWindow == window) {
    __glutFatalUsage("destroying menu window not allowed while menus in use");
  }
  /* if not a toplevel window... */
  if (window->parent) {
    /* destroying subwindows may change colormap requirements;
       recalculate toplevel window's WM_COLORMAP_WINDOWS
       property */
    __glutPutOnWorkList(__glutToplevelOf(window->parent),
      GLUT_COLORMAP_WORK);
  }
  __glutDestroyWindow(window, window);
}

void
glutSwapBuffers(void)
{
  if (__glutCurrentWindow->fakeSingle) {
    /* pretend the double buffered window is single buffered, 
       so treat glutSwapBuffers as a no-op */
  } else {
    glXSwapBuffers(__glutDisplay, __glutCurrentWindow->win);
  }
}
/* ENDCENTRY */

void
__glutChangeWindowEventMask(long eventMask, Bool add)
{
  if (add) {
    /* add eventMask to window's event mask */
    if ((__glutCurrentWindow->eventMask & eventMask) !=
      eventMask) {
      __glutCurrentWindow->eventMask |= eventMask;
      __glutPutOnWorkList(__glutCurrentWindow,
        GLUT_EVENT_MASK_WORK);
    }
  } else {
    /* remove eventMask from window's event mask */
    if (__glutCurrentWindow->eventMask & eventMask) {
      __glutCurrentWindow->eventMask &= ~eventMask;
      __glutPutOnWorkList(__glutCurrentWindow,
        GLUT_EVENT_MASK_WORK);
    }
  }
}

void
glutDisplayFunc(GLUTdisplayCB displayFunc)
{
  __glutChangeWindowEventMask(ExposureMask, displayFunc != NULL);
  __glutCurrentWindow->display = displayFunc;
}

void
glutKeyboardFunc(GLUTkeyboardCB keyboardFunc)
{
  __glutChangeWindowEventMask(KeyPressMask,
    keyboardFunc != NULL || __glutCurrentWindow->special != NULL);
  __glutCurrentWindow->keyboard = keyboardFunc;
}

#if (GLUT_API_VERSION >= 2)
void
glutSpecialFunc(GLUTspecialCB specialFunc)
{
  __glutChangeWindowEventMask(KeyPressMask,
    specialFunc != NULL || __glutCurrentWindow->keyboard != NULL);
  __glutCurrentWindow->special = specialFunc;
}

#endif
void
glutMouseFunc(GLUTmouseCB mouseFunc)
{
  if (__glutCurrentWindow->mouse) {
    if (!mouseFunc) {
      /* previous mouseFunc being disabled */
      __glutCurrentWindow->buttonUses--;
      __glutChangeWindowEventMask(
        ButtonPressMask | ButtonReleaseMask,
        __glutCurrentWindow->buttonUses > 0);
    }
  } else {
    if (mouseFunc) {
      /* previously no mouseFunc, new one being installed */
      __glutCurrentWindow->buttonUses++;
      __glutChangeWindowEventMask(
        ButtonPressMask | ButtonReleaseMask, True);
    }
  }
  __glutCurrentWindow->mouse = mouseFunc;
}

void
glutMotionFunc(GLUTmotionCB motionFunc)
{
  /* Hack.  Some window managers (4Dwm by default) will mask
     motion events if the client is not selecting for button
     press and release events. So we select for press and
     release events too (being careful to use reference
     counting).  */
  if (__glutCurrentWindow->motion) {
    if (!motionFunc) {
      /* previous mouseFunc being disabled */
      __glutCurrentWindow->buttonUses--;
      __glutChangeWindowEventMask(
        ButtonPressMask | ButtonReleaseMask,
        __glutCurrentWindow->buttonUses > 0);
    }
  } else {
    if (motionFunc) {
      /* previously no mouseFunc, new one being installed */
      __glutCurrentWindow->buttonUses++;
      __glutChangeWindowEventMask(
        ButtonPressMask | ButtonReleaseMask, True);
    }
  }
  /* Real work of selecting for passive mouse motion.  */
  __glutChangeWindowEventMask(
    Button1MotionMask | Button2MotionMask | Button3MotionMask,
    motionFunc != NULL);
  __glutCurrentWindow->motion = motionFunc;
}

void
glutPassiveMotionFunc(GLUTpassiveCB passiveMotionFunc)
{
  __glutChangeWindowEventMask(PointerMotionMask,
    passiveMotionFunc != NULL);

  /* Passive motion also requires watching enters and leaves so 
     that a fake passive motion event can be generated on an
     enter. */
  __glutChangeWindowEventMask(EnterWindowMask | LeaveWindowMask,
    __glutCurrentWindow->entry != NULL || passiveMotionFunc != NULL);

  __glutCurrentWindow->passive = passiveMotionFunc;
}

void
glutEntryFunc(GLUTentryCB entryFunc)
{
  __glutChangeWindowEventMask(EnterWindowMask | LeaveWindowMask,
    entryFunc != NULL || __glutCurrentWindow->passive);
  __glutCurrentWindow->entry = entryFunc;
}

void
glutVisibilityFunc(GLUTvisibilityCB visibilityFunc)
{
  __glutChangeWindowEventMask(VisibilityChangeMask,
    visibilityFunc != NULL);
  __glutCurrentWindow->visibility = visibilityFunc;
  if (!visibilityFunc) {
    /* make state invalid */
    __glutCurrentWindow->visState = -1;
  }
}

void
glutReshapeFunc(GLUTreshapeCB reshapeFunc)
{
  if (reshapeFunc) {
    __glutCurrentWindow->reshape = reshapeFunc;
  } else {
    __glutCurrentWindow->reshape = __glutDefaultReshape;
  }
}
