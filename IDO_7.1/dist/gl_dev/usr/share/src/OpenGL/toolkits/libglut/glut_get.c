
/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <GL/glut.h>
#include "glutint.h"

/* CENTRY */
int
glutGet(GLenum param)
{
  Window win, root;
  int x, y, value;
  unsigned int width, height, border, depth;

  switch (param) {
  case GLUT_INIT_WINDOW_X:
    return __glutInitX;
  case GLUT_INIT_WINDOW_Y:
    return __glutInitY;
  case GLUT_INIT_WINDOW_WIDTH:
    return __glutInitWidth;
  case GLUT_INIT_WINDOW_HEIGHT:
    return __glutInitHeight;
  case GLUT_INIT_DISPLAY_MODE:
    /* glutInitDisplayMode takes an unsigned long, but glutGet
       returns the display mode as an int.  An unfortunate but
       harmless API type mismatch. */
    return (int) __glutDisplayMode;
  case GLUT_WINDOW_X:
    XTranslateCoordinates(__glutDisplay, __glutCurrentWindow->win,
      __glutRoot, 0, 0, &x, &y, &win);
    return x;
  case GLUT_WINDOW_Y:
    XTranslateCoordinates(__glutDisplay, __glutCurrentWindow->win,
      __glutRoot, 0, 0, &x, &y, &win);
    return y;
  case GLUT_WINDOW_WIDTH:
    if (!__glutCurrentWindow->reshape) {
      XGetGeometry(__glutDisplay, __glutCurrentWindow->win,
        &root, &x, &y,
        &width, &height, &border, &depth);
      return width;
    }
    return __glutCurrentWindow->width;
  case GLUT_WINDOW_HEIGHT:
    if (!__glutCurrentWindow->reshape) {
      XGetGeometry(__glutDisplay, __glutCurrentWindow->win,
        &root, &x, &y,
        &width, &height, &border, &depth);
      return height;
    }
    return __glutCurrentWindow->height;

#define GET_CONFIG(attrib) \
  glXGetConfig(__glutDisplay, __glutCurrentWindow->vis, \
    attrib, &value);

  case GLUT_WINDOW_BUFFER_SIZE:
    GET_CONFIG(GLX_BUFFER_SIZE);
    return value;
  case GLUT_WINDOW_STENCIL_SIZE:
    GET_CONFIG(GLX_STENCIL_SIZE);
    return value;
  case GLUT_WINDOW_DEPTH_SIZE:
    GET_CONFIG(GLX_DEPTH_SIZE);
    return value;
  case GLUT_WINDOW_RED_SIZE:
    GET_CONFIG(GLX_RED_SIZE);
    return value;
  case GLUT_WINDOW_GREEN_SIZE:
    GET_CONFIG(GLX_GREEN_SIZE);
    return value;
  case GLUT_WINDOW_BLUE_SIZE:
    GET_CONFIG(GLX_BLUE_SIZE);
    return value;
  case GLUT_WINDOW_ALPHA_SIZE:
    GET_CONFIG(GLX_ALPHA_SIZE);
    return value;
  case GLUT_WINDOW_ACCUM_RED_SIZE:
    GET_CONFIG(GLX_ACCUM_RED_SIZE);
    return value;
  case GLUT_WINDOW_ACCUM_GREEN_SIZE:
    GET_CONFIG(GLX_ACCUM_GREEN_SIZE);
    return value;
  case GLUT_WINDOW_ACCUM_BLUE_SIZE:
    GET_CONFIG(GLX_ACCUM_BLUE_SIZE);
    return value;
  case GLUT_WINDOW_ACCUM_ALPHA_SIZE:
    GET_CONFIG(GLX_ACCUM_ALPHA_SIZE);
    return value;
  case GLUT_WINDOW_DOUBLEBUFFER:
    GET_CONFIG(GLX_DOUBLEBUFFER);
    return value;
  case GLUT_WINDOW_RGBA:
    GET_CONFIG(GLX_RGBA);
    return value;
  case GLUT_WINDOW_COLORMAP_SIZE:
    GET_CONFIG(GLX_RGBA);
    if (value) {
      return 0;
    } else {
      return __glutCurrentWindow->vis->visual->map_entries;
    }
  case GLUT_WINDOW_PARENT:
    return __glutCurrentWindow->parent ?
      __glutCurrentWindow->parent->num + 1 : 0;
  case GLUT_WINDOW_NUM_CHILDREN:
    {
      int num = 0;
      GLUTwindow *children = __glutCurrentWindow->children;

      while (children) {
        num++;
        children = children->siblings;
      }
      return num;
    }
#if (GLUT_API_VERSION >= 2)
  case GLUT_WINDOW_NUM_SAMPLES:
#if defined(GLX_VERSION_1_1) && defined(GLX_SGIS_multisample)
    if (__glutIsSupportedByGLX("GLX_SGIS_multisample")) {
      GET_CONFIG(GLX_SAMPLES_SGIS);
      return value;
    } else {
      return 0;
    }
#else
    return 0;
#endif
  case GLUT_WINDOW_STEREO:
    GET_CONFIG(GLX_STEREO);
    return value;
#endif
  case GLUT_SCREEN_WIDTH:
    return DisplayWidth(__glutDisplay, __glutScreen);
  case GLUT_SCREEN_HEIGHT:
    return DisplayHeight(__glutDisplay, __glutScreen);
  case GLUT_SCREEN_WIDTH_MM:
    return DisplayWidthMM(__glutDisplay, __glutScreen);
  case GLUT_SCREEN_HEIGHT_MM:
    return DisplayHeightMM(__glutDisplay, __glutScreen);
  case GLUT_MENU_NUM_ITEMS:
    return __glutCurrentMenu->num;
  case GLUT_DISPLAY_MODE_POSSIBLE:
    {
      XVisualInfo *vi;

      vi = __glutGetVisualInfo(__glutDisplayMode);
      if (vi) {
        XFree(vi);
        return 1;
      }
      vi = __glutGetVisualInfo(__glutDisplayMode | GLUT_DOUBLE);
      if (vi) {
        XFree(vi);
        return 1;
      }
      return 0;
    }
  case GLUT_ELAPSED_TIME:
    {
      struct timeval elapsed, beginning, now;

      __glutInitTime(&beginning);
      GETTIMEOFDAY(&now);
      TIMEDELTA(elapsed, now, beginning);
      /* Return elapsed milliseconds. */
      return (int) ((elapsed.tv_sec * 1000) + (elapsed.tv_usec / 1000));
    }
  default:
    __glutWarning("invalid glutGet parameter: %d", param);
    return -1;
  }
}
/* ENDCENTRY */
