
/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <GL/glut.h>
#include "glutint.h"

GLUTcolormap *__glutColormapList = NULL;

static GLUTcolormap *
associateNewColormap(XVisualInfo * vis)
{
  GLUTcolormap *cmap;
  int i;

  cmap = (GLUTcolormap *) malloc(sizeof(GLUTcolormap));
  if (!cmap)
    __glutFatalError("out of memory.");
  cmap->visual = vis->visual;
  cmap->refcnt = 1;
  cmap->size = vis->visual->map_entries;
  cmap->cells = (GLUTcolorcell *)
    malloc(sizeof(GLUTcolorcell) * cmap->size);
  /* make all color cell entries be invalid */
  for (i = cmap->size - 1; i >= 0; i--) {
    cmap->cells[i].component[GLUT_RED] = -1.0;
    cmap->cells[i].component[GLUT_GREEN] = -1.0;
    cmap->cells[i].component[GLUT_BLUE] = -1.0;
  }
  if (!cmap->cells)
    __glutFatalError("out of memory.");
  cmap->cmap = XCreateColormap(__glutDisplay,
    __glutRoot, vis->visual, AllocAll);
  cmap->next = __glutColormapList;
  __glutColormapList = cmap;
  return cmap;
}

GLUTcolormap *
__glutAssociateColormap(XVisualInfo * vis)
{
  GLUTcolormap *cmap = __glutColormapList;

  while (cmap != NULL) {
    /* Play safe: compare visual IDs, not Visual*'s */
    if (cmap->visual->visualid == vis->visual->visualid) {
      /* already have created colormap for the visual */
      cmap->refcnt++;
      return cmap;
    }
    cmap = cmap->next;
  }
  return associateNewColormap(vis);
}

#define CLAMP(i) ((i) > 1.0 ? 1.0 : ((i) < 0.0 ? 0.0 : (i)))

/* CENTRY */
void
glutSetColor(int ndx, GLfloat red, GLfloat green, GLfloat blue)
{
  GLUTcolormap *cmap, *newcmap;
  XColor color;
  int i;

  cmap = __glutCurrentWindow->colormap;
  if (!cmap) {
    __glutWarning("glutSetColor: current window is RGBA");
    return;
  }
  if (ndx >= __glutCurrentWindow->vis->visual->map_entries ||
    ndx < 0) {
    __glutWarning("glutSetColor: index %d out of range", ndx);
    return;
  }
  if (cmap->refcnt > 1) {
    GLUTwindow *toplevel;

    newcmap = associateNewColormap(__glutCurrentWindow->vis);
    cmap->refcnt--;
    /* Wouldn't it be nice if XCopyColormapAndFree could be
       told not to free the old colormap's entries! */
    for (i = cmap->size - 1; i >= 0; i--) {
      if (i == ndx) {
        /* We are going to set this cell shortly! */
        continue;
      }
      if (cmap->cells[i].component[GLUT_RED] >= 0.0) {
        color.pixel = i;
        newcmap->cells[i].component[GLUT_RED] =
          cmap->cells[i].component[GLUT_RED];
        color.red = (GLfloat) 0xffff *
          cmap->cells[i].component[GLUT_RED];
        newcmap->cells[i].component[GLUT_GREEN] =
          cmap->cells[i].component[GLUT_GREEN];
        color.green = (GLfloat) 0xffff *
          cmap->cells[i].component[GLUT_GREEN];
        newcmap->cells[i].component[GLUT_BLUE] =
          cmap->cells[i].component[GLUT_BLUE];
        color.blue = (GLfloat) 0xffff *
          cmap->cells[i].component[GLUT_BLUE];
        color.flags = DoRed | DoGreen | DoBlue;
        XStoreColor(__glutDisplay, newcmap->cmap, &color);
      } else {
        /* leave unallocated entries unallocated */
      }
    }
    __glutCurrentWindow->colormap = newcmap;
    __glutCurrentWindow->cmap = newcmap->cmap;
    XSetWindowColormap(__glutDisplay, __glutCurrentWindow->win,
      __glutCurrentWindow->cmap);
    toplevel = __glutToplevelOf(__glutCurrentWindow);
    if (toplevel->cmap != __glutCurrentWindow->cmap) {
      __glutPutOnWorkList(toplevel, GLUT_COLORMAP_WORK);
    }
    cmap = newcmap;
  }
  color.pixel = ndx;
  red = CLAMP(red);
  cmap->cells[ndx].component[GLUT_RED] = red;
  color.red = (GLfloat) 0xffff *red;
  green = CLAMP(green);
  cmap->cells[ndx].component[GLUT_GREEN] = green;
  color.green = (GLfloat) 0xffff *green;
  blue = CLAMP(blue);
  cmap->cells[ndx].component[GLUT_BLUE] = blue;
  color.blue = (GLfloat) 0xffff *blue;
  color.flags = DoRed | DoGreen | DoBlue;
  XStoreColor(__glutDisplay, cmap->cmap, &color);
}

GLfloat
glutGetColor(int ndx, int comp)
{
  if (!__glutCurrentWindow->colormap) {
    __glutWarning("glutGetColor: current window is RGBA");
    return -1.0;
  }
  if (ndx >= __glutCurrentWindow->vis->visual->map_entries ||
    ndx < 0) {
    __glutWarning("glutGetColor: index %d out of range", ndx);
    return -1.0;
  }
  return
    __glutCurrentWindow->colormap->cells[ndx].component[comp];
}
/* ENDCENTRY */

void
__glutFreeColormap(GLUTcolormap * cmap)
{
  GLUTcolormap *cur, **prev;

  cmap->refcnt--;
  if (cmap->refcnt == 0) {
    /* remove from colormap list */
    cur = __glutColormapList;
    prev = &__glutColormapList;
    while (cur) {
      if (cur == cmap) {
        *prev = cmap->next;
        break;
      }
      prev = &(cur->next);
      cur = cur->next;
    }
    /* actually free colormap */
    XFreeColormap(__glutDisplay, cmap->cmap);
    free(cmap->cells);
    free(cmap);
  }
}

/* CENTRY */
void
glutCopyColormap(int winnum)
{
  GLUTwindow *window = __glutWindowList[winnum];
  GLUTcolormap *oldcmap, *newcmap, *copycmap;
  XColor color;
  int i, last;

  if (!__glutCurrentWindow->colormap) {
    __glutWarning("glutCopyColormap: current window is RGBA");
    return;
  }
  if (!window->colormap) {
    __glutWarning("glutCopyColormap: window %d is RGBA", winnum);
    return;
  }
  oldcmap = __glutCurrentWindow->colormap;
  newcmap = window->colormap;

  if (newcmap == oldcmap)
    return;

  /* Play safe: compare visual IDs, not Visual*'s */
  if (newcmap->visual->visualid == oldcmap->visual->visualid) {
    GLUTwindow *toplevel;

    /* Visuals match!  "Copy" by reference...  */
    __glutFreeColormap(oldcmap);
    newcmap->refcnt++;
    __glutCurrentWindow->colormap = newcmap;
    __glutCurrentWindow->cmap = newcmap->cmap;
    XSetWindowColormap(__glutDisplay, __glutCurrentWindow->win,
      __glutCurrentWindow->cmap);
    toplevel = __glutToplevelOf(window);
    if (toplevel->cmap != window->cmap) {
      __glutPutOnWorkList(toplevel, GLUT_COLORMAP_WORK);
    }
  } else {
    /* Visuals different - need a distinct X colormap! */
    copycmap = associateNewColormap(__glutCurrentWindow->vis);
    /* Wouldn't it be nice if XCopyColormapAndFree could be
       told not to free the old colormap's entries! */
    last = newcmap->size;
    if (last > copycmap->size) {
      last = copycmap->size;
    }
    for (i = last - 1; i >= 0; i--) {
      if (newcmap->cells[i].component[GLUT_RED] >= 0.0) {
        color.pixel = i;
        copycmap->cells[i].component[GLUT_RED] =
          newcmap->cells[i].component[GLUT_RED];
        color.red = (GLfloat) 0xffff *
          newcmap->cells[i].component[GLUT_RED];
        copycmap->cells[i].component[GLUT_GREEN] =
          newcmap->cells[i].component[GLUT_GREEN];
        color.green = (GLfloat) 0xffff *
          newcmap->cells[i].component[GLUT_GREEN];
        copycmap->cells[i].component[GLUT_BLUE] =
          newcmap->cells[i].component[GLUT_BLUE];
        color.blue = (GLfloat) 0xffff *
          newcmap->cells[i].component[GLUT_BLUE];
        color.flags = DoRed | DoGreen | DoBlue;
        XStoreColor(__glutDisplay, copycmap->cmap, &color);
      }
    }
  }
}
/* ENDCENTRY */
