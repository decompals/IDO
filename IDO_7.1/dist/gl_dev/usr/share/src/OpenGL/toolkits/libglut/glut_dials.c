/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include "glutint.h"

#if (GLUT_API_VERSION >= 2)

void
glutButtonBoxFunc(GLUTbuttonBoxCB buttonBoxFunc)
{
  __glutCurrentWindow->buttonBox = buttonBoxFunc;
  __glutUpdateInputDeviceMaskFunc = __glutUpdateInputDeviceMask;
  __glutPutOnWorkList(__glutCurrentWindow,
    GLUT_DEVICE_MASK_WORK);
}

void
glutDialsFunc(GLUTdialsCB dialsFunc)
{
  __glutCurrentWindow->dials = dialsFunc;
  __glutUpdateInputDeviceMaskFunc = __glutUpdateInputDeviceMask;
  __glutPutOnWorkList(__glutCurrentWindow,
    GLUT_DEVICE_MASK_WORK);
}

#endif /* GLUT_API_VERSION */
