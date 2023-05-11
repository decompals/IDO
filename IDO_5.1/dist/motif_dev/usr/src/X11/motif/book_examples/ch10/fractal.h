/**********************************************************************************
 * fractal.h: declarations for the fractal program
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  247-253
  *
  *
  *  Copyright 1989 by Prentice Hall
  *  All Rights Reserved
  *
  * This code is based on the OSF/Motif widget set and the X Window System
  *
  * Permission to use, copy, modify, and distribute this software for 
  * any purpose and without fee is hereby granted, provided that the above
  * copyright notice appear in all copies and that both the copyright notice
  * and this permission notice appear in supporting documentation.
  *
  * Prentice Hall and the author disclaim all warranties with regard to 
  * this software, including all implied warranties of merchantability and fitness.
  * In no event shall Prentice Hall or the author be liable for any special,
  * indirect or cosequential damages or any damages whatsoever resulting from 
  * loss of use, data or profits, whether in an action of contract, negligence 
  * or other tortious action, arising out of or in connection with the use 
  * or performance of this software.
  *
  * Open Software Foundation is a trademark of The Open Software Foundation, Inc.
  * OSF is a trademark of Open Software Foundation, Inc.
  * OSF/Motif is a trademark of Open Software Foundation, Inc.
  * Motif is a trademark of Open Software Foundation, Inc.
  * DEC is a registered trademark of Digital Equipment Corporation
  * HP is a registered trademark of the Hewlett Packard Company
  * DIGITAL is a registered trademark of Digital Equipment Corporation
  * X Window System is a trademark of the Massachusetts Institute of Technology
  **********************************************************************************/




#include <X11/StringDefs.h> 
#include <X11/Intrinsic.h> 
#include <Xm/Xm.h> 
#include <Xm/DrawingA.h> 
#include <X11/Xutil.h>
#include "libXs.h"

void     resize();
void     redisplay();
void     create_image ();
/*
 * Structure to represent a complex number.
 */
typedef struct {
  float  real, imag;
} complex;
/*
 * Assorted information needed to generate and draw the image.
 */
typedef struct {
  int          depth, ncolors;
  float        range, max_distance;
  complex      origin;
  GC           gc;
  Pixmap       pix;
  Dimension    width, height;
} image_data, *image_data_ptr;
/*
 * Resource that affect the appearance of the fractal image.
 */
static XtResource resources[] = {
 {"depth", "Depth", XtRInt, sizeof (int),
   XtOffset(image_data_ptr, depth), XtRString, "20"         },
 {"real_origin", "RealOrigin", XtRFloat, sizeof (float),
   XtOffset(image_data_ptr, origin.real), XtRString, "-1.4" },
 {"imaginary_origin","ImaginaryOrigin",XtRFloat,sizeof(float),
   XtOffset(image_data_ptr, origin.imag), XtRString, "1.0"  },
 {"range", "Range", XtRFloat, sizeof(float),
   XtOffset(image_data_ptr,range), XtRString, "2.0"         },
 {"max_distance", "MaxDistance", XtRFloat, sizeof (float),
   XtOffset(image_data_ptr, max_distance),XtRString, "4.0"  }
 };
