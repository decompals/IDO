/**********************************************************************************
  * rmtest3.c: floating point version.
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     62-64
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





#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "libXs.h"

typedef struct {
   Pixel    fg, bg;
   float    delay;
   Boolean  verbose;
} ApplicationData, *ApplicationDataPtr;

static XtResource resources[] = {
  { XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
    XtOffset(ApplicationDataPtr, fg), XtRString, "Black"    },
  { XtNbackground, XtCBackground, XtRPixel, sizeof (Pixel),
    XtOffset(ApplicationDataPtr, bg), XtRString, "White"    },
  { "delay", "Delay", XtRFloat, sizeof (float),
    XtOffset(ApplicationDataPtr, delay), XtRString,"2.5"    },
  { "verbose", "Verbose", XtRBoolean, sizeof (Boolean),
    XtOffset(ApplicationDataPtr,verbose), XtRString, "FALSE"},
  };

static XrmOptionDescRec options[] = { 
 {"-verbose", "*verbose", XrmoptionNoArg, "TRUE"},
 {"-delay",   "*delay",   XrmoptionSepArg, NULL}
};

main(argc, argv)
    int   argc;
    char *argv[];
 {
  Widget          toplevel;
  ApplicationData data;
  toplevel = XtInitialize(argv[0], "Rmtest", options, 
                          XtNumber(options), &argc, argv);
  /*
   *   Add the string to float type-converter.
   */
  XtAddConverter(XtRString, XtRFloat, xs_cvt_str_to_float,
                 NULL, 0);
  /*
   *  Retrieve the resources.
   */
  XtGetApplicationResources(toplevel, &data, resources, 
                            XtNumber(resources), NULL, 0);
  /*
   * Print the result.
   */
  printf("fg = %d, bg = %d, delay = %f, verbose = %d\n", 
         data.fg, data.bg, data.delay, data.verbose);
 }
