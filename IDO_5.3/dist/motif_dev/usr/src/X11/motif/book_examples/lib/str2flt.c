/*************************************************
 * str2flt.c: Convert a string to a float.
  *
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages: ??
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

void xs_cvt_str_to_float(args, nargs, fromVal, toVal)
     XrmValue    *args;
     int         *nargs;
     XrmValue    *fromVal, *toVal;
{
  static float result;
  /*
   * Make sure the number of args is correct.
   */
  if (*nargs != 0)
   XtWarning("String to Float conversion needs no arguments");
  /*
   * Convert the string in the fromVal to a floating pt.
   */
  if (sscanf((char *)fromVal->addr, "%f", &result) == 1) {
    /*
     * Make the toVal point to the result.
     */
    toVal->size = sizeof (float); 
    toVal->addr = (caddr_t) &result; 
  } 
  else 
  /* 
   * If sscanf fails, issue a warning that something is wrong.
   */
   XtStringConversionWarning((char *) fromVal->addr, "Float");
}
