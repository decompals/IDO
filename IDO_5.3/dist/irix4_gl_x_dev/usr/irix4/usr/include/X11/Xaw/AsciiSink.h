/*
 * $XConsortium: AsciiSink.h,v 1.6 90/05/08 15:21:40 converse Exp $
 */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _XawAsciiSink_h
#define _XawAsciiSink_h

/***********************************************************************
 *
 * AsciiSink Object
 *
 ***********************************************************************/

#include <X11/Xaw/TextSink.h>

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 echo                Output             Boolean         True
 displayNonprinting  Output             Boolean         True

*/

#define XtCOutput "Output"

#define XtNdisplayNonprinting "displayNonprinting"
#define XtNecho "echo"

/* Class record constants */

extern WidgetClass asciiSinkObjectClass;

typedef struct _AsciiSinkClassRec *AsciiSinkObjectClass;
typedef struct _AsciiSinkRec      *AsciiSinkObject;

/************************************************************
 *
 * Public Functions.
 *
 ************************************************************/

#ifdef XAW_BC
/************************************************************
 *  For Compatability Only.                                 */

#define XtAsciiSinkCreate          XawAsciiSinkCreate
#define XtAsciiSinkDestroy         XawAsciiSinkDestroy

#define XawTextSink Widget
#define XtTextSink XawTextSink

#ifdef __cplusplus
extern "C" {					/* for C++ V2.0 */
#endif

extern XawTextSink XawAsciiSinkCreate(
#if NeedFunctionPrototypes
    Widget		/* parent */,
    ArgList		/* args	*/,
    Cardinal		/* num_args */
#endif
);

#ifdef __cplusplus
}						/* for C++ V2.0 */
#endif

#define XawAsciiSinkDestroy XtDestroyWidget

#endif /* XAW_BC */

#endif /* _XawAsciiSrc_h */
/* DON'T ADD STUFF AFTER THIS #endif */
