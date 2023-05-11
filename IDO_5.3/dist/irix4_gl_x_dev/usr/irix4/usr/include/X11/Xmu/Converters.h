/*
 * $XConsortium: Converters.h,v 1.9 90/05/11 15:30:55 jim Exp $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 * The interfaces described by this header file are for miscellaneous utilities
 * and are not part of the Xlib standard.
 */

#ifndef _XMU_STRCONVERT_H_
#define _XMU_STRCONVERT_H_

#ifdef __cplusplus			/* do not leave open across includes */
extern "C" {					/* for C++ V2.0 */
#endif

/*
 * Converters - insert in alphabetical order
 */

/******************************************************************************
 * XmuCvtFunctionToCallback
 */
extern void XmuCvtFunctionToCallback(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToBackingStore
 */
#define XtNbackingStore "backingStore"
#define XtCBackingStore "BackingStore"
#define XtRBackingStore "BackingStore"
#define XtEnotUseful "notUseful"
#define XtEwhenMapped "whenMapped"
#define XtEalways "always"
#define XtEdefault "default"
extern void XmuCvtStringToBackingStore(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToCursor
 */
extern void XmuCvtStringToCursor(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);
extern Boolean XmuCvtStringToColorCursor(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */,
    XtPointer*		/* converter_data */
#endif
);

#define XtRColorCursor "ColorCursor"
#define XtNpointerColor "pointerColor"
#define XtNpointerColorBackground "pointerColorBackground"


/******************************************************************************
 * XmuCvtStringToFontStruct
 */

extern Boolean XmuCvtStringToFontStruct (
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */,
    XtPointer*		/* converter_data */
#endif
);

extern void XmuSetStringToFontStructConverter (
#if NeedFunctionPrototypes
    Boolean		/* trydefaults */
#endif
);

/******************************************************************************
 * XmuCvtStringToGravity
 */
typedef int XtGravity;

#define XtRGravity "Gravity"
#define XtEForget "forget"
#define XtENorthWest "northwest"
#define XtENorth "north"
#define XtENorthEast "northeast"
#define XtEWest "west"
#define XtECenter "center"
#define XtEEast "east"
#define XtESouthWest "southwest"
#define XtESouth "south"
#define XtESouthEast "southeast"
#define XtEStatic "static"
#define XtEUnmap "unmap"

extern void XmuCvtStringToGravity (
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToJustify
 */
typedef enum {
    XtJustifyLeft,       /* justify text to left side of button   */
    XtJustifyCenter,     /* justify text in center of button      */
    XtJustifyRight       /* justify text to right side of button  */
} XtJustify;
#define XtEleft "left"
#define XtEcenter "center"
#define XtEright "right"
#define XtEtop "top"
#define XtEbottom "bottom"

extern void XmuCvtStringToJustify(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToLong
 */
#define XtRLong "Long"
extern void XmuCvtStringToLong(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToOrientation
 */
typedef enum {XtorientHorizontal, XtorientVertical} XtOrientation;
extern void XmuCvtStringToOrientation(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToBitmap
 */
extern void XmuCvtStringToBitmap(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);


/******************************************************************************
 * XmuCvtStringToShapeStyle; is XtTypeConverter (i.e. new style)
 * no conversion arguments, not particularly useful to cache the results.
 */

#define XtRShapeStyle "ShapeStyle"
#define XtERectangle "Rectangle"
#define XtEOval "Oval"
#define XtEEllipse "Ellipse"
#define XtERoundedRectangle "RoundedRectangle"

#define XmuShapeRectangle 1
#define XmuShapeOval 2
#define XmuShapeEllipse 3
#define XmuShapeRoundedRectangle 4

extern Boolean XmuCvtStringToShapeStyle(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */,
    XtPointer*		/* converter_data */
#endif
);

extern Boolean XmuReshapeWidget(
#if NeedFunctionPrototypes
    Widget	/* w */,
    int		/* shape_style */,
    int		/* corner_width */,
    int		/* corner_height */
#endif
);

/******************************************************************************
 * XmuCvtStringToWidget
 */
extern void XmuCvtStringToWidget(
#if NeedFunctionPrototypes
    XrmValue*		/* args */,
    Cardinal*		/* num_args */,
    XrmValuePtr		/* fromVal */,
    XrmValuePtr		/* toVal */
#endif
);

#ifdef __cplusplus
}						/* for C++ V2.0 */
#endif

#endif /* _XMU_STRCONVERT_H_ */
