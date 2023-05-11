/*
 * $XConsortium: Drawing.h,v 1.7 90/04/03 09:42:31 swick Exp $
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

#ifndef _XMU_DRAWING_H_
#define _XMU_DRAWING_H_

#if NeedFunctionPrototypes
#include <stdio.h>
#if ! defined(_XtIntrinsic_h) && ! defined(PIXEL_ALREADY_TYPEDEFED)
typedef unsigned long Pixel;
#endif
#endif

#ifdef __cplusplus			/* do not leave open across includes */
extern "C" {					/* for C++ V2.0 */
#endif

void XmuDrawRoundedRectangle(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    Drawable 	/* draw */,
    GC 		/* gc */,
    int		/* x */,
    int		/* y */,
    int		/* w */,
    int		/* h */,
    int		/* ew */,
    int		/* eh */
#endif
);

void XmuFillRoundedRectangle(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    Drawable 	/* draw */,
    GC 		/* gc */,
    int		/* x */,
    int		/* y */,
    int		/* w */,
    int		/* h */,
    int		/* ew */,
    int		/* eh */
#endif
);

void XmuDrawLogo(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    Drawable 	/* drawable */,
    GC		/* gcFore */,
    GC		/* gcBack */,
    int		/* x */,
    int		/* y */,
    unsigned int /* width */,
    unsigned int /* height */
#endif
);

Pixmap XmuCreatePixmapFromBitmap(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    Drawable 		/* d */,
    Pixmap 		/* bitmap */,
    unsigned int	/* width */,
    unsigned int	/* height */,
    unsigned int	/* depth */,
    unsigned long	/* fore */,
    unsigned long	/* back */
#endif
);

Pixmap XmuCreateStippledPixmap(
#if NeedFunctionPrototypes
    Screen*		/* screen */,
    Pixel		/* fore */,
    Pixel		/* back */,
    unsigned int	/* depth */
#endif
);

void XmuReleaseStippledPixmap(
#if NeedFunctionPrototypes
    Screen*		/* screen */,
    Pixmap 		/* pixmap */
#endif
);

Pixmap XtGrayPixmap(
#if NeedFunctionPrototypes
    Screen*		/* screen */
#endif
);

Pixmap XmuLocateBitmapFile(
#if NeedFunctionPrototypes
    Screen*		/* screen */,
    char*		/* name */,
    char*		/* srcname_return */,
    int 		/* srcnamelen */,
    int*		/* width_return */,
    int*		/* height_return, */,
    int*		/* xhot_return */,
    int*		/* yhot_return */
#endif
);

Pixmap XmuLocatePixmapFile(
#if NeedFunctionPrototypes
    Screen*		/* screen */,
    char*		/* name */,
    unsigned long	/* fore */,
    unsigned long	/* back */,
    unsigned int	/* depth */,
    char*		/* srcname_return */,
    int 		/* srcnamelen */,
    int*		/* width_return */,
    int*		/* height_return, */,
    int*		/* xhot_return */,
    int*		/* yhot_return */
#endif
);

int XmuReadBitmapData(
#if NeedFunctionPrototypes
    FILE*		/* fstream */,
    unsigned int*	/* width_return */,
    unsigned int*	/* height_return */,
    unsigned char**	/* datap_return */,
    int*		/* xhot_return */,
    int*		/* yhot_return */
#endif
);

int XmuReadBitmapDataFromFile(
#if NeedFunctionPrototypes
    char*		/* filename */,
    unsigned int*	/* width_return */,
    unsigned int*	/* height_return */,
    unsigned char**	/* datap_return */,
    int*		/* xhot_return */,
    int*		/* yhot_return */
#endif
);

#ifdef __cplusplus
}						/* for C++ V2.0 */
#endif

#endif /* _XMU_DRAWING_H_ */
