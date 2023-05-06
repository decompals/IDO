/*
 *    These files are for informational purposes and can demonstrate how
 *   to write your own mixed-mode GL-based widgets.  However SGI does not
 *   guarantee that this source code absolutely matches the GLxDraw
 *   widget that is shipped as part of the system.
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

#ifndef _GlxDrawP_h
#define _GlxDrawP_h

#ifdef __GLX_MOTIF
#include <GlxMDraw.h>
#else
#include <GlxDraw.h>
#endif

typedef struct _GlxDrawClassPart
{
    caddr_t extension;
} GlxDrawClassPart;

#ifdef __GLX_MOTIF
typedef struct _GlxMDrawClassRec {
    CoreClassPart		core_class;
    XmPrimitiveClassPart	primitive_class;
    GlxDrawClassPart		glxDraw_class;
} GlxMDrawClassRec;

extern GlxMDrawClassRec glxMDrawClassRec;

#else /* not __GLX_MOTIF */

typedef struct _GlxDrawClassRec {
    CoreClassPart	core_class;
    GlxDrawClassPart	glxDraw_class;
} GlxDrawClassRec;

extern GlxDrawClassRec glxDrawClassRec;
#endif /* __GLX_MOTIF */

typedef struct {
    Boolean		exists;
    Window		window;
    Colormap		colormap;
    int			depth;
    XVisualInfo *	visualInfo;
    XtCallbackList	expose_callback;
} GlxDrawWindowInfo;

typedef struct {
    /* resources */
    GLXconfig *		config;
    XVisualInfo *	visualInfo;
    Boolean		override_colormap;
    XtCallbackList	ginit_callback;
    XtCallbackList	resize_callback;
    XtCallbackList	expose_callback;
    XtCallbackList	input_callback;
    GlxDrawWindowInfo	overlay_info;
    GlxDrawWindowInfo	underlay_info;
    GlxDrawWindowInfo	popup_info;
} GlxDrawPart;

#ifdef __GLX_MOTIF
typedef struct _GlxMDrawRec {
    CorePart		core;
    XmPrimitivePart	primitive;
    GlxDrawPart		glxDraw;
} GlxMDrawRec;
#else /* not __GLX_MOTIF */
typedef struct _GlxDrawRec {
    CorePart	core;
    GlxDrawPart	glxDraw;
} GlxDrawRec;
#endif /* __GLX_MOTIF */

#endif /* _GlxDrawP_h */
