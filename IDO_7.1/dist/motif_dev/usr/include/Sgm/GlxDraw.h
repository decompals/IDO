/*
 * $Header: /hosts/bonnie/proj/banyan/isms/motif/src/lib/Sgm/RCS/GlxDraw.h,v 1.2 1992/12/03 16:24:28 joel Exp $
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

#ifndef _GlxDraw_h
#define _GlxDraw_h

#include <gl/glws.h>

/****************************************************************
 *
 * GlxDraw widgets
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 destroyCallback     Callback		Pointer		NULL
 exposeCallback      Callback		Pointer		NULL
 ginitCallback       Callback		Pointer		NULL
 inputCallback       Callback		Pointer		NULL
 resizeCallback      Callback		Pointer		NULL
 glxConfig	     GlxConfig		GLXconfig *	{{0,0,0}}
 visual		     Visual		Visual		NULL
 overrideColormap    OverrideColormap	Boolean		TRUE
 useOverlay	     UseOverlay		Boolean		FALSE
 overlayColormap     Colormap		Colormap	NULL
 overlayDepth        Depth		int		0
 overlayVisual       Visual		VisualInfo	NULL
 overlayWindow       Window		Window		0
 usePopup	     UsePopup		Boolean		FALSE
 popupColormap       Colormap		Colormap	NULL
 popupDepth          Depth		int		0
 popupVisual         Visual		VisualInfo	NULL
 popupWindow         Window		Window		0
*/

#define GlxNglxConfig	"glxConfig"
#define GlxCGlxConfig	"GlxConfig"
#define GlxRGlxConfig	"GlxConfig"
#define GlxNvisual	"visual"
#define GlxCVisual	"Visual"
#define GlxRVisualInfo	"VisualInfo"
#define GlxNoverrideColormap	"overrideColormap"
#define GlxCOverrideColormap	"OverrideColormap"

#define GlxCCallback 	"Callback"
#define GlxNexposeCallback "exposeCallback"
#define GlxNginitCallback "ginitCallback"
#define GlxNresizeCallback "resizeCallback"
#define GlxNinputCallback "inputCallback"

#define GlxCColormap		"Colormap"
#define GlxCDepth		"Depth"
#define GlxCWindow		"Window"
#define GlxNuseOverlay		"useOverlay"
#define GlxCUseOverlay		"UseOverlay"
#define GlxNoverlayColormap	"overlayColormap"
#define GlxNoverlayDepth	"overlayDepth"
#define GlxNoverlayVisual	"overlayVisual"
#define GlxNoverlayWindow	"overlayWindow"
#define GlxNoverlayExposeCallback	"overlayExposeWindow"
#define GlxNuseUnderlay		"useUnderlay"
#define GlxCUseUnderlay		"UseUnderlay"
#define GlxNunderlayColormap	"underlayColormap"
#define GlxNunderlayDepth	"underlayDepth"
#define GlxNunderlayVisual	"underlayVisual"
#define GlxNunderlayWindow	"underlayWindow"
#define GlxNunderlayExposeCallback	"underlayExposeWindow"
#define GlxNusePopup		"usePopup"
#define GlxCUsePopup		"UsePopup"
#define GlxNpopupColormap	"popupColormap"
#define GlxNpopupDepth		"popupDepth"
#define GlxNpopupVisual		"popupVisual"
#define GlxNpopupWindow		"popupWindow"
#define GlxNpopupExposeCallback	"popupExposeWindow"

#ifdef __GLX_MOTIF
typedef struct _GlxMDrawClassRec	*GlxMDrawWidgetClass;
typedef struct _GlxMDrawRec		*GlxMDrawWidget;

extern WidgetClass glxMDrawWidgetClass;
#else /* not __GLX_MOTIF */
typedef struct _GlxDrawClassRec		*GlxDrawWidgetClass;
typedef struct _GlxDrawRec		*GlxDrawWidget;

extern WidgetClass glxDrawWidgetClass;
#endif

/* Callback reasons */
#ifdef __GLX_MOTIF
#define GlxCR_EXPOSE	XmCR_EXPOSE
#define GlxCR_RESIZE	XmCR_RESIZE
#define GlxCR_INPUT	XmCR_INPUT
#else /* not __GLX_MOTIF */
/* The same values as Motif, but don't use Motif constants */
#define GlxCR_EXPOSE	38
#define GlxCR_RESIZE	39
#define GlxCR_INPUT	40
#endif /* __GLX_MOTIF */

#define GlxCR_GINIT	32135	/* Arbitrary number that should neverr clash*/

typedef struct
{
    int     reason;
    XEvent  *event;
    Window  window;
    int     buffer;
    Dimension width, height;		/* for resize callback */
} GlxDrawCallbackStruct;


#ifdef __GLX_MOTIF
#ifdef _NO_PROTO
extern Widget GlxCreateMDraw ();
#else /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

extern Widget GlxCreateMDraw (Widget parent, char *name, ArgList arglist, Cardinal argcount);
#endif /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
#endif /* __GLX_MOTIF */

#endif /* _GlxDraw_h */
