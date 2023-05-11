/*
 *  @OSF_COPYRIGHT@
 *  COPYRIGHT NOTICE
 *  Copyright (c) 1990, 1991, 1992, 1993 Open Software Foundation, Inc.
 *  ALL RIGHTS RESERVED (MOTIF). See the file named COPYRIGHT.MOTIF for
 *  the full copyright text.
 */
#ifndef _XmGrabShell_h
#define _XmGrabShell_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

externalref WidgetClass xmGrabShellWidgetClass;

typedef struct _XmGrabShellClassRec       * XmGrabShellWidgetClass;
typedef struct _XmGrabShellWidgetRec      * XmGrabShellWidget;

#ifndef XmIsGrabShell
#define XmIsGrabShell(w) XtIsSubclass(w, xmGrabShellWidgetClass)
#endif /* XmIsGrabShell */


/*
 * Additional Resource Name / Class defines
 * for new Overlay Plane Resource
 */
#define SgNuseOverlayPlane "useOverlayPlane"
#define SgCUseOverlayPlane "UseOverlayPlane"


/*
 * The following Defines are taken from the 2.0 version of
 * XmStrDefs.h. They need to be included for compatibility reasons.
 */
#define XmCOwnerEvents "OwnerEvents"
#define XmNownerEvents "ownerEvents"
#define XmCGrabStyle "GrabStyle"
#define XmNgrabStyle "grabStyle"
#define XmRDynamicPixmap "DynamicPixmap"

/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget XmCreateGrabShell() ;

#else

extern Widget XmCreateGrabShell( 
                        Widget parent,
                        char *name,
                        ArgList al,
                        Cardinal ac) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmGrabShell_h */
/* DON'T ADD STUFF AFTER THIS #endif */
