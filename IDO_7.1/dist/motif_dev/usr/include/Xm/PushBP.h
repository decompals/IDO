/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: PushBP.h,v $ $Revision: 0.5 $ $Date: 1995/01/10 02:08:02 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
/*
*  (c) Copyright 1988 MASSACHUSETTS INSTITUTE OF TECHNOLOGY  */
/*
*  (c) Copyright 1988 MICROSOFT CORPORATION */
#ifndef _XmPButtonP_h
#define _XmPButtonP_h

#include <Xm/PushB.h>
#include <Xm/LabelP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* PushButton class structure */

typedef struct _XmPushButtonClassPart
{
   XtPointer extension;   /* Pointer to extension record */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   _SgClassExtension	 _SG_vendorExtension;
#endif /* __sgi */
} XmPushButtonClassPart;


/* Full class record declaration for PushButton class */

typedef struct _XmPushButtonClassRec {
    CoreClassPart	  core_class;
    XmPrimitiveClassPart  primitive_class;
    XmLabelClassPart      label_class;
    XmPushButtonClassPart pushbutton_class;
} XmPushButtonClassRec;

#ifdef __sgi /* Specify SGI PushButton instance record extension */

typedef struct __SG_XmPushButtonExtPart
{
  Pixmap		locate_pixmap; 
  Pixmap		locate_arm_pixmap; 
  Boolean		pixmap_locate_highlight;
}_SG_XmPushButtonExtPart;

typedef struct __SG_XmPushButtonExt
{
  _SgInstanceExtensionRec     common;   /* Stuff all instance rec's have */
  _SG_XmPushButtonExtPart      instance; /* Resources & instance var's */
} _SG_XmPushButtonExtRec, *_SG_XmPushButtonExt;

#define _SG_PushButtonPtr(w) \
((_SG_XmPushButtonExt)(((XmPushButtonWidget)(w))->pushbutton._SG_vendorExtension))

#endif /* __sgi */


externalref XmPushButtonClassRec xmPushButtonClassRec;

/* PushButton instance record */

typedef struct _XmPushButtonPart
{
   Boolean 	    fill_on_arm;
   Dimension        show_as_default;
   Pixel	    arm_color;
   Pixmap	    arm_pixmap;
   XtCallbackList   activate_callback;
   XtCallbackList   arm_callback;
   XtCallbackList   disarm_callback;

   Boolean 	    armed;
   Pixmap	    unarm_pixmap;
   GC               fill_gc;
   GC               background_gc;
   XtIntervalId     timer;	
   unsigned char    multiClick;		/* KEEP/DISCARD resource */
   int		    click_count;
   Time		    armTimeStamp;
   Boolean      compatible;   /* if false it is Motif 1.1 else Motif 1.0  */
   Dimension    default_button_shadow_thickness;  
		/* New resource - always add it
                    to widgets dimension. */

#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   _SG_XmPushButtonExt  _SG_vendorExtension;
#endif /* __sgiXS */

} XmPushButtonPart;


/* Full instance record declaration */

typedef struct _XmPushButtonRec {
    CorePart	     core;
    XmPrimitivePart  primitive;
    XmLabelPart      label;
    XmPushButtonPart pushbutton;
} XmPushButtonRec;


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

extern void _XmClearBCompatibility() ;

#else

extern void _XmClearBCompatibility( 
                        Widget pb) ;

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmPButtonP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
