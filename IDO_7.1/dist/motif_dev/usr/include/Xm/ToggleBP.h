/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: ToggleBP.h,v $ $Revision: 0.6 $ $Date: 1995/01/10 02:12:38 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
/********************************************
 *
 *   No new fields need to be defined
 *   for the Toggle widget class record
 *
 ********************************************/

#ifndef _XmToggleButtonP_h
#define _XmToggleButtonP_h

#include <Xm/ToggleB.h>
#include <Xm/LabelP.h>
extern int _sgRadioData[primitiveVisualStates][primitiveNumSegments];
extern int _sgIndicatorData[primitiveVisualStates][primitiveNumSegments];

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmToggleButtonClassPart
 {
   XtPointer extension;   /* Pointer to extension record */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   _SgClassExtension  _SG_vendorExtension;
#endif /* __sgi */
 } XmToggleButtonClassPart;


/****************************************************
 *
 * Full class record declaration for Toggle class
 *
 ****************************************************/
typedef struct _XmToggleButtonClassRec {
    CoreClassPart	  	core_class;
    XmPrimitiveClassPart  	primitive_class;
    XmLabelClassPart      	label_class;
    XmToggleButtonClassPart	toggle_class;
} XmToggleButtonClassRec;

#ifdef __sgi /* Specify SGI Toggle instance record extension */

typedef struct __SG_XmToggleExtPart
{
  XmRenderIndicatorProc indicator_render;
  XmRenderShadowsProc   indicator_render_shadows;
  int                   indicator_visual_state;
  visual_state_array    indicator_visual_array;
  Pixel                 indicator_background;
  shaderptr             indicator_shader;
  int                   indicator_shadow_thickness;
  shaderptr             indicator_select_shader;
  Pixmap		locate_pixmap; 
  Pixmap		locate_select_pixmap; 
  Boolean		pixmap_locate_highlight;
}_SG_XmToggleExtPart;

typedef struct __SG_XmToggleExt
{
  _SgInstanceExtensionRec     common;   /* Stuff all instance rec's have */
  _SG_XmToggleExtPart      instance; /* Resources & instance var's */
} _SG_XmToggleExtRec, *_SG_XmToggleExt;

#define _SG_TogglePtr(w) \
((_SG_XmToggleExt)(((XmToggleButtonWidget)(w))->toggle._SG_vendorExtension))

#endif /* __sgi */



externalref XmToggleButtonClassRec xmToggleButtonClassRec;


/********************************************
 *
 * No new fields needed for instance record
 *
 ********************************************/

typedef struct _XmToggleButtonPart
{ 
   unsigned char	ind_type;
   Boolean		visible;
   Dimension		spacing;
   Dimension		indicator_dim;
   Boolean		indicator_set;
			/* FYI...	TRUE if indicator_dim was set from
					a resource or if a PIXMAP is being
					used. This means that size of
					indicator box should not be
					algorithmically changed */
   Pixmap		on_pixmap; 
   Pixmap		insen_pixmap; 
   Boolean		set;
   Boolean     		visual_set; /* used for visuals and does not reflect
                                        the true state of the button */
   Boolean		ind_on;
   Boolean		fill_on_select;
   Pixel		select_color;
   GC			select_GC;
   GC			background_gc;
   XtCallbackList 	value_changed_CB,
                        arm_CB,
                        disarm_CB;
   Boolean      	Armed;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   _SG_XmToggleExt  _SG_vendorExtension;
#endif /* __sgi */
} XmToggleButtonPart;



/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _XmToggleButtonRec {
    CorePart	        core;
    XmPrimitivePart     primitive;
    XmLabelPart		label;
    XmToggleButtonPart  toggle;
} XmToggleButtonRec;


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO


#else


#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmToggleButtonP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
