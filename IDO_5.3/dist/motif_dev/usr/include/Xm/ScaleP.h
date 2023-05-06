/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: ScaleP.h,v $ $Revision: 0.6 $ $Date: 1994/08/31 16:36:00 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _XmScaleP_h
#define _XmScaleP_h


#include <Xm/Scale.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Constraint part record for Scale widget */

typedef struct _XmScaleConstraintPart
{
   char unused;
} XmScaleConstraintPart, * XmScaleConstraint;


/*  New fields for the Scale widget class record  */

typedef struct
{
   XtPointer extension;   /* Pointer to extension record */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   _SgClassExtension  _SG_vendorExtension;
#endif /* __sgi */
} XmScaleClassPart;


/* Full class record declaration */

typedef struct _XmScaleClassRec
{
   CoreClassPart       core_class;
   CompositeClassPart  composite_class;
   ConstraintClassPart constraint_class;
   XmManagerClassPart  manager_class;
   XmScaleClassPart    scale_class;
} XmScaleClassRec;

externalref XmScaleClassRec xmScaleClassRec;


#ifdef __sgi /* Specify SGI Primitive instance record extension */

typedef struct __SG_XmScaleExtPart
{
  Position scaleX;
  Position scaleY;
  Dimension scaleWidth;
  Dimension scaleHeight;
  /* Percent done */
  XmSlidingMode slidingMode;		/* XmSLIDER or XmTHERMOMETER */
  Boolean editable;			/* False==insensitive but not grayed */
  XmSliderVisual sliderVisual;		/* XmSHADOWED, XmFLAT_FOREGROUND, or XmETCHED_LINE */
  Boolean slanted;
  Boolean exposeChildFirst;
  /* end Percent done */
}_SG_XmScaleExtPart;

typedef struct __SG_XmScaleExt
{
  _SgInstanceExtensionRec     common;   /* Stuff all instance rec\'s have */
  _SG_XmScaleExtPart          instance; /* Resources & instance var\'s */
}_SG_XmScaleExtRec, *_SG_XmScaleExt;

#define _SG_ScalePtr(w) \
((_SG_XmScaleExt)(((XmScaleWidget)(w))->scale._SG_vendorExtension))

#endif /* __sgi */

/* New fields for the Scale widget record */

typedef struct
{
   int            value;
   int            maximum;
   int            minimum;
   unsigned char  orientation;
   unsigned char  processing_direction;
   XmString       title; 
   XmFontList     font_list;
   XFontStruct  * font_struct;
   Boolean        show_value;
   short          decimal_points;
   Dimension      scale_width;
   Dimension      scale_height;
   Dimension      highlight_thickness;
   Boolean        highlight_on_enter;
   XtCallbackList drag_callback;
   XtCallbackList value_changed_callback;

   int last_value;
   int slider_size;
   GC  foreground_GC;
   int show_value_x;
   int show_value_y;
   int show_value_width;
   int show_value_height;
   int scale_multiple;
#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   _SG_XmScaleExt  _SG_vendorExtension;
#endif /* __sgi */
} XmScalePart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _XmScaleRec
{
    CorePart       core;
    CompositePart  composite;
    ConstraintPart constraint;
    XmManagerPart  manager;
    XmScalePart    scale;
} XmScaleRec;


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO


#else


#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmScaleP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
