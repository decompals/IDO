/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: ScrollBarP.h,v $ $Revision: 0.7 $ $Date: 1994/06/19 22:23:26 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _XmScrollBarP_h
#define _XmScrollBarP_h


#include <Xm/ScrollBar.h>
#include <Xm/PrimitiveP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  Minimum slider width or height  */

#define MIN_SLIDER_THICKNESS	1
#define SGI_MIN_SLIDER_LENGTH  26
#define MIN_SLIDER_LENGTH	6
#define drawArrow1 1
#define drawArrow2 2
#define arrowFaceShade 7 /* do some intelligent here */


/*  ScrollBar class structure  */

typedef struct __SG_XmScrollBarClassExtPart
{
  XmRenderShadowsProc arrow_render_shadows; 
}_SG_XmScrollBarClassExtPart;

typedef struct __SG_XmScrollBarClassExt
{
  _SgClassExtensionRec common;
  _SG_XmScrollBarClassExtPart scrollBar;
} _SG_XmScrollBarClassExtRec, *_SG_XmScrollBarClassExt;


typedef struct _XmScrollBarClassPart
{
   XtPointer extension;   /* Pointer to extension record */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   _SgClassExtension  _SG_vendorExtension;
#endif /* __sgi */
} XmScrollBarClassPart;


/*  Full class record declaration for CheckBox class  */

typedef struct _XmScrollBarClassRec
{
   CoreClassPart        core_class;
   XmPrimitiveClassPart primitive_class;
   XmScrollBarClassPart scrollBar_class;
} XmScrollBarClassRec;

externalref XmScrollBarClassRec xmScrollBarClassRec;

#ifdef __sgi /* Specify SGI ScrollBar instance record extension */

typedef struct __SG_XmScrollBarExtPart
{
  unsigned char arrows_adjacent;
  unsigned char arrows_type;
  Drawable located_pixmap;
  Drawable divot_pixmap;
  visual_state_array arrow_state_array;
  visual_state_array thumb_state_array;
  int arrow1_state;
  int arrow2_state;
  Boolean arrow1_located;
  Boolean arrow2_located;
  Boolean thumb_located;
  Boolean divot_drawn;
  short divot_x;
  short divot_y;
  Boolean show_thumb;
  int thumb_visual_state;
  shaderptr thumb_shader;
  /* percent done */
  XmSlidingMode slidingMode;		/* XmSLIDER or XmTHERMOMETER */
  Boolean editable;			/* False==insensitive but not grayed */
  XmSliderVisual sliderVisual;		/* XmSHADOWED, XmFLAT_FOREGROUND, or XmETCHED_LINE */
  Boolean slanted;			/* True=="max" edge of slider slants */
  GC bg_gc;
  /* end percent done */
}_SG_XmScrollBarExtPart;


typedef struct __SG_XmScrollBarExt
{
  _SgInstanceExtensionRec     common;   /* Stuff all instance rec\'s have */
  _SG_XmScrollBarExtPart      instance; /* Resources & instance var\'s */
} _SG_XmScrollBarExtRec, *_SG_XmScrollBarExt;

#define _SG_ScrollBarPtr(w) \
((_SG_XmScrollBarExt)(((XmScrollBarWidget)(w))->scrollBar._SG_vendorExtension))

#define _SG_ScrollBarClassPtr(wc) \
((_SG_XmScrollBarClassExt)(((XmScrollBarClassRec*)(wc))->\
             scrollBar_class._SG_vendorExtension))
#endif /* __sgi */


/*  The ScrollBar instance record  */

typedef struct _XmScrollBarPart
{
   int value;
   int minimum;
   int maximum;
   int slider_size;

   unsigned char orientation;
   unsigned char processing_direction;
   Boolean show_arrows;

   int increment;
   int page_increment;

   int initial_delay;
   int repeat_delay;

   XtCallbackList value_changed_callback;
   XtCallbackList increment_callback;
   XtCallbackList decrement_callback;
   XtCallbackList page_increment_callback;
   XtCallbackList page_decrement_callback;
   XtCallbackList to_top_callback;
   XtCallbackList to_bottom_callback;
   XtCallbackList drag_callback;

   /* obsolete */
   GC unhighlight_GC;
   /***********/

   GC foreground_GC;
   Pixel trough_color;

   Drawable pixmap;
   Boolean  sliding_on;
   Boolean  etched_slider;
   int saved_value;

   unsigned char flags;

   unsigned char change_type;
   XtIntervalId timer;

   short initial_x;
   short initial_y;
   short separation_x;
   short separation_y;

   short slider_x;
   short slider_y;
   short slider_width;
   short slider_height;

   short slider_area_x;
   short slider_area_y;
   short slider_area_width;
   short slider_area_height;

   short arrow1_x;
   short arrow1_y;
   unsigned char arrow1_orientation;
   Boolean arrow1_selected;

   short arrow2_x;
   short arrow2_y;
   unsigned char arrow2_orientation;
   Boolean arrow2_selected;

   short arrow_width;
   short arrow_height;

   /*  Obsolete fields */
   short arrow1_top_count;
   short arrow1_cent_count;
   short arrow1_bot_count;

   XRectangle * arrow1_top;
   XRectangle * arrow1_cent;
   XRectangle * arrow1_bot;

   short arrow2_top_count;
   short arrow2_cent_count;
   short arrow2_bot_count;

   XRectangle * arrow2_top;
   XRectangle * arrow2_cent;
   XRectangle * arrow2_bot;
   /***********/


   /* new for 1.2 */
   GC	unavailable_GC;
#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   _SG_XmScrollBarExt _SG_vendorExtension;
#endif /* __sgi */
} XmScrollBarPart;


/*  Full instance record declaration  */

typedef struct _XmScrollBarRec
{
   CorePart	   core;
   XmPrimitivePart primitive;
   XmScrollBarPart scrollBar;
} XmScrollBarRec;


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

extern void _XmSetEtchedSlider() ;

#else

extern void _XmSetEtchedSlider( 
                        XmScrollBarWidget sbw) ;

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmScrollBarP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
