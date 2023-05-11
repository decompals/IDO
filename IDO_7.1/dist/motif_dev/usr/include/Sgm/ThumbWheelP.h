/* 
 * ThumbWheelP.h - private header file for empty widget
 */

#ifndef _SG_ThumbWheelP_h
#define _SG_ThumbWheelP_h

#include <Xm/XmP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/DrawP.h>
#include <Sgm/ThumbWheel.h>

typedef struct {
  int make_compiler_happy;
#ifdef __sgi
  caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgThumbWheelClassPart;


typedef struct {
  CoreClassPart         core_class;
  XmPrimitiveClassPart  primitive_class;
  SgThumbWheelClassPart thumbWheel_class;
} SgThumbWheelClassRec;


extern SgThumbWheelClassRec sgThumbWheelClassRec;


typedef struct {
  /* resources */
  int lower_bound;
  int upper_bound;
  int home_position;
  int angle_range;
  int angle_factor;
  int value;
  unsigned char orientation;
  Boolean animate;
  Boolean show_home_button;

  XtCallbackList value_changed_callback;
  XtCallbackList drag_callback;

  /* private state */
  Boolean infinite;
  Boolean dragging;
  int drag_begin_value;
  int last_mouse_position;
  Boolean pegged;
  int pegged_mouse_position;
  Dimension viewable_pixels;
  int user_pixels;
  Pixmap pix1;
  Pixmap pix2;
  Pixmap pix3;
  Pixmap pix4;
  Pixmap pix1_hilite;
  Pixmap pix2_hilite;
  Pixmap pix3_hilite;
  Pixmap pix4_hilite;
  Pixmap current_quiet_pixmap; /* this will be equal to one of the others */
  Pixmap current_hilite_pixmap; /* this will be equal to one of the others */
  Boolean wheel_hilite;

  Pixmap button_quiet_pixmap;
  Pixmap button_hilite_pixmap;
  Boolean button_hilite;

  GC foreground_GC;
  shaderptr shader;

  int wheel_x;
  int wheel_y;
  int button_x;
  int button_y;

  Boolean home_button_armed;

#ifdef __sgi
  caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgThumbWheelPart;


typedef struct _SgThumbWheelRec {
  CorePart         core;
  XmPrimitivePart  primitive;
  SgThumbWheelPart thumbWheel;
} SgThumbWheelRec;


/* DON'T ADD ANYTHING AFTER THIS #ENDIF */
#endif /* _SG_ThumbWheelP_h */
