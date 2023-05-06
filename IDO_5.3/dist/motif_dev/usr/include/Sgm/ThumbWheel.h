/* 
 * ThumbWheel.h - public header file for thumbwheel widget
 */

#ifndef _SG_ThumbWheel_h
#define _SG_ThumbWheel_h


#ifdef __cplusplus
extern "C" {
#endif

#ifndef SgNlowerBound
#define SgNlowerBound "minimum"
#endif
/* SgNlowerBound is OBSOLETE!  Please use XmNminimum! */

#ifndef SgNupperBound
#define SgNupperBound "maximum"
#endif
/* SgNupperBound is OBSOLETE!  Please use XmNminimum! */

#ifndef SgNhomePosition
#define SgNhomePosition "homePosition"
#endif
#ifndef SgNangleRange
#define SgNangleRange "angleRange"
#endif
#ifndef SgNunitsPerRotation
#define SgNunitsPerRotation "unitsPerRotation"
#endif
#ifndef SgNanimate
#define SgNanimate "animate"
#endif
#ifndef SgNshowHomeButton
#define SgNshowHomeButton "showHomeButton"
#endif


#ifndef SgCLowerBound
#define SgCLowerBound "Minimum"
#endif
/* SgCLowerBound is OBSOLETE!  Please use XmCMinimum! */

#ifndef SgCUpperBound
#define SgCUpperBound "Maximum"
#endif
/* SgCUpperBound is OBSOLETE!  Please use XmCMaximum! */

#ifndef SgCHomePosition
#define SgCHomePosition "HomePosition"
#endif
#ifndef SgCAngleRange
#define SgCAngleRange "AngleRange"
#endif
#ifndef SgCUnitsPerRotation
#define SgCUnitsPerRotation "UnitsPerRotation"
#endif
#ifndef SgCAnimate
#define SgCAnimate "Animate"
#endif
#ifndef SgCShowHomeButton
#define SgCShowHomeButton "ShowHomeButton"
#endif


extern WidgetClass sgThumbWheelWidgetClass;


typedef struct _SgThumbWheelClassRec *SgThumbWheelWidgetClass;
typedef struct _SgThumbWheelRec      *SgThumbWheelWidget;


/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateThumbWheel();

#else

extern Widget SgCreateThumbWheel(Widget parent,
				 char *name,
				 ArgList arglist,
				 Cardinal argcount);

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


/*
 * Structure for both callbacks (Drag and Value Changed).
 */
typedef struct {
  int reason;
  XEvent *event;
  int value;
} SgThumbWheelCallbackStruct;


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

/* DON'T ADD ANYTHING AFTER THIS #ENDIF */
#endif /* _SG_ThumbWheel_h */
