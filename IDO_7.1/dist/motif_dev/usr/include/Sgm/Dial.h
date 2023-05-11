/***************************************************
 * Dial.h: Public header  for the dial widget.
 ***************************************************/
#ifndef DIAL_H
#define DIAL_H

#ifdef __cplusplus
extern "C" {
#endif

extern WidgetClass	sgDialWidgetClass;
typedef struct _SgDialClassRec *SgDialWidgetClass;
typedef struct _SgDialRec      *SgDialWidget;

/*
 * define resource strings
 */
#define  SgNdialForeground	"dialForeground"
#define  SgNdialMarkers		"dialMarkers"
#define  SgNdialStartAngle	"startAngle"
/* SgNdialStartAngle is OBSOLETE!  Please use SgNstartAngle! */

#ifndef  SgNstartAngle
#define  SgNstartAngle		"startAngle"
#endif

#define  SgNdialEndAngle	"angleRange"
/* SgNdialEndAngle is OBSOLETE!  Please use SgNangleRange! */

#ifndef  SgNangleRange
#define  SgNangleRange		"angleRange"
#endif

#define  SgNindicatorColor	"indicatorColor"
#define  SgNmarkerLength	"markerLength"
#define  SgCForeground		"Foreground"
#define  SgCMarkers		"Markers"
#define  SgCStartAngle		"StartAngle"
#define  SgCEndAngle		"AngleRange"
/* SgCEndAngle is OBSOLETE!  Please use SgCAngleRange! */

#ifndef  SgCAngleRange
#define  SgCAngleRange		"AngleRange"
#endif

#define  Sg_SELECTED		1
/* Resource for "knob" visual added. */
#define	SgNdialVisual		"dialVisual"
#define	SgCDialVisual		"DialVisual"
#define SgRDialVisual		"DialVisual"
/* Valid values for dialVisual */
typedef enum {
  SgKNOB,
  SgPOINTER
} SgDialVisual;
#define Sgknob "knob"
#define Sgpointer "pointer"

/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern void SgDialSetValue();
extern void SgDialGetValue();
extern Widget SgCreateDial() ;

#else

extern void SgDialSetValue(Widget w, int value);
extern void SgDialGetValue(Widget w, int *value);
extern Widget SgCreateDial( 
                        Widget parent,
                        char *name,
                        Arg *arglist,
                        Cardinal argCount) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

typedef struct {
   int 		reason;
   XEvent 	*event;
   int		position;
} SgDialCallbackStruct;

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* DIAL_H */
/* DON'T ADD ANYTHING AFTER THIS #endif */
