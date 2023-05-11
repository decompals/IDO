/* 
 * (c) Copyright 1993 Silcon Graphics, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/*   $RCSfile: ColorC.h,v $ $Revision: 1.8 $ $Date: 1995/03/29 18:58:46 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _SgColorChooser_h
#define _SgColorChooser_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SgNshowSliders  "showSliders"
#define SgNrgbBase      "rgbBase"
#define SgNwysiwyg      "wysiwyg"
#define SgNuseGl        "useGl"
#define SgNdoubleBuffer "doubleBuffer"

#define SgNcurrentColorLabelString "currentColorLabelString"
#define SgNstoredColorLabelString  "storedColorLabelString"
#define SgNhueLabelString          "hueLabelString"
#define SgNsaturationLabelString   "saturationLabelString"
#define SgNvalueLabelString        "valueLabelString"
#define SgNredLabelString          "redLabelString"
#define SgNgreenLabelString        "greenLabelString"
#define SgNblueLabelString         "blueLabelString"

#define SgCShowSliders  "ShowSliders"
#define SgCRgbBase      "RgbBase"
#define SgCWysiwyg      "Wysiwyg"
#define SgCUseGl        "UseGl"
#define SgCDoubleBuffer "DoubleBuffer"

#define SgRShowSliders "ShowSliders"

typedef enum _ShowSliders {
  SgValue,
  SgRGB_and_Value,
  SgRGB_and_HSV
  } ShowSliders;

typedef struct _SgColorChooserClassRec * SgColorChooserWidgetClass;
typedef struct _SgColorChooserRec      * SgColorChooserWidget;

typedef struct {
  int      reason;
  XEvent  *event;
  short    r, g, b;		/* Color Chooser RGB values */
} SgColorChooserCallbackStruct;

/*
 * IRIS GL specific definitions.
 * IRIS GL is not available in all cases (e.g. 64 bits)
 */
#if defined (IRIS_GL) && IRIS_GL

    /* Class record constants */

	externalref WidgetClass sgColorChooserWidgetClass;

    /* Check for either kind of color chooser widget */
#	ifndef SgIsColorChooser
#		define SgIsColorChooser(w) \
			(  (XtIsSubclass ((w), sgColorChooserWidgetClass)) \
			|| (XtIsSubclass ((w), sgOglColorChooserWidgetClass)) )
#	endif

    /* Check specifically for an IRIS GL color chooser widget */
#	ifndef SgIglIsColorChooser
#		define SgIglIsColorChooser(w)  \
			(XtIsSubclass ((w), sgColorChooserWidgetClass))
#	endif

#else /* IRIS GL is not available */

    /* Check for either kind of color chooser widget */
#	ifndef SgIsColorChooser
#		define SgIsColorChooser(w) \
			(XtIsSubclass ((w), sgOglColorChooserWidgetClass))
#	endif

    /* Check specifically for an IRIS GL color chooser widget */
#	ifndef SgIglIsColorChooser
#		define SgIglIsColorChooser(w) (1!=1)
#	endif

#endif /* IRIS_GL */

/*
 * IRIS GL specific definitions.
 */
    /* Class record constants for the OpenGL widget */

	externalref WidgetClass sgOglColorChooserWidgetClass;

    /* Check for OpenGL color chooser widget */
#	ifndef SgOglIsColorChooser
#		define SgOglIsColorChooser(w) \
			(XtIsSubclass ((w), sgOglColorChooserWidgetClass))
#	endif



/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateColorChooser() ;
extern Widget SgOglCreateColorChooser() ;
extern Widget SgCreateColorChooserDialog() ;
extern Widget SgOglCreateColorChooserDialog() ;
extern Widget SgColorChooserGetChild();
extern void   SgColorChooserSetColor();
extern void   SgColorChooserGetColor();
extern void   SgColorChooserSetCurrentColor();
extern void   SgColorChooserSetStoredColor();

#else

/* Create Color Chooser widget convenient routine */
extern Widget SgCreateColorChooser( 
                        Widget p,
                        String name,
                        ArgList args,
                        Cardinal n) ;
extern Widget SgOglCreateColorChooser( 
                        Widget p,
                        String name,
                        ArgList args,
                        Cardinal n) ;

/* Create Color Chooser dialog convenient routine */
extern Widget SgCreateColorChooserDialog( 
                        Widget p,
                        String name,
                        ArgList args,
                        Cardinal n) ;
extern Widget SgOglCreateColorChooserDialog( 
                        Widget p,
                        String name,
                        ArgList args,
                        Cardinal n) ;

/* Get a particular piece of the Color Chooser */
extern Widget SgColorChooserGetChild(Widget widget, unsigned char child);

/* Set the color of the Color Chooser widget */
extern void   SgColorChooserSetColor(Widget w, short r, short g, short b);

/* Get the current color of the Color Chooser widget */
extern void   SgColorChooserGetColor(Widget w, short *r, short *g, short *b);

/* Set the color of the Color Chooser widget, but not the stored color */
extern void   SgColorChooserSetCurrentColor(Widget w, short r, short g,short b);

/* Set the stored color of the Color Chooser widget, not main selected color */
extern void   SgColorChooserSetStoredColor(Widget w, short r, short g, short b);

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgColorChooser_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
