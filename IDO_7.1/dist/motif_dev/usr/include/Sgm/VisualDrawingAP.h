/***************************************************
 * VisualDrawingA.h: Private header for the visual drawing area widget.
 ***************************************************/
#ifndef _SgVisualDrawingAreaP_h
#define _SgVisualDrawingAreaP_h

#include <Xm/ManagerP.h>
#include <Xm/DrawingAP.h>
#include "VisualDrawingA.h"

#ifdef __cplusplus
extern "C" {
#endif

/*  New fields for the VisualDrawingArea widget class record  */

typedef struct
{
   int mumble;   /* No new procedures */
} SgVisualDrawingAreaClassPart;


/* Full class record declaration */

typedef struct _SgVisualDrawingAreaClassRec
{
	CoreClassPart		core_class;
	CompositeClassPart	composite_class;
	ConstraintClassPart	constraint_class;
	XmManagerClassPart	manager_class;
	XmDrawingAreaClassPart	drawing_area_class;
	SgVisualDrawingAreaClassPart	visual_drawing_area_class;
} SgVisualDrawingAreaClassRec;

externalref SgVisualDrawingAreaClassRec sgVisualDrawingAreaClassRec;


/* New fields for the VisualDrawingArea widget record */

typedef struct
{
	Visual *visual;
	Boolean	installColormap;/* Install the colormap */
	Boolean ditherBackground; /* Dither background to obtain better match*/
	Widget parentShell;
	Boolean override;	/* true if shell is override redirect */
	Colormap* savedColormaps;	/* used with override redirect */
	int numSavedColormaps;
	Boolean colormapInstalled;	/* colormap is actually installed */
	char *backgroundString;	/* string name of background resource */
} SgVisualDrawingAreaPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _SgVisualDrawingAreaRec
{
	CorePart		core;
	CompositePart		composite;
	ConstraintPart		constraint;
	XmManagerPart		manager;
	XmDrawingAreaPart	drawing_area;
	SgVisualDrawingAreaPart	visual_drawing_area;
} SgVisualDrawingAreaRec;



/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgVisualDrawingAreaP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
