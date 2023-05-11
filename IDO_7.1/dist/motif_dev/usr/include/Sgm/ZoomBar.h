/***********************************************************************
 *
 * ZoomBar Widget
 *
 ***********************************************************************/

#ifndef _SGZOOMBAR_H_
#define _SGZOOMBAR_H_

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef SgIsZoomBar
#define SgIsZoomBar(w) XtIsSubclass(w, sgZoomBarWidgetClass)
#endif /* SgIsZoomBar */

/* ZoomBar Widget */

externalref WidgetClass sgZoomBarWidgetClass;

typedef struct _SgZoomBarClassRec *SgZoomBarWidgetClass;
typedef struct _SgZoomBarRec      *SgZoomBarWidget;

/******** defines for ZoomBar resources      ********/
#define SgNsectionWidths "sectionWidths"
#define SgCSectionWidths "SectionWidths"
#define SgNsectionCount  "sectionCount"
#define SgCSectionCount  "SectionCount"


/********    Public Function Declarations    ********/

extern Widget SgCreateZoomBar( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;

/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SGZOOMBAR_H_ */
/* DON'T ADD ANYTHING AFTER THIS #endif */

