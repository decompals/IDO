/***************************************************
 * VisualDrawingA.h: Public header  for the visual drawing area widget.
 ***************************************************/
#ifndef _SgVisualDrawingArea_h
#define _SgVisualDrawingArea_h

#include <Xm/Xm.h>
#include <Xm/DrawingA.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Class record constants */

externalref WidgetClass sgVisualDrawingAreaWidgetClass;

typedef struct _SgVisualDrawingAreaClassRec * SgVisualDrawingAreaWidgetClass;
typedef struct _SgVisualDrawingAreaRec      * SgVisualDrawingAreaWidget;


#ifndef SgIsVisualDrawingArea
#define SgIsVisualDrawingArea(w)  (XtIsSubclass (w, sgVisualDrawingAreaWidgetClass))
#endif



/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateVisualDrawingArea() ;
extern void SgVisualDrawingAreaInstallColormap();
extern void SgVisualDrawingAreaUninstallColormap();

#else

extern Widget SgCreateVisualDrawingArea( 
                        Widget p,
                        String name,
                        ArgList args,
                        Cardinal n) ;
extern void SgVisualDrawingAreaInstallColormap(Widget w);
extern void SgVisualDrawingAreaUninstallColormap(Widget w);

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

/* new resource declarations */
#ifndef SgNinstallColormap
#define SgNinstallColormap "installColormap"
#define SgCInstallColormap "InstallColormap"
#endif
#ifndef SgNditherBackground
#define SgNditherBackground "ditherBackground"
#define SgCDitherBackground "DitherBackground"
#endif

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgVisualDrawingArea_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
