#ifndef _SGFINDERP_H_
#define _SGFINDERP_H_

#include "Finder.h"
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif


#define MAXDIVS 255

#define MIN_TEXT_WIDTH 40
#define DESIRED_SEPARATOR 2

/* Finder class structure */

typedef struct _SgFinderClassPart
{
   int foo;
#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgFinderClassPart;


/* Full class record declaration for Finder class */

typedef struct _SgFinderClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart  manager_class;
    SgFinderClassPart    finder_class;
} SgFinderClassRec;


externalref SgFinderClassRec sgFinderClassRec;

/* Finder instance record */

typedef struct _SgFinderPart
{

  Widget dropPocket;
  Widget zbText;
  Widget zoomBar;
  Widget text;
  Widget historyBar;

  Dimension divisions[MAXDIVS];
  XFontStruct * fontStruct;

  int divisionCount;
  XmTextPosition oldFirstPos, oldLastPos;
  unsigned char separator;

  Boolean addHistoryOnActivate;
  Boolean useDropPocket;
  Boolean useHistoryMenu;
  Pixmap historyPixmap;
  Pixmap insensitiveHistoryPixmap;

  XtCallbackList activate_callback;
  XtCallbackList value_changed_callback;

  SgSetTextFunc setTextSectionFunc;

#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgFinderPart;


/* Full instance record declaration */

typedef struct _SgFinderRec {
   CorePart	  core;
   CompositePart  composite;
   ConstraintPart constraint;
   XmManagerPart  manager;
   SgFinderPart    finder;
} SgFinderRec;


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmPButtonP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */


