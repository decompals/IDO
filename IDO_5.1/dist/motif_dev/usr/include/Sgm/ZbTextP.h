#ifndef _SGZBTEXTP_H_
#define _SGZBTEXTP_H_

#include <Xm/XmP.h>
#include <Sgm/ZbText.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif


#define MAXDIVS 255

/* ZbText class structure */

typedef struct _SgZbTextClassPart
{
   int foo;
#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgZbTextClassPart;


/* Full class record declaration for ZbText class */

typedef struct _SgZbTextClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart  manager_class;
    SgZbTextClassPart    zbtext_class;
} SgZbTextClassRec;


externalref SgZbTextClassRec sgZbTextClassRec;

/* ZbText instance record */

typedef struct _SgZbTextPart
{

  Widget zoomBar;
  Widget text;

#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgZbTextPart;


/* Full instance record declaration */

typedef struct _SgZbTextRec {
   CorePart	  core;
   CompositePart  composite;
   ConstraintPart constraint;
   XmManagerPart  manager;
   SgZbTextPart    zbtext;
} SgZbTextRec;


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmPButtonP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */


