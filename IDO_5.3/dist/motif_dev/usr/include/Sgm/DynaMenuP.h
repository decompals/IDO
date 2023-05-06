#ifndef _SGDYNAMENUP_H_
#define _SGDYNAMENUP_H_

#include <Xm/XmP.h>
#include <Sgm/DynaMenu.h>
#include <Xm/ManagerP.h>
#include <Xm/RowColumn.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>
#include <Xm/MenuShell.h>
#include <Xm/Label.h>
#include <Xm/DrawnB.h>

#ifdef __cplusplus
extern "C" {
#endif

/* DynaMenu class structure */

typedef struct _SgDynaMenuClassPart
{
   int foo;
#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgDynaMenuClassPart;


/* Full class record declaration for DynaMenu class */

typedef struct _SgDynaMenuClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart  manager_class;
    SgDynaMenuClassPart dynaMenu_class;
} SgDynaMenuClassRec;


externalref SgDynaMenuClassRec sgDynaMenuClassRec;

/* DynaMenu instance record */

typedef struct _SgDynaMenuPart
{

  Widget historyPopup;
  Widget historyButton;

  XmString * historyItems;
  int historyItemCount;
  int maxHistoryCount;

  XtCallbackList dynaPushCallback;

#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgDynaMenuPart;


/* Full instance record declaration */

typedef struct _SgDynaMenuRec {
   CorePart	  core;
   CompositePart  composite;
   ConstraintPart constraint;
   XmManagerPart  manager;
   SgDynaMenuPart dynaMenu;
} SgDynaMenuRec;


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SGDYNAMENU_H_ */
/* DON'T ADD ANYTHING AFTER THIS #endif */


