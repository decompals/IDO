/***********************************************************************
 *
 * ZbText Widget
 *
 ***********************************************************************/

#ifndef _SGZBTEXT_H_
#define _SGZBTEXT_H_

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef SgIsZbText
#define SgIsZbText(w) XtIsSubclass(w, sgZbTextWidgetClass)
#endif /* SgIsZbText */

#define SgZBTEXT_TEXT 0
#define SgZBTEXT_ZOOM_BAR 1

/* ZbText Widget */

externalref WidgetClass sgZbTextWidgetClass;

typedef struct _SgZbTextClassRec *SgZbTextWidgetClass;
typedef struct _SgZbTextRec      *SgZbTextWidget;


/********    Public Function Declarations    ********/

extern Widget SgCreateZbText( Widget parent, char *name, ArgList arglist, Cardinal argcount) ;

extern Widget SgZbTextGetChild( Widget w, int child );


/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SGZBTEXT_H_ */
/* DON'T ADD ANYTHING AFTER THIS #endif */

