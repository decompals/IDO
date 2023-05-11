/***********************************************************************
 *
 * DynaMenu Widget
 *
 ***********************************************************************/

#ifndef _SGDYNAMENU_H_
#define _SGDYNAMENU_H_

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif


#ifndef SgNhistoryListItems
#define SgNhistoryListItems "historyListItems"
#endif
#ifndef SgNmaxHistoryCount
#define SgNmaxHistoryCount "historyListCount"
#endif
#ifndef SgNhistoryListItemCount
#define SgNhistoryListItemCount "historyListItemCount"
#endif

#ifndef SgCHistoryListItems
#define SgCHistoryListItems "HistoryListItems"
#endif
#ifndef SgCMaxHistoryCount
#define SgCMaxHistoryCount "HistoryListCount"
#endif
#ifndef SgCHistoryListItemCount
#define SgCHistoryListItemCount "HistoryListItemCount"
#endif


#ifndef SgCR_DYNA_PUSH
#define SgCR_DYNA_PUSH 3134
#endif

#ifndef SgNdynaPushCallback
#define SgNdynaPushCallback "dynaPushCallback"
#endif


#ifndef SgIsDynaMenu
#define SgIsDynaMenu(w) XtIsSubclass(w, SgDynaMenuWidgetClass)
#endif /* SgIsDynaMenu */


/* DynaMenu Widget */

externalref WidgetClass sgDynaMenuWidgetClass;

typedef struct _SgDynaMenuClassRec *SgDynaMenuWidgetClass;
typedef struct _SgDynaMenuRec      *SgDynaMenuWidget;


/********    Public Function Declarations    ********/

typedef struct
{
    int     reason;
    XEvent  *event;
    int	    button_number;
} SgDynaMenuCallbackStruct;


extern Widget SgCreateDynaMenu( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;


extern void SgDynaMenuAddHistoryItem( SgDynaMenuWidget ft, char * str );
extern void SgDynaMenuClearHistory( SgDynaMenuWidget ft );

/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SGDYNAMENU_H_ */
/* DON'T ADD ANYTHING AFTER THIS #endif */



