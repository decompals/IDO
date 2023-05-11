/***********************************************************************
 *
 * Finder Widget
 *
 ***********************************************************************/

#ifndef _SGFINDER_H_
#define _SGFINDER_H_

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef SgIsFinder
#define SgIsFinder(w) XtIsSubclass(w, SgFinderWidgetClass)
#endif /* SgIsFinder */

/* Definitions of char constants for resources */
#ifndef SgNaddHistoryOnActivate
#define SgNaddHistoryOnActivate "addHistoryOnActivate"
#endif
#ifndef SgCAddHistoryOnActivate
#define SgCAddHistoryOnActivate "AddHistoryOnActivate"
#endif
#ifndef SgNseparator
#define SgNseparator "separator"
#endif
#ifndef SgCSeparator
#define SgCSeparator "Separator"
#endif
#ifndef SgNsetTextSectionFunc
#define SgNsetTextSectionFunc "setTextSectionFunc"
#endif
#ifndef SgCSetTextSectionFunc
#define SgCSetTextSectionFunc "SetTextSectionFunc"
#endif
#ifndef SgNhistoryPixmap
#define SgNhistoryPixmap "historyPixmap"
#endif
#ifndef SgCHistoryPixmap
#define SgCHistoryPixmap "HistoryPixmap"
#endif

#define SgFINDER_TEXT 0
#define SgFINDER_ZOOM_BAR 1
#define SgFINDER_DROP_POCKET 2
#define SgFINDER_HISTORY_MENUBAR 3

/* Finder Widget */

externalref WidgetClass sgFinderWidgetClass;

typedef struct _SgFinderClassRec *SgFinderWidgetClass;
typedef struct _SgFinderRec      *SgFinderWidget;


/********    Public Function Declarations    ********/


typedef void (*SgSetTextFunc)(SgFinderWidget ft, int section);

extern Widget SgCreateFinder( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;


extern void SgFinderAddHistoryItem( SgFinderWidget ft, char * str );
extern void SgFinderClearHistory( SgFinderWidget ft );
extern char * SgFinderGetTextString( SgFinderWidget ft );
extern void SgFinderSetTextString( SgFinderWidget ft, char * value );
extern Widget SgFinderGetChild( SgFinderWidget ft, int child );

/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SGFINDER_H_ */
/* DON'T ADD ANYTHING AFTER THIS #endif */

