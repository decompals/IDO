#ifndef _SGDROPPOCKET_H
#define _SGDROPPOCKET_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif


/* resource names */
#ifndef SgNname
#define SgNname "name"
#endif

#ifndef SgCName
#define SgCName "Name"
#endif

#ifndef SgNactivePixel
#define SgNactivePixel "activePixel"
#endif

#ifndef SgCActivePixel
#define SgCActivePixel "ActivePixel"
#endif

#ifndef SgNiconDataBasePath
#define SgNiconDataBasePath "iconDataBasePath"
#endif

#ifndef SgCIconDataBasePath
#define SgCIconDataBasePath "IconDataBasePath"
#endif

#ifndef SgNiconUpdateCallback
#define SgNiconUpdateCallback "iconUpdateCallback"
#endif

#ifndef SgCR_ICON_CHANGE
#define SgCR_ICON_CHANGE 0x5f
#endif


/* Class record constants */

externalref WidgetClass sgDropPocketWidgetClass;

typedef struct _SgDropPocketClassRec * SgDropPocketWidgetClass;
typedef struct _SgDropPocketRec      * SgDropPocketWidget;


#ifndef SgIsDropPocket
#define SgIsDropPocket(w)  (XtIsSubclass (w, sgDropPocketWidgetClass))
#endif

/*    Callback structure declaration  */

typedef struct
{
    int     reason;
    XEvent  *event;
    Window  window;
    XmString iconName;
    char *  iconData;
} SgDropPocketCallbackStruct;


/*    Public Function Declarations    */

extern Widget SgCreateDropPocket( Widget p, String name, ArgList args, Cardinal n) ;


#ifdef __cplusplus
}  /* Closes 'extern "C"' for C++. */
#endif

#endif /* _SGDROPPOCKET_H */
/* DON'T ADD ANYTHING AFTER THIS */


