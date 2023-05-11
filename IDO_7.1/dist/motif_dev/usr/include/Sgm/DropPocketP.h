#ifndef _SGDROPPOCKETP_H
#define _SGDROPPOCKETP_H

#include <Xm/XmP.h>
#include <limits.h>
#include <Xm/PrimitiveP.h>
#include <Xm/DragDrop.h>
#include <oz/fileicon.h>
#include <errno.h>
#include <Sgm/DropPocket.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  DropPocket widget class Part */

typedef struct
{
  int foo;   /* does nothing */
#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
  caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgDropPocketClassPart;


/* Class record declaration */

typedef struct _SgDropPocketClassRec
{
	CoreClassPart		core_class;
	XmPrimitiveClassPart	primitive_class;
	SgDropPocketClassPart   dropPocketClass;
} SgDropPocketClassRec;

externalref SgDropPocketClassRec sgDropPocketClassRec;

/* Icon data structure for internal use */
typedef struct _Icon
{
  fiIconType type;
  Boolean valid;
  float sx, sy;
} SgIcon;


typedef struct
{
  String iconDataBasePath;
  XmString name;
  Boolean loadedDataBase;
  fiFileIconDB iconDB;
  SgIcon icon;
  GC gc;
  Pixel normalPixel;
  Pixel activePixel;
  Atom _SGI_ICON;

  XtCallbackList iconUpdateCallback;
  Pixmap drag_pixmap;
  Pixmap drag_pixmap_mask;

#ifdef __sgi
/* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgDropPocketPart;


/* DropPocket widget instance Record */


typedef struct _SgDropPocketRec
{
	CorePart		core;
	XmPrimitivePart		primitive;
	SgDropPocketPart        dropPocket;
} SgDropPocketRec;



#ifdef __cplusplus
}   /* Closes 'extern "C"' for C++. */
#endif

#endif /* _SGDROPPOCKETP_H */
/* DON'T ADD ANYTHING AFTER THIS */
