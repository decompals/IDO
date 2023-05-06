/*******************************************************************************
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
*******************************************************************************/
#ifndef _SgIconGP_h
#define _SgIconGP_h

#include "IconG.h"
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>


#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmArrowPixmap
{
   Dimension    height, 
                width;
   unsigned int depth;
   Pixel        top_shadow_color, 
                bottom_shadow_color, 
		foreground_color;
   Display     *display;
   Screen      *screen;
   Pixmap       pixmap;
} XmArrowPixmap;


/*************************************************************/
/* The Icon Gadget Cache Object's class and instance records*/
/*************************************************************/

typedef struct _SgIconGCacheObjClassPart
{
    int foo;
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
    caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgIconGCacheObjClassPart;


typedef struct _SgIconGCacheObjClassRec     /* icon cache class record */
{
    ObjectClassPart			object_class;
    XmExtClassPart                 	ext_class;
    SgIconGCacheObjClassPart	 	icon_class_cache;
} SgIconGCacheObjClassRec;

externalref SgIconGCacheObjClassRec sgIconGCacheObjClassRec;


/*  The Icon Gadget Cache instance record  */

typedef struct _SgIconGCacheObjPart
{
        unsigned char	alignment;
        unsigned char	string_direction;
	
	Dimension	margin_height;   /* margin around widget */
	Dimension	margin_width;

	Dimension	margin_left;    /* additional margins on */
	Dimension	margin_right;   /* each side of widget */
	Dimension	margin_top;     /* text (or pixmap) is placed */
	Dimension	margin_bottom;  /* inside the margins */
	Dimension	margin_middle;  /* margin between icon and label */

        Pixel           foreground;
	Pixel           background;
	Pixel           top_shadow_color;
	Pixel           bottom_shadow_color;
	Pixel           highlight_color;

	Boolean         recompute_size;

	Boolean		skipCallback; /* set by RowColumn when entryCallback
					is provided. */
	unsigned char   menu_type;
	
	Boolean		show_cascade;
	Dimension       pixmap_width;
	Dimension       pixmap_height;
        int             icon_placement;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgIconGCacheObjPart;

typedef struct _SgIconGCacheObjRec
{
    XmExtPart	             ext;
    SgIconGCacheObjPart     icon_cache;
} SgIconGCacheObjRec;

/*  The Icon Widget Class and instance records  */
/*************************************************/

typedef struct _SgIconGadgetClassPart     /* icon class record */
{
        XtWidgetProc		setOverrideCallback;
	XmMenuProc		menuProcs;
        XtPointer		extension; /* Pointer to extension record */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgIconGadgetClassPart;

typedef struct _SgIconGadgetClassRec
{
    RectObjClassPart       rect_class;
    XmGadgetClassPart      gadget_class;
    SgIconGadgetClassPart  icon_class;
} SgIconGadgetClassRec;

externalref SgIconGadgetClassRec sgIconGadgetClassRec;

typedef struct _SgIconGadgetPart
{
	_XmString	_label;  /* String sent to this widget */
        XmFontList	font;
	Pixmap		bitmap; 
	Pixmap		bitmap_insen; 
        Pixmap		pixmap; 
        Pixmap		pixmap_insen; 
	unsigned char   shadow_type;
	XtCallbackList  double_click_callback;

        /* PRIVATE members -- values computed by IconWidgetClass methods */

        GC		normal_GC;   /* GC for text */	
	GC		topshadow_GC;   
        GC		bottomshadow_GC;
	GC		insensitive_GC;
	GC		inverse_GC; 
        XRectangle	TextRect;    /* The bounding box of the text, or clip
                                        rectangle of the window; whichever is
                                        smaller */
        XRectangle	pixmapRect; /* The bounding box of the pixmap, or clip
                                        rectangle of the window; whichever is
                                        smaller */

	Pixmap        cascade_pixmap;         /* pixmap for the cascade */
        int           cascade_width;
        int           cascade_height;

	SgIconGCacheObjPart *cache;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgIconGadgetPart;


typedef struct _SgIconGadgetRec
{
   ObjectPart        object;
   RectObjPart       rectangle;
   XmGadgetPart      gadget;
   SgIconGadgetPart  icon;
} SgIconGadgetRec;

/* Inherited  Functions exported by icon */

#define XmInheritSetOverrideCallback ((XtWidgetProc) _XtInherit)
#define XmInheritResize  ((XtWidgetProc) _XtInherit)

/* MACROS */
/********
 * Macros for cached instance fields
 */
#define IconG_Alignment(w)		(((SgIconGadget)(w)) -> \
					   icon.cache-> alignment)
#define IconG_StringDirection(w)		(((SgIconGadget)(w)) -> \
					   icon.cache-> string_direction)
#define IconG_MarginHeight(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_height)
#define IconG_MarginWidth(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_width)
#define IconG_MarginLeft(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_left)
#define IconG_MarginRight(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_right)
#define IconG_MarginTop(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_top)
#define IconG_MarginBottom(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_bottom)
#define IconG_MarginMiddle(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> margin_middle)
#define IconG_RecomputeSize(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> recompute_size)
#define IconG_SkipCallback(w)		(((SgIconGadget)(w)) -> \
                                           icon.cache-> skipCallback)
#define IconG_MenuType(w)		(((SgIconGadget)(w)) -> \
					 icon.cache-> menu_type)

#define IconG_Foreground(w)		(((SgIconGadget)(w)) ->icon.cache->foreground)
#define IconG_Background(w)		(((SgIconGadget)(w)) ->icon.cache->background)
#define IconG_PixmapWidth(w)		(((SgIconGadget)(w)) ->icon.cache->pixmap_width) 
#define IconG_PixmapHeight(w)		(((SgIconGadget)(w)) ->icon.cache->pixmap_height) 
#define IconG_IconPlacement(w)		(((SgIconGadget)(w)) ->icon.cache->icon_placement) 
#define IconG_TopShadowColor(w)		(((SgIconGadget)(w)) ->icon.cache->top_shadow_color) 
#define IconG_BottomShadowColor(w)      (((SgIconGadget)(w)) ->icon.cache->bottom_shadow_color) 
#define IconG_ShowCascade(w)          (((SgIconGadget)(w)) ->icon.cache->show_cascade) 

/********
 * Macros for UNcached instance fields
 */
#define IconG_ShadowType(w)		(((SgIconGadget)(w)) ->icon.shadow_type) 
#define IconG_CascadePixmap(w)          (((SgIconGadget)(w)) ->icon.cascade_pixmap)
#define IconG_CascadePixmap_width(w)     (((SgIconGadget)(w)) ->icon.cascade_width)
#define IconG_CascadePixmap_height(w)    (((SgIconGadget)(w)) ->icon.cascade_height)
#define IconG_TopShadowGC(w)		(((SgIconGadget)(w)) ->icon.topshadow_GC) 
#define IconG_BottomShadowGC(w)		(((SgIconGadget)(w)) ->icon.bottomshadow_GC) 
#define IconG_InverseGC(w)		(((SgIconGadget)(w)) ->icon.inverse_GC) 

#define IconG__label(w)			(((SgIconGadget)(w)) -> \
					   icon._label)
#define IconG__acceleratorText(w)	(((SgIconGadget)(w)) -> \
                                           icon._acc_text)
#define IconG_Font(w)			(((SgIconGadget)(w)) -> \
					   icon.font)
#define IconG_Mnemonic(w)		(((SgIconGadget)(w)) -> \
					   icon.mnemonic)
#define IconG_MnemonicCharset(w)         (((SgIconGadget)(w)) -> \
                                           icon.mnemonicCharset)
#define IconG_Accelerator(w)		(((SgIconGadget)(w)) -> \
                                           icon.accelerator)
#define IconG_Pixmap(w)			(((SgIconGadget)(w)) -> \
                                           icon.pixmap)
#define IconG_PixmapInsensitive(w)	(((SgIconGadget)(w)) -> \
                                           icon.pixmap_insen)
#define IconG_Bitmap(w)		        (((SgIconGadget)(w)) -> icon.bitmap)
#define IconG_BitmapInsensitive(w)	(((SgIconGadget)(w)) -> icon.bitmap_insen)

#define IconG_NormalGC(w)		(((SgIconGadget)(w)) -> \
                                           icon.normal_GC)
#define IconG_InsensitiveGC(w)		(((SgIconGadget)(w)) -> \
                                           icon.insensitive_GC)
#define IconG_TextRect(w)		(((SgIconGadget)(w)) -> \
                                           icon.TextRect)
#define IconG_PixmapRect(w)		(((SgIconGadget)(w)) -> \
                                           icon.pixmapRect)


/********
 * Convenience Macros 
 */
#define IconG_TextRect_x(w)		(IconG_TextRect(w).x)
                                           
#define IconG_TextRect_y(w)		(IconG_TextRect(w).y)
                                           
#define IconG_TextRect_width(w)		(IconG_TextRect(w).width)
                                           
#define IconG_TextRect_height(w)	(IconG_TextRect(w).height)

#define IconG_PixmapRect_x(w)		(IconG_PixmapRect(w).x)
                                           
#define IconG_PixmapRect_y(w)		(IconG_PixmapRect(w).y)
                                           
#define IconG_PixmapRect_width(w)		(IconG_PixmapRect(w).width)
                                           
#define IconG_PixmapRect_height(w)		(IconG_PixmapRect(w).height)

                                           
#define IconG_Cache(w)			(((SgIconGadget)(w))-> \
					   icon.cache)
#define IconG_Shadow(w)                   (((SgIconGadget)(w))->gadget.shadow_thickness)
#define IconG_Highlight(w)                (((SgIconGadget)(w))->gadget.highlight_thickness)
#define IconG_Baseline(w)                 (_XmStringBaseline ( \
                                         ((SgIconGadget)(w))->icon.font,\
                                         ((SgIconGadget)(w))->icon._icon))
#define IconG_ClassCachePart(w) \
	(((SgIconGadgetClass)sgIconGadgetClass)->gadget_class.cache_part)


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

extern int _SgIconCacheCompare() ;
extern void _SgCalcIconGDimensions() ;
extern void _SgReCacheIconG() ;
extern void _SgAssignIconG_MarginHeight() ;
extern void _SgAssignIconG_MarginWidth() ;
extern void _SgAssignIconG_MarginLeft() ;
extern void _SgAssignIconG_MarginRight() ;
extern void _SgAssignIconG_MarginTop() ;
extern void _SgAssignIconG_MarginBottom() ;
extern void _SgProcessDrag() ;

#else

extern int _SgIconCacheCompare( 
                        XtPointer A,
                        XtPointer B) ;
extern void _SgCalcIconGDimensions( 
                        Widget wid) ;
extern void _SgReCacheIconG( 
                        Widget wid) ;
extern void _SgAssignIconG_MarginHeight( 
                        SgIconGadget lw,
#if NeedWidePrototypes
                        int value) ;
#else
                        Dimension value) ;
#endif /* NeedWidePrototypes */
extern void _SgAssignIconG_MarginWidth( 
                        SgIconGadget lw,
#if NeedWidePrototypes
                        int value) ;
#else
                        Dimension value) ;
#endif /* NeedWidePrototypes */
extern void _SgAssignIconG_MarginLeft( 
                        SgIconGadget lw,
#if NeedWidePrototypes
                        int value) ;
#else
                        Dimension value) ;
#endif /* NeedWidePrototypes */
extern void _SgAssignIconG_MarginRight( 
                        SgIconGadget lw,
#if NeedWidePrototypes
                        int value) ;
#else
                        Dimension value) ;
#endif /* NeedWidePrototypes */
extern void _SgAssignIconG_MarginTop( 
                        SgIconGadget lw,
#if NeedWidePrototypes
                        int value) ;
#else
                        Dimension value) ;
#endif /* NeedWidePrototypes */
extern void _SgAssignIconG_MarginBottom( 
                        SgIconGadget lw,
#if NeedWidePrototypes
                        int value) ;
#else
                        Dimension value) ;
#endif /* NeedWidePrototypes */
extern void _SgProcessDrag( 
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params) ;

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgIconGP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
