/*
* $XConsortium: Text.h,v 1.35 90/05/08 15:18:22 converse Exp $
*/


/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _XawText_h
#define _XawText_h

#include <X11/Xaw/TextI.h>
#include <X11/Xaw/TextSink.h>
#include <X11/Xaw/TextSrc.h>

/****************************************************************
 *
 * Text widget
 *
 ****************************************************************/

/* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 dialogHOffset	     Margin		int		10
 dialogVOffset	     Margin		int		10
 displayCaret	     Output		Boolean		True
 displayPosition     TextPosition	int		0
 editType	     EditType		XtTextEditType	XttextRead
 height		     Height		Dimension	font height
 insertPosition	     TextPosition	int		0
 leftMargin	     Margin		Dimension	2
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 selectTypes	     SelectTypes	Pointer		(internal)
 selection	     Selection		Pointer		empty selection
 sensitive	     Sensitive		Boolean		True
 textSink	     TextSink		Pointer		(none)
 textSource	     TextSource		Pointer		(none)
 width		     Width		Dimension	100
 x		     Position		int		0
 y		     Position		int		0

*/

#define XtEtextScrollNever "never"
#define XtEtextScrollWhenNeeded "whenneeded"
#define XtEtextScrollAlways "always"

#define XtEtextWrapNever "never"
#define XtEtextWrapLine "line"
#define XtEtextWrapWord "word"

#define XtEtextResizeNever "never"
#define XtEtextResizeWidth "width"
#define XtEtextResizeHeight "height"
#define XtEtextResizeBoth "both"

#define XtNautoFill "autoFill"
#define XtNbottomMargin "bottomMargin"
#define XtNdialogHOffset "dialogHOffset"
#define XtNdialogVOffset "dialogVOffset"
#define XtNdisplayCaret "displayCaret"
#define XtNdisplayPosition "displayPosition"
#define XtNinsertPosition "insertPosition"
#define XtNleftMargin "leftMargin"
#define XtNresize "resize"
#define XtNrightMargin "rightMargin"
#define XtNscrollVertical "scrollVertical"
#define XtNscrollHorizontal "scrollHorizontal"
#define XtNselectTypes "selectTypes"
#define XtNselection "selection"
#define XtNtopMargin "topMargin"
#define XtNwrap "wrap"

#define XtCAutoFill "AutoFill"
#define XtCResize "Resize"
#define XtCScroll "Scroll"
#define XtCSelectTypes "SelectTypes"
#define XtCWrap "Wrap"

/* Return Error code for XawTextSearch */

#define XawTextSearchError      (-12345L)

/* Return codes from XawTextReplace */

#define XawReplaceError	       -1
#define XawEditDone		0
#define XawEditError		1
#define XawPositionError	2

extern Atom FMT8BIT;

/* Class record constants */

extern WidgetClass textWidgetClass;

typedef struct _TextClassRec *TextWidgetClass;
typedef struct _TextRec      *TextWidget;

#ifdef XAW_BC
/************************************************************
 *
 * This Stuff is only for compatibility, and will go away in 
 * future releases.                                         */

/* preserved for Back Compatability only. */

#define XawTextSource Widget
#define XtTextSource  Widget

#define wordBreak		0x01
#define scrollVertical		0x02
#define scrollHorizontal	0x04
#define scrollOnOverflow	0x08
#define resizeWidth		0x10
#define resizeHeight		0x20
#define editable		0x40

typedef long XtTextPosition;

#define XtTextBlock                XawTextBlock
#define XtTextBlockPtr             XawTextBlockPtr

#define EditDone	           XawEditDone
#define EditError	           XawEditError
#define PositionError	           XawPositionError

#define XtEditDone	           XawEditDone
#define XtEditError	           XawEditError
#define XtPositionError	           XawPositionError

#define XttextRead                 XawtextRead
#define XttextAppend               XawtextAppend
#define XttextEdit                 XawtextEdit
#define XtTextEditType             XawTextEditType
#define XtselectNull               XawselectNull

#define XtselectPosition           XawselectPosition
#define XtselectChar               XawselectChar
#define XtselectWord               XawselectWord
#define XtselectLine               XawselectLine
#define XtselectParagraph          XawselectParagraph
#define XtselectAll                XawselectAll
#define XtTextSelectType           XawTextSelectType

#define XtTextDisableRedisplay     XawTextDisableRedisplay
#define XtTextEnableRedisplay      XawTextEnableRedisplay
#define XtTextGetSource            XawTextGetSource

#define XtTextDisplay              XawTextDisplay
#define XtTextDisplayCaret         XawTextDisplayCaret
#define XtTextSetSelectionArray    XawTextSetSelectionArray
#define XtTextSetLastPos           XawTextSetLastPos
#define XtTextGetSelectionPos      XawTextGetSelectionPos
#define XtTextSetSource            XawTextSetSource
#define XtTextReplace              XawTextReplace
#define XtTextTopPosition          XawTextTopPosition
#define XtTextSetInsertionPoint    XawTextSetInsertionPoint
#define XtTextGetInsertionPoint    XawTextGetInsertionPoint
#define XtTextUnsetSelection       XawTextUnsetSelection
#define XtTextChangeOptions        XawTextChangeOptions
#define XtTextGetOptions           XawTextGetOptions
#define XtTextSetSelection         XawTextSetSelection
#define XtTextInvalidate           XawTextInvalidate

#define XtDiskSourceCreate         XawDiskSourceCreate
#define XtDiskSourceDestroy        XawDiskSourceDestroy
#define XtStringSourceCreate       XawStringSourceCreate
#define XtStringSourceDestroy      XawStringSourceDestroy

#ifdef __cplusplus
extern "C" {					/* for C++ V2.0 */
#endif

extern void XawTextChangeOptions(
#if NeedFunctionPrototypes
    Widget		/* w */,
    int			/* options */
#endif
);

extern int XawTextGetOptions(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern void XawTextSetLastPos(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* lastPos */
#endif
);

#ifdef __cplusplus
extern "C" {					/* for C++ V2.0 */
#endif

/*************************************************************/
#endif /* XAW_BC */

#ifdef __cplusplus
extern "C" {					/* for C++ V2.0 */
#endif

extern void XawTextDisplay(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
); 

extern void XawTextEnableRedisplay(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern void XawTextDisableRedisplay(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern void XawTextSetSelectionArray(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextSelectType*	/* sarray */
#endif
);

extern void XawTextGetSelectionPos(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition*	/* begin_return */,
    XawTextPosition*	/* end_return */
#endif
);

extern void XawTextSetSource(
#if NeedFunctionPrototypes
    Widget		/* w */,
    Widget		/* source */,
    XawTextPosition	/* position */
#endif
);

extern int XawTextReplace(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* start */,
    XawTextPosition	/* end */,
    XawTextBlock*	/* text */
#endif
);

extern XawTextPosition XawTextTopPosition(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern void XawTextSetInsertionPoint(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* position */
#endif
);

extern XawTextPosition XawTextGetInsertionPoint(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern void XawTextUnsetSelection(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern void XawTextSetSelection(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* left */,
    XawTextPosition	/* right */
#endif
);

extern void XawTextInvalidate(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* from */,
    XawTextPosition	/* to */
#endif
);

extern Widget XawTextGetSource(
#if NeedFunctionPrototypes
    Widget		/* w */
#endif
);

extern XawTextPosition XawTextSearch(
#if NeedFunctionPrototypes
    Widget			/* w */,
#if NeedWidePrototypes
    /* XawTextScanDirection */ int /* dir */,
#else
    XawTextScanDirection	/* dir */,
#endif
    XawTextBlock*		/* text */
#endif
);

extern void XawTextDisplayCaret(
#if NeedFunctionPrototypes
    Widget		/* w */,
#if NeedWidePrototypes
    /* Boolean */ int	/* visible */
#else
    Boolean		/* visible */
#endif
#endif
);

#ifdef __cplusplus
}						/* for C++ V2.0 */
#endif

/*
 * For R3 compatability only. 
 */

#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/AsciiSink.h>

#endif /* _XawText_h */
/* DON'T ADD STUFF AFTER THIS #endif */
