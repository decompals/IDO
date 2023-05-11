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
#ifndef _SgArc_h
#define _SgArc_h

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

externalref WidgetClass sgArcWidgetClass;
typedef struct _SgArcClassRec *SgArcWidgetClass;
typedef struct _SgArcRec      *SgArcWidget;

/*#define  XmARC_BIT    (1<<29) */

#define SgIsArc(w)  XtIsSubclass(w, sgArcWidgetClass)


#define XmNlabel		"label"
#define XmNarcEditedCallback	"arcEditedCallback"
#define XmNaddArcCallback       "addArcCallback"
#define XmNto		        "to"
#define XmCTo	                "To"
#define XmNfrom		        "from"
#define XmCFrom	             	"From"
#define XmNmapLabel             "mapLabel"
#define XmCMapLabel             "MapLabel"
#define XmNdelta		"delta"
#define XmCDelta		"Delta"
#define XmNarcDirection		"arcDirection" /* XmCDirection class */
#define XmNattribute		"attribute"

#define XmNtoPosition		"toPosition"
#define XmNfromPosition		"fromPosition"
#define XmNtoSide		"toSide"
#define XmNfromSide		"fromSide"
#define XmCSide		        "Side"

#define XmNstyle                "style"
#define XmCStyle                "Style"
#define XmRLineStyle             "LineStyle"
#define XmNcapStyle             "capStyle"
#define XmCCapStyle             "CapStyle"
#define XmRCapStyle             "CapStyle"
#define XmNdashes               "dashes"
#define XmCDashes               "Dashes"
#define XmNdashOffset           "dashOffset"
#define XmCDashOffset           "DashOffset"
#define XmCDirection            "Direction"
#define XmRArcDirection         "ArcDirection"
#define XmNarcWidth             "arcWidth"
#define XmCArcWidth         	"ArcWidth"
#define XmCAttribute         	"Attribute"

#define XmLEFT    0
#define XmRIGHT   1
#define XmBOTTOM  2
#define XmTOP     3


#ifndef _NO_PROTO

extern Widget SgCreateArc(Widget,String,ArgList,Cardinal);
extern Widget SgCreateAttachedArc(Widget,String,Widget, Widget, ArgList,Cardinal);
extern void SgArcGetPos(Widget, Position *, Position *, Position *, Position *);
#else    /* _NO_PROTO */

extern Widget SgCreateArc();
extern Widget SgCreateAttachedArc();
extern void SgArcGetPos();
#endif  /* _NO_PROTO */


#define XmBIDIRECTED  0
#define XmDIRECTED    1
#define XmUNDIRECTED  2

#define XmLineSolid               0
#define XmLineOnOffDash           1
#define XmLineDoubleDash          2


#define XmCapNotLast              0
#define XmCapButt                 1
#define XmCapRound                2
#define XmCapProjecting           3

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /* _SgArc_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */




