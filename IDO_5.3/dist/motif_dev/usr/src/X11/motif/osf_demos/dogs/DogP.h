/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: DogP.h,v $ $Revision: 1.3 $ $Date: 1993/10/23 21:06:11 $ */

/*****************************************************************************
*
*  DogP.H - widget private header file
*  
******************************************************************************/

#ifndef _DogP_h
#define _DogP_h

#include <Xm/PrimitiveP.h>
#include "Dog.h"

void _DogDrawPixmap();
void _DogPosition();

#define DogIndex (XmPrimitiveIndex + 1)

typedef struct _DogClassPart {
    XtPointer reserved;
} DogClassPart;

typedef struct _DogClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    DogClassPart dog_class;
} DogClassRec;

extern DogClassRec dogClassRec;

typedef struct _DogPart {
    int wag_time;
    int bark_time;
    XtCallbackList bark_callback;

    Boolean wagging;
    Boolean barking;
    GC draw_GC;
    Pixmap up_pixmap;
    Pixmap down_pixmap;
    Pixmap bark_pixmap;
    Position pixmap_x;
    Position pixmap_y;
    Position draw_x;
    Position draw_y;
    Dimension draw_width;
    Dimension draw_height;
    int curr_px;
    Pixmap curr_pixmap;
    Dimension curr_width;
    Dimension curr_height;
    XtIntervalId wagId;
    XtIntervalId barkId;
} DogPart;

typedef struct _DogRec {
    CorePart core;
    XmPrimitivePart primitive;
    DogPart dog;
} DogRec;

#define UpPx 0
#define DownPx 1
#define BarkPx 2

#endif /* _DogP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
