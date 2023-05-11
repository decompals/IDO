/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: Dog.h,v $ $Revision: 1.2 $ $Date: 1993/10/23 21:06:09 $ */

/*****************************************************************************
*
*  Dog.h - widget public header file
*  
******************************************************************************/

#ifndef _Dog_h
#define _Dog_h

#define USING_UIL

externalref WidgetClass dogWidgetClass;

typedef struct _DogClassRec *DogWidgetClass;
typedef struct _DogRec *DogWidget;

#define DogNbarkCallback "barkCallback"
#define DogNwagTime "wagTime"
#define DogNbarkTime "barkTime"

#define DogCWagTime "WagTime"
#define DogCBarkTime "BarkTime"

#define IsDog(w) XtIsSubclass((w), dogWidgetClass)

extern Widget DogCreate();
#ifdef	USING_UIL
extern int DogMrmInitialize();
#endif

#endif /* _Dog_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
