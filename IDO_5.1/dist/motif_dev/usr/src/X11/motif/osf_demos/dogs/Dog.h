/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: Dog.h,v $ $Revision: 1.1 $ $Date: 1992/05/11 11:01:36 $ */

/*****************************************************************************
*
*  Dog.h - widget public header file
*  
******************************************************************************/

#ifndef _Dog_h
#define _Dog_h

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
extern int DogMrmInitialize();

#endif /* _Dog_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
