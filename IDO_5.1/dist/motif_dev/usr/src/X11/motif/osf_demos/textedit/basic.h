/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: basic.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:52 $ */

/************************************************************
 *     basic.h -- basic functions and macros
 ************************************************************/

#define false 0
#define true 1

#include <X11/Intrinsic.h>

#define BasicMalloc(siz) XtMalloc(siz)
#define BasicRealloc(buf,siz) XtRealloc(buf,siz)
#define BasicFree(ptr) XtFree(ptr)

#define strdup(strto,strfrom) ( (strto) = XtMalloc(strlen(strfrom)+1), strcpy(strto,strfrom) )
