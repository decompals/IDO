/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: mainE.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:57 $ */

#if ( defined main_h )
#define extern 
#endif

#ifdef _NO_PROTO
extern void SetSensitive();
extern void ViewError();
extern void ViewWarning();
extern XmString FetchString();
#else
extern void SetSensitive(Widget cascade, String item, Boolean sensitive);

extern void ViewError(ViewPtr this, XmString s1, XmString s2);

extern void ViewWarning(ViewPtr this, XmString s1, XmString s2);

extern XmString FetchString(ViewPtr this, String name);

#endif

#if ( defined main_h )
#define extern 
#endif
