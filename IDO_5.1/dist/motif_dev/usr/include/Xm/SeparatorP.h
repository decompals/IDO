/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: SeparatorP.h,v $ $Revision: 1.4 $ $Date: 1993/05/13 01:21:57 $ */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _XmSeparatorP_h
#define _XmSeparatorP_h

#include <Xm/Separator.h>
#include <Xm/PrimitiveP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  Separator class structure  */

typedef struct _XmSeparatorClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} XmSeparatorClassPart;


/*  Full class record declaration for Separator class  */

typedef struct _XmSeparatorClassRec
{
   CoreClassPart         core_class;
   XmPrimitiveClassPart  primitive_class;
   XmSeparatorClassPart  separator_class;
} XmSeparatorClassRec;

externalref XmSeparatorClassRec xmSeparatorClassRec;


/*  The Separator instance record  */

typedef struct _XmSeparatorPart
{
   Dimension	  margin;
   unsigned char  orientation;
   unsigned char  separator_type;
   GC             separator_GC;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* __sgi */
} XmSeparatorPart;


/*  Full instance record declaration  */

typedef struct _XmSeparatorRec
{
   CorePart	    core;
   XmPrimitivePart  primitive;
   XmSeparatorPart  separator;
} XmSeparatorRec;


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO


#else


#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmSeparatorP_h */
/* DON'T ADD STUFF AFTER THIS #endif */