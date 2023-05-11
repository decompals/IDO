#ifndef __STDARG_H__
#define __STDARG_H__
#ifdef __cplusplus
extern "C" {
#endif
/* Copyright (C) 1987,1989 Silicon Graphics, Inc. All rights reserved.  */
/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

/* #ident "$Revision: 7.7 $" */

/* ANSI C Notes:
 *
 * - THE IDENTIFIERS APPEARING OUTSIDE OF #ifdef __EXTENSIONS__ IN THIS
 *   standard header ARE SPECIFIED BY ANSI!  CONFORMANCE WILL BE ALTERED
 *   IF ANY NEW IDENTIFIERS ARE ADDED TO THIS AREA UNLESS THEY ARE IN ANSI's
 *   RESERVED NAMESPACE. (i.e., unless they are prefixed by __[a-z] or
 *   _[A-Z].  For external objects, identifiers with the prefix _[a-z] 
 *   are also reserved.)
 *
 * - At each call to a function with a variable number of arguments,
 *   either the function definition or prototype must be visible.
 */

#ifndef _VA_LIST_
#define _VA_LIST_
typedef char *va_list;
#endif /* !_VA_LIST_ */

#define va_end(__list)

#if defined(_CFE)
#if defined(__STDC__) && (__STDC__ != 0 )
	/* va_start makes list point past the parmN */
#define va_start(list, parmN) (list = ((char *)&parmN + sizeof(parmN)))
#else
#define va_start(list, name) (void) (list = (void *)((char *)&...))
#endif


/* align p at least to 4-byte alignment, or a if a is larger */
#define _VA_ALIGN(p,a) (((unsigned int)(((char *)p)+((a)>4?(a-1):3))) & ~(unsigned int)((a)>4?(a-1):3))

/*
** "va_stack_arg" is the old MIPS va_arg, which we fall back
** on when we're dealing with arguments on the stack.
*/
#define __va_stack_arg(list,mode)\
(\
	((list)=(char *)_VA_ALIGN(list,__builtin_alignof(mode))+ \
		_VA_ALIGN(sizeof(mode),4)), \
 	(((char *)list) - (_VA_ALIGN(sizeof(mode),4) - sizeof(mode))) \
)

/*
** "_va_double_arg" checks the status in the lower-order 2 bits
** of the "list" pointer, and correctly extracts arguments with
** type double either from the arguements stack, or from the
** floating point argument register spill area.
*/
#define __va_double_arg(list,mode) (\
   (((int)list & 0x1) /* 1 byte aligned? */ \
   ?(list = (char *)((int)list + 7),(char *)((int)list-6-_VA_FP_SAVE_AREA))\
     :(((int)list & 0x2) /* 2 byte aligned? */ \
      ?(list = (char *)((int)list +10),(char *)((int)list-24-_VA_FP_SAVE_AREA))  :__va_stack_arg(list,mode) )))
 	
/*  +++++++++++++++++++++++++++++++++++++++++++
    Because of parameter passing conventions in C:
    use mode=int for char, and short types
    use mode=double for float types
    use a pointer for array types
    +++++++++++++++++++++++++++++++++++++++++++ */
#define _INT 0
#define _FP  1
#define _STRUCT 2

#define va_arg(list,mode) ((mode*)(\
	((__builtin_classof(mode)==_FP &&		     \
	  __builtin_alignof(mode)==sizeof(double)) \
				   ? __va_double_arg(list,mode)\
	 			   : __va_stack_arg(list,mode))))[-1]

#else /* !_CFE */

/* these with with both ANSI and traditional SGI C */
#define va_start(__list, __parmN) (__list = (char *) \
 ( \
  (__builtin_alignof(__parmN) == 8) ? \
   (((unsigned int)&__parmN + sizeof(__parmN) + 8 - 1) & (~0x7) ) : \
   (((unsigned int)&__parmN + ((sizeof(__parmN) > 4)?sizeof(__parmN):4) + 4 - 1) & (~0x3)) \
  ) \
 )

#define va_arg(__list, __mode) ((__mode *)(__list = (char *) \
    ( \
      (__builtin_alignof(__mode) == 8) ? \
       (((unsigned int)__list + sizeof(__mode) + 8 - 1) & (~0x7) ) : \
       (((unsigned int)__list + ((sizeof(__mode) > 4)?sizeof(__mode):4) + 4 - 1) & (~0x3)) \
    ) \
    )) [-1]



#endif /* !_CFE */

#ifdef __cplusplus
}
#endif
#endif /* !__STDARG_H__ */

