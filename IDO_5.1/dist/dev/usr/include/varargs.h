#ifndef __VARARGS_H__
#define __VARARGS_H__
#ifdef __cplusplus
extern "C" {
#endif
/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident "$Revision: 7.7 $"

/* ANSI C Notes:
 *
 *  This non-standard header file has identifiers that collide with the
 *  ones in <stdarg.h>.
 *
 *  gb - went back to rev. 1.10 of varargs.h for MIPS 3.00 release, as
 *  	 we have implemented __builtin_alignof in all current revisions
 *	 (which bypasses the need for <stamp.h>),
 *	 and our version is easier to read.
 *	 -- minor changes for __lint, etc.
 */

#ifndef _VA_LIST_
#define _VA_LIST_
typedef char *va_list;
#endif /* !_VA_LIST_ */

#define va_dcl 	int va_alist;
#define va_end(__list)

#ifndef __lint
#ifdef _CFE

/*
** the compiler built-in function _VA_INIT_STATE():
**	-returns 1 if the va_alist marker is the first
**		parameter in the parameter list, or
**	-returns 2 if the va_alist marker is the second
**		parameter in the parameter list, and the
**		first parameter has type double, or
**	-returns 0 otherwise.
**
** the compiler built-in function __builtin_classof(type):
**	-returns 0 if type is integer or pointer
**	-returns 1 if floating point
**	-returns 2 if aggregate
*/ 
#define va_start(list) list = (char *) &va_alist - _VA_INIT_STATE

/* align p at least to 4-byte alignment, or a if a is larger */
#define _VA_ALIGN(p,a) (((unsigned int)((p)+((a)>4?(a-1):3))) & ~(unsigned int)((a)>4?(a-1):3))

/*
** "va_stack_arg" is the old MIPS va_arg, which we fall back
** on when we're dealing with arguments on the stack.
*/
#define __va_stack_arg(list,mode)\
(\
	((list)=(char *)_VA_ALIGN(list,__builtin_alignof(mode))+ \
		_VA_ALIGN(sizeof(mode),4)), \
 	((list) - (_VA_ALIGN(sizeof(mode),4) - sizeof(mode))) \
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

#define va_start(__list) __list = (char *) &va_alist
#define va_arg(__list, __mode) ((__mode *)(__list = (char *) \
    ( \
      (__builtin_alignof(__mode) == 8) ? \
       (((unsigned int)__list + sizeof(__mode) + 8 - 1) & (~0x7) ) : \
       (((unsigned int)__list + ((sizeof(__mode) > 4)?sizeof(__mode):4) + 4 - 1) & (~0x3)) \
    ) \
    )) [-1]


#endif /* !_CFE */
#else /* __lint */
#define va_start(__list) __list = (char *) &va_alist
#define va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]
#endif

#ifdef __cplusplus
}
#endif
#endif /* !__VARARGS_H__ */





