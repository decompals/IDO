#ifndef _KMEM_H
#define _KMEM_H
#ifdef __cplusplus
extern "C" {
#endif
/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Avenue                               |
 * |         Sunnyvale, California 94088-3650, USA             |
 * |-----------------------------------------------------------|
 */
/* $Header: /proj/irix5.3/isms/cmplrs/commonlib/include/RCS/kmem.h,v 1.11 1994/02/10 09:56:05 bhaskar Exp $ */

/*
	This header file contains basic definitions and declarations
	for libkmem. 
*/

/* 
   FIX: Basic types. Usage should be consistent with other
   os parts
*/
typedef	int 			Kmem_int;
typedef	unsigned int 		Kmem_uint;
#if (_MIPS_SZLONG == 64)
typedef long			Kmem_int64;
typedef unsigned long		Kmem_uint64;
#else /* 32-bit */
typedef long long		Kmem_int64;
typedef unsigned long long 	Kmem_uint64;
#endif

typedef struct _kmem kmem_t;

typedef Kmem_uint64 *pcb_regset;

kmem_t *kmem_open(
	const char*/* executable */, 
	const char*/* corefile */, 
	const char*/* swapfile */,
	Kmem_int   /* flag, O_RDONLY, O_RDWR */,
	const char*/* errstr, appended to error messages */
	);

Kmem_int kmem_setpid(
	kmem_t*	   /* kernel */,
	pid_t	   /* pid to set to */
	);

Kmem_int kmem_getpid(kmem_t * /*kernel*/);

/* 
   Note: Callback functions returning negative numbers are 
	 treated as errors
*/

/*
   Call back function 1: Given symbol name, return address
*/
typedef Kmem_int (*kmem_sym_addr_func) (
	void*	   /* pointer to be returned */,
	char*	   /* name of symbol */,
	Kmem_uint64*	/* address of symbol */
	);
/*
   Call back function 2: Given struct name, return length 
*/
typedef Kmem_int (*kmem_struct_len_func) (
	void*	   /* pointer to be returned */,
	char*	   /* name of struct */,
	Kmem_int*  /* length of struct */
	);

/*
   Call back function 3: Given struct name and member name, 
   return offset of member in struct 
*/
typedef Kmem_int (*kmem_member_offset_func) (
	void*	   /* pointer to be returned */,
	char*	   /* name of struct */,
	char*	   /* name of member */,
	Kmem_int*  /* offset of member */
	);

/*
   Call back function 4: Given struct name and member name, 
   return length of member in bits 
*/
typedef Kmem_int (*kmem_member_bitlen_func) (
	void*	   /* pointer to be returned */,
	char*	   /* name of struct */,
	char*	   /* name of member */,
	Kmem_int*  /* length of member in bits */
	);

Kmem_int kmem_add_callbacks(
	kmem_t*		    	
		/* kernel */,
	void*		    	
		/* pointer to be passed back */,
	kmem_sym_addr_func  	
		/* sym_addr, given symbol, return address */,
	kmem_struct_len_func	
		/* struct_len, gives length of structure */,
	kmem_member_offset_func 
		/* member_offset, gives offset of struct member */,
	kmem_member_bitlen_func 
		/* member_bitlen, length of struct member in bits */
	);

Kmem_int kmem_close(kmem_t *);

Kmem_int64
kmem_read(
	kmem_t*		/* kernel */, 
	Kmem_uint64 	/* vaddr, virtual address */, 
	char*		/* buf, buffer for read */,
	Kmem_uint64 	/* nbytes, no. of bytes to be read */
	);

Kmem_int64
kmem_write(
	kmem_t*		/* kernel */, 
	Kmem_uint64 	/* vaddr */, 
	char*		/* buf */, 
	Kmem_uint64 	/* nbytes */
	);

Kmem_int
kmem_getregs(
	kmem_t*		/* kernel */,
	pcb_regset*	/* register set */
	);

#ifdef __cplusplus
}
#endif
#endif
