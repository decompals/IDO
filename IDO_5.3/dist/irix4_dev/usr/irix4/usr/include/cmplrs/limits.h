#ifndef __LIMITS_H__
#define __LIMITS_H__
/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/
/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

/* #ident	"$Revision: 6.14 $" */

/* ANSI C Notes:
 *
 * - THE IDENTIFIERS APPEARING OUTSIDE OF #ifdef __EXTENSIONS__ IN THIS
 *   standard header ARE SPECIFIED BY ANSI!  CONFORMANCE WILL BE ALTERED
 *   IF ANY NEW IDENTIFIERS ARE ADDED TO THIS AREA UNLESS THEY ARE IN ANSI's
 *   RESERVED NAMESPACE. (i.e., unless they are prefixed by __[a-z] or
 *   _[A-Z].  For external objects, identifiers with the prefix _[a-z] 
 *   are also reserved.)
 *
 */

#define	CHAR_BIT	8		/* # of bits in a "char" */
#define	CHAR_MAX	UCHAR_MAX	/* max integer value of a "char" */
#define	CHAR_MIN	0		/* min integer value of a "char" */
#define SCHAR_MAX	127
#define SCHAR_MIN	(-128)
#define UCHAR_MAX	255
#define	SHRT_MAX	32767		/* max decimal value of a "short" */
#define	SHRT_MIN	(-32768)	/* min decimal value of a "short" */
#define USHRT_MAX	65535
#define	INT_MAX		2147483647	/* max decimal value of an "int" */
#define	INT_MIN		(-2147483647-1)	/* min decimal value of an "int" */
#define UINT_MAX	4294967295
#define	LONG_MAX	2147483647	/* max decimal value of a "long" */
#define	LONG_MIN	(-2147483647-1)	/* min decimal value of a "long" */
#define ULONG_MAX	4294967295
#define MB_LEN_MAX	1	/* Max # of bytes in a multibyte character */

/*
 * First comes the portion of non-ANSI which _POSIX_SOURCE needs
 */
#if (defined(__EXTENSIONS__) || defined(_POSIX_SOURCE))
/*
 * POSIX conformance: ARG_MAX, CHILD_MAX and OPEN_MAX are not
 * defined here since they are configurable and available from sysconf.
 */
#define NGROUPS_MAX	16		/* max # of groups per user */
#define	LINK_MAX	30000		/* max # of links to a single file */
#define	NAME_MAX	255		/* max # of characters in a file name */
#define	PATH_MAX	1024		/* max # of characters in a path name */
#define	PID_MAX		30000		/* max value for a process ID */
#define	PIPE_BUF	10240		/* max # bytes atomic in pipe write */
#define	PIPE_MAX	10240		/* max # bytes written to pipe in wrt */
#define	UID_MAX		60000		/* max value for a user or group ID */

#ifndef CLK_TCK
#define	CLK_TCK		100		/* # of clock ticks per second */
#endif
#define	WORD_BIT	32		/* # of bits in a "word" or "int" */

/* Table of POSIX mimimum values */
#define _POSIX_ARG_MAX		4096	/* bytes in args to exec() */
#define _POSIX_CHILD_MAX	6	/* processes per user id */
#define _POSIX_LINK_MAX		8	/* max # links per file */
#define _POSIX_MAX_CANON	255	/* bytes in term canon. input queue */
#define _POSIX_MAX_INPUT	255	/* bytes of avail space in term queue */
#define _POSIX_NAME_MAX		14	/* bytes in a filename */
#define _POSIX_NGROUPS_MAX	0	/* simultaneous supplementary gids */
#define _POSIX_OPEN_MAX		16	/* open files per process */
#define _POSIX_PATH_MAX		255	/* bytes in pathname */
#define _POSIX_PIPE_BUF		512	/* bytes written atomically to pipe */

#define MAX_CANON	_POSIX_MAX_CANON
#define MAX_INPUT	_POSIX_MAX_INPUT
#endif /* __EXTENSIONS__ || _POSIX_SOURCE */


/*
 * Then the non-ANSI part that must be excluded when _POSIX_SOURCE is defined
 */
#if (defined(__EXTENSIONS__) && !defined(_POSIX_SOURCE))

#define	ARG_MAX		5120 		/* max length of arguments to exec 
#define	CHILD_MAX	25		/* max # of processes per user id */
#define	OPEN_MAX	20		/* max # of files proc can have open */

#define	DBL_DIG		15		/* digits of precision of a "double" */
#define	DBL_MAX		1.7976931348623157e+308 /* max decimal val of "double" */
#define	DBL_MIN		2.2250738585072014e-308 /* min decimal val of "double" */
#define	FCHR_MAX	2147483647	/* max size of a file in bytes */
#define	FLT_DIG		6		/* digits of precision of a "float" */
#ifdef __STDC__
#define FLT_MAX		3.40282347e+38F
#else
#define FLT_MAX		3.40282347e+38
#endif
#ifdef __STDC__
#define FLT_MIN		1.17549435e-38F
#else
#define FLT_MIN		1.17549435e-38
#endif

#ifndef HUGE_VAL 	/* also in math.h */
#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
/* __infinity is a double-precision variable in libc set to infinity */
extern const double __infinity;
#define HUGE_VAL __infinity	
#endif /* _LANGUAGE_C || _LANGUAGE_C_PLUS_PLUS */
#endif /* !HUGE_VAL */

#define	PASS_MAX	8		/* max # of characters in a password */
#define	STD_BLK		1024		/* # bytes in a physical I/O block */
#define	SYS_NMLN	9		/* # of chars in uname-retned strngs */
#define	USI_MAX		4294967295	/* max decimal value of an "unsigned" */

#endif /* __EXTENSIONS__ && !_POSIX_SOURCE */

#endif /* __LIMITS_H__ */
