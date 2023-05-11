#ifndef __CTYPE_H__
#define __CTYPE_H__

#ifdef __cplusplus
extern "C" {
#endif

/**************************************************************************
 *									  *
 * 		 Copyright (C) 1984,1989 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 **************************************************************************/
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* #ident "$Revision: 1.16 $" */

/* ANSI C Notes:
 *
 * - THE IDENTIFIERS APPEARING OUTSIDE OF #ifdef __EXTENSIONS__ IN THIS
 *   standard header ARE SPECIFIED BY ANSI!  CONFORMANCE WILL BE ALTERED
 *   IF ANY NEW IDENTIFIERS ARE ADDED TO THIS AREA UNLESS THEY ARE IN ANSI's
 *   RESERVED NAMESPACE. (i.e., unless they are prefixed by __[a-z] or
 *   _[A-Z].  For external objects, identifiers with the prefix _[a-z] 
 *   are also reserved.)
 *
 * - In addition, function names which begin with 'to[a-z]' or 'is[a-z]'
 *   are reserved when including ctype.h (Section 4.13.2).
 *
 * - A prototype for each function required in <ctype.h> must be declared
 *   prior to the macro definition, so that an #undef will expose it.
 *
 */

#define	_U	01	/* Upper case */
#define	_L	02	/* Lower case */
#define	_N	04	/* Numeral (digit) */
#define	_S	010	/* Spacing character */
#define	_P	020	/* Punctuation */
#define	_C	040	/* Control character */
#define	_B	0100	/* Blank */
#define	_X	0200	/* heXadecimal digit */

extern int 	isalnum(int);
extern int 	isalpha(int);
extern int	iscntrl(int);
extern int 	isdigit(int);
extern int 	isgraph(int);
extern int	islower(int);
extern int	isprint(int);
extern int	ispunct(int);
extern int	isspace(int);
extern int	isupper(int);
extern int	isxdigit(int);

#ifndef _LINT
#if IP17 && PROM
extern int	_ctype[];
#else
extern char	_ctype[];
#endif

#define	isalpha(c)	((_ctype + 1)[c] & (_U | _L))
#define	isupper(c)	((_ctype + 1)[c] & _U)
#define	islower(c)	((_ctype + 1)[c] & _L)
#define	isdigit(c)	((_ctype + 1)[c] & _N)
#define	isxdigit(c)	((_ctype + 1)[c] & _X)
#define	isalnum(c)	((_ctype + 1)[c] & (_U | _L | _N))
#define	isspace(c)	((_ctype + 1)[c] & _S)
#define	ispunct(c)	((_ctype + 1)[c] & _P)
#define	isprint(c)	((_ctype + 1)[c] & (_P | _U | _L | _N | _B))
#define	isgraph(c)	((_ctype + 1)[c] & (_P | _U | _L | _N))
#define	iscntrl(c)	((_ctype + 1)[c] & _C)
#define	isascii(c)	(!((c) & ~0177))
#define	_toupper(c)     ((_ctype + 258)[c])
#define	_tolower(c)	((_ctype + 258)[c])
#define	toascii(c)	((c) & 0177)

#endif /* !_LINT */

extern int toupper (int);
extern int tolower (int);

#ifdef __cplusplus
}
#endif

#endif /* !__CTYPE_H__ */
