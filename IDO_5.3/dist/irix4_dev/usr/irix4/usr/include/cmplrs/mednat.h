/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: /d1/cmplrs.src/v3.12/include/cmplrs/RCS/mednat.h,v 1.3 1992/05/26 14:36:34 daveb Exp $ */
#ifndef _CMPLRS_MEDNAT_H
#define _CMPLRS_MEDNAT_H

#ifdef _LANGUAGE_PASCAL

/*#define MEDNAT	1	/* define MEDNAT flag		*/

#ifdef _MEDNAT

#define		flush(err);		;
#define		flush(stderr);		;
#define		flush(output);		;
#define		flush(dumpfile);	;

#define		maxint		maxlong

#define		lshift(_a,_b)	((_a) << (_b))
#define		rshift(_a,_b)	((_a) >> (_b))

type
    integer = int32;
    double = longreal;
    pointer = extaddr;
    cardinal = card32;

function  max(a,b : integer) : integer; external;
function  min(a,b : integer) : integer; external;
function  argc : integer; external;
procedure argv( i : integer; s : fstring); external;

#define bitand(_a,_b) ((_a) And (_b))
#define bitxor(_a,_b) ((_a) Xor (_b))
#define bitor(_a,_b) ((_a) Or (_b))
#define bitnot(_a) (Not (_a))

#else

#define		stderr			err

#endif

#endif

#endif /* _LANGUAGE_PASCAL */
