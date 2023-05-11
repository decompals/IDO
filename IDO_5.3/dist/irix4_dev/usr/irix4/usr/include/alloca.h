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
/* $Header: /d1/cmplrs.src/v3.12/include/RCS/alloca.h,v 7.4 1992/01/19 23:17:25 daveb Exp $ */
#ifndef __ALLOCA_H
#define __ALLOCA_H

#ifdef __cplusplus
extern "C" {
#endif
/*
** Synopsis
**   #include <alloca.h>
**   void *alloca(integral_types);
**
** Description
**   This header is to be included if the intrinsic version
**   of alloca is desired. The intrinsic version is more
**   efficient in that space is allocated/deallocated off the
**   runtime stack rather than the heap like the libc version does.
**   See also alloca(3).
*/



#if defined(_CFE)
extern char *alloca(int);
#pragma intrinsic(alloca)
#else
/* old compiler, pre-2.20, would not change */
void *alloca(unsigned int);
#define alloca __builtin_alloca
#endif
#ifdef __cplusplus
}
#endif
#endif


