#ifndef __FCNTL_H__
#define __FCNTL_H__
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

#ident "$Revision: 1.10 $"

#include <sys/types.h>
#include <sys/fcntl.h>

#ifdef __cplusplus
#ifndef ____OPEN_P_
#define ____OPEN_P_
extern int	open(const char *, int, int=0644);
#endif
#else
extern int	open(const char *, int, ...);
#endif

extern int	creat(const char *, mode_t);
extern int	fcntl(int, int, ...);

#ifdef __cplusplus
}
#endif
#endif	/* !__FCNTL_H__ */
