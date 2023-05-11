#ifndef __FTW_H__
#define __FTW_H__
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

#ident "$Revision: 1.8 $"

#include <sys/stat.h>

/*
 *	Codes for the third argument to the user-supplied function
 *	which is passed as the second argument to ftw(3C).
 */

#define	FTW_F	0	/* file */
#define	FTW_D	1	/* directory */
#define	FTW_DNR	2	/* directory without read permission */
#define	FTW_NS	3	/* unknown type, stat failed */

extern int ftw(const char *, int (*) (const char *, struct stat *, int), int);

#ifdef __cplusplus
}
#endif
#endif /* !__FTW_H__ */

