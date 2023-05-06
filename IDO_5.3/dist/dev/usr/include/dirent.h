#ifndef __DIRENT_H__
#define __DIRENT_H__
#ifdef __cplusplus
extern "C" {
#endif
#ident "$Revision: 1.18 $"
/*
*
* Copyright 1992, Silicon Graphics, Inc.
* All Rights Reserved.
*
* This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
* the contents of this file may not be disclosed to third parties, copied or
* duplicated in any form, in whole or in part, without the prior written
* permission of Silicon Graphics, Inc.
*
* RESTRICTED RIGHTS LEGEND:
* Use, duplication or disclosure by the Government is subject to restrictions
* as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
* and Computer Software clause at DFARS 252.227-7013, and/or in similar or
* successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
* rights reserved under the Copyright Laws of the United States.
*/
/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#if !defined(_POSIX_SOURCE) 
#define MAXNAMLEN	255		/* maximum filename length */
#define DIRBUF		4096		/* buffer size for fs-indep. dirs */
#endif /* !defined(_POSIX_SOURCE) */ 

#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif

typedef struct
	{
	int		dd_fd;		/* file descriptor */
	int		dd_loc;		/* offset in block */
	int		dd_size;	/* amount of valid data */
	char		*dd_buf;	/* directory block */
	}	DIR;			/* stream data from opendir() */

#if defined(_MODERN_C)

extern DIR		*opendir( const char * );
extern struct dirent	*readdir( DIR * );
#if !defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE)
extern long		telldir( DIR * );
extern void		seekdir( DIR *, long );
#endif /* !defined(_POSIX_SOURCE) */ 
#if defined(_SGI_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE)
extern int 		scandir(const char *, struct dirent **[],
				int (*)(struct dirent *),
				int (*)(struct dirent **, struct dirent **));
extern int		alphasort(struct dirent **, struct dirent **);
#endif /* defined(_SGI_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE) */
extern void		rewinddir( DIR * );
extern int		closedir( DIR * );

#if (defined(_SGI_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE)) || defined(_SGI_REENTRANT_FUNCTIONS)
extern struct dirent	*readdir_r(DIR *, struct dirent *);
#endif

#else	/* !_MODERN_C */

extern DIR		*opendir();
extern struct dirent	*readdir();
#if !defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE)
extern long		telldir();
extern void		seekdir();
#endif /* !defined(_POSIX_SOURCE) */ 
#if defined(_SGI_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE)
extern int 		scandir();
extern int		alphasort();
#endif /* defined(_SGI_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE) */
extern void		rewinddir();
extern int		closedir();

#endif

#if !defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE)
#define rewinddir( dirp )	seekdir( dirp, 0L )
#endif

#include <sys/dirent.h>

#ifdef __cplusplus
}
#endif
#endif /* !__DIRENT_H__ */
