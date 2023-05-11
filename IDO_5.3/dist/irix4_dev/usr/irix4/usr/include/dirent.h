#ifndef	__DIRENT__H__
#define	__DIRENT__H__
#ifdef __cplusplus
extern "C" {
#endif
/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident	"$Revision: 1.13 $"

/* DON'T CHANGE MAXNAMLEN ever */
#define MAXNAMLEN	255		/* maximum filename length */
#define DIRBUF		4096		/* buffer size for fs-indep. dirs */


typedef struct
	{
	int	dd_fd;			/* file descriptor */
	int	dd_loc;			/* offset in block */
	int	dd_size;		/* amount of valid data */
	char	*dd_buf;		/* directory block */
	}	DIR;			/* stream data from opendir() */

extern DIR *		opendir(const char *);
extern struct dirent *	readdir(DIR *);
extern int		closedir(DIR *);
#define rewinddir(dirp)	seekdir(dirp, 0L)

#if !defined(_POSIX_SOURCE)
extern long		telldir(DIR *);
extern void		seekdir(DIR *, long);
extern int 		scandir(const char *, struct dirent **[],
				int (*)(struct dirent *),
				int (*)(struct dirent **, struct dirent **));
extern int		alphasort(struct dirent **, struct dirent **);
#endif /* !_POSIX_SOURCE */

#include <sys/dirent.h>

#ifdef __cplusplus
}
#endif
#endif	/* !__DIRENT__H__ */
