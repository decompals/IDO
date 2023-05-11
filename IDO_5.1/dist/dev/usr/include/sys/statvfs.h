/*	Copyright (c) 1990, 1991 UNIX System Laboratories, Inc.	*/
/*	Copyright (c) 1984, 1986, 1987, 1988, 1989, 1990 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF     	*/
/*	UNIX System Laboratories, Inc.                     	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef _FS_STATVFS_H	/* wrapper symbol for kernel use */
#define _FS_STATVFS_H	/* subject to change without notice */

#ifdef __cplusplus
extern "C" {
#endif

/*#ident	"@(#)uts-3b2:fs/statvfs.h	1.2"*/
#ident	"$Revision: 1.4 $"
/*
 * Structure returned by statvfs(2).
 */

#include <sys/types.h>

#define	FSTYPSZ	16

typedef struct statvfs {
	u_long	f_bsize;	/* fundamental file system block size */
	u_long	f_frsize;	/* fragment size */
	u_long	f_blocks;	/* total # of blocks of f_frsize on fs */
	u_long	f_bfree;	/* total # of free blocks of f_frsize */
	u_long	f_bavail;	/* # of free blocks avail to non-superuser */
	u_long	f_files;	/* total # of file nodes (inodes) */
	u_long	f_ffree;	/* total # of free file nodes */
	u_long	f_favail;	/* # of free nodes avail to non-superuser */
	u_long	f_fsid;		/* file system id (dev for now) */
	char	f_basetype[FSTYPSZ]; /* target fs type name, null-terminated */
	u_long	f_flag;		/* bit-mask of flags */
	u_long	f_namemax;	/* maximum file name length */
	char	f_fstr[32];	/* filesystem-specific string */
	u_long	f_filler[16];	/* reserved for future expansion */
} statvfs_t;

/*
 * Flag definitions.
 */

#define	ST_RDONLY	0x01	/* read-only file system */
#define	ST_NOSUID	0x02	/* does not support setuid/setgid semantics */
#define ST_NOTRUNC	0x04	/* does not truncate long file names */
#define	ST_NODEV	0x20000000	/* disallow opening of device files */
#define	ST_GRPID	0x40000000	/* group-ID assigned from directory */
#define	ST_LOCAL	0x80000000	/* local filesystem, for find */

#ifndef _KERNEL
int statvfs(const char *, struct statvfs *);
int fstatvfs(int, struct statvfs *);
#endif

#ifdef _KERNEL
typedef struct irix5_statvfs {
	app32_ulong_t	f_bsize;
	app32_ulong_t	f_frsize;
	app32_ulong_t	f_blocks;
	app32_ulong_t	f_bfree;
	app32_ulong_t	f_bavail;
	app32_ulong_t	f_files;
	app32_ulong_t	f_ffree;
	app32_ulong_t	f_favail;
	app32_ulong_t	f_fsid;	
	char		f_basetype[FSTYPSZ];
	app32_ulong_t	f_flag;
	app32_ulong_t	f_namemax;
	char		f_fstr[32];
	app32_ulong_t	f_filler[16];
} irix5_statvfs_t;
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _FS_STATVFS_H */
