#ifndef __MNTENT_H__
#define __MNTENT_H__
#ident "$Revision: 1.22 $"
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
/*	@(#)mntent.h	1.3 88/05/20 4.0NFSSRC SMI;	from SMI 1.13 99/01/06	*/

/*
 * File system table, see mntent (5)
 *
 * Used by dump, mount, umount, swap, fsck, df, ...
 *
 * Quota files are always named "quotas", so if type is "rq",
 * then use concatenation of mnt_dir and "quotas" to locate
 * quota file.
 */

#define	MNTTAB		"/etc/fstab"
#define	MOUNTED		"/etc/mtab"

#define	MNTMAXSTR	128

#include <sys/fsid.h>

#define MNTTYPE_EFS	FSID_EFS	/* extent filesystem */
#define MNTTYPE_NFS	FSID_NFS	/* network file system */
#define MNTTYPE_SOCKET	FSID_SOCKET	/* socket pseudo-filesystem */
#define MNTTYPE_DBG	FSID_DBG	/* debug (proc) pseudo-filesystem */
#define MNTTYPE_PROC	FSID_PROCFS	/* proc file system */
#define MNTTYPE_PIPE	FSID_COM	/* pipe pseudo-filesystem */
#define MNTTYPE_MFS	"mfs"	 	/* mfs filesystem - ClearCase */
#define	MNTTYPE_PC	"pc"		/* IBM PC (MSDOS) file system */
#define	MNTTYPE_SWAP	"swap"		/* swap file system */
#define	MNTTYPE_IGNORE	"ignore" 	/* No type specified; ignore entry */
#define MNTTYPE_LO      "lo"    	/* Loop back File system */
#define	MNTTYPE_DOS	"dos"		/* MS-DOS file system (SGI) */
#define MNTTYPE_FD	FSID_FD		/* /dev/fd file system */
#define MNTTYPE_AFS	"afs"		/* Andrew File System */

#define	MNTOPT_RO	"ro"		/* read only */
#define	MNTOPT_RW	"rw"		/* read/write */
#define	MNTOPT_QUOTA	"quota"		/* quotas */
#define	MNTOPT_NOQUOTA	"noquota"	/* no quotas */
#define	MNTOPT_NOSUID	"nosuid"	/* disallow setuid program execution */
#define MNTOPT_NODEV	"nodev"		/* disallow access to device files */
#define	MNTOPT_NOAUTO	"noauto"	/* hide entry from mount -a */
#define MNTOPT_GRPID 	"grpid"		/* SysV-compatible group-id on create */
#define MNTOPT_REMOUNT	"remount"	/* change options on previous mount */
#define MNTOPT_RAW	"raw"		/* raw device name */
#define MNTOPT_FSCK	"fsck"		/* fsck by default */
#define MNTOPT_NOFSCK	"nofsck"	/* do not fsck */
#define	MNTOPT_SOFT	"soft"		/* soft mount */
#define	MNTOPT_HARD	"hard"		/* hard mount */
#define	MNTOPT_INTR	"intr"		/* allow interrupts on hard mount */
#define	MNTOPT_NOAC	"noac"		/* don't cache nfs attributes */
#define MNTOPT_PORT	"port"		/* server IP port number */
#define MNTOPT_RETRANS	"retrans"	/* set number of request retries */
#define MNTOPT_RSIZE	"rsize"		/* set read size (bytes) */
#define MNTOPT_WSIZE	"wsize"		/* set write size (bytes) */
#define MNTOPT_TIMEO	"timeo"		/* set initial timeout (1/10 sec) */
#define MNTOPT_ACTIMEO	"actimeo"	/* attr cache timeout (sec) */
#define MNTOPT_ACREGMIN	"acregmin"	/* min ac timeout for reg files (sec) */
#define MNTOPT_ACREGMAX	"acregmax"	/* max ac timeout for reg files (sec) */
#define MNTOPT_ACDIRMIN	"acdirmin"	/* min ac timeout for dirs (sec) */
#define MNTOPT_ACDIRMAX	"acdirmax"	/* max ac timeout for dirs (sec) */
#define MNTOPT_PRIVATE	"private"	/* mount nfs single-client tree */
#define MNTOPT_SYMTTL	"symttl"	/* symlink cache time-to-live */
#define MNTOPT_SWPLO	"swplo"		/* first block(512 byte) to use */
#define MNTOPT_LENGTH	"length"	/* # 512 byte blocks */
#define MNTOPT_MAXLENGTH "maxlength"	/* max # 512 byte blocks to grow to */
#define MNTOPT_VLENGTH	"vlength"	/* virtual # 512 byte blocks */
#define MNTOPT_PRI	"pri"		/* priority of device */


#define MNTINFO_DEV     "dev"   /* device number of the mounted file system */

struct mntent {
	char	*mnt_fsname;		/* name of mounted file system */
	char	*mnt_dir;		/* file system path prefix */
	char	*mnt_type;		/* MNTTYPE_* */
	char	*mnt_opts;		/* MNTOPT* */
	int	mnt_freq;		/* dump frequency, in days */
	int	mnt_passno;		/* pass number on parallel fsck */
};

#ifdef __cplusplus
extern "C" {
#endif

struct __file_s	*setmntent(const char *, const char *);
struct mntent	*getmntent(struct __file_s *);
int		addmntent(struct __file_s *, struct mntent *);
char	*hasmntopt(struct mntent *, const char *);
int		endmntent(struct __file_s *);

#ifdef __cplusplus
}
#endif
#endif /* !__MNTENT_H__ */
