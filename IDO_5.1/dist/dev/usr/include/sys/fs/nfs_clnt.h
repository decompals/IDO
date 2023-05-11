#ifndef __SYS_NFS_CLNT_H__
#define __SYS_NFS_CLNT_H__

/**************************************************************************
 *									  *
 * 		 Copyright (C) 1990, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
/*      @(#)nfs_clnt.h	1.2 88/07/15 NFSSRC4.0 from 2.19 88/02/08 SMI      */
/*	Copyright (C) 1988, Sun Microsystems Inc.	*/

#ident "$Revision: 1.14 $"

#include <netinet/in.h>
#include <sys/sema.h>
#include <sys/fstyp.h> /* for FSTYPSZ */

/*
 * NFS arguments to the mount system call.
 */
struct nfs_args {
	struct sockaddr_in	*addr;		/* file server address */
	fhandle_t		*fh;		/* File handle to be mounted */
	int			flags;		/* flags */
	u_int			wsize;		/* write size in bytes */
	u_int			rsize;		/* read size in bytes */
	u_int			timeo;		/* initial timeout in .1 secs */
	u_int			retrans;	/* times to retry send */
	char			*hostname;	/* server's name */
	u_int			acregmin;	/* attr cache file min secs */
	u_int			acregmax;	/* attr cache file max secs */
	u_int			acdirmin;	/* attr cache dir min secs */
	u_int			acdirmax;	/* attr cache dir max secs */
	u_int			symttl;		/* symlink cache time-to-live */
	char			base[FSTYPSZ];	/* base type for statvfs */
	u_int			namemax;	/* name length for statvfs */
};

/*
 * NFS mount option flags
 */
#define	NFSMNT_SOFT	0x001	/* soft mount (hard is default) */
#define	NFSMNT_WSIZE	0x002	/* set write size */
#define	NFSMNT_RSIZE	0x004	/* set read size */
#define	NFSMNT_TIMEO	0x008	/* set initial timeout */
#define	NFSMNT_RETRANS	0x010	/* set number of request retrys */
#define	NFSMNT_HOSTNAME	0x020	/* set hostname for error printf */
#define	NFSMNT_INT	0x040	/* allow interrupts on hard mount */
#define	NFSMNT_NOAC	0x080	/* don't cache attributes */
#define	NFSMNT_ACREGMIN	0x0100	/* set min secs for file attr cache */
#define	NFSMNT_ACREGMAX	0x0200	/* set max secs for file attr cache */
#define	NFSMNT_ACDIRMIN	0x0400	/* set min secs for dir attr cache */
#define	NFSMNT_ACDIRMAX	0x0800	/* set max secs for dir attr cache */
#define	NFSMNT_PRIVATE	0x1000	/* mount private, single-client tree */
#define	NFSMNT_SYMTTL	0x2000	/* set symlink cache time-to-live */
#define	NFSMNT_LOOPBACK	0x4000	/* local mount via loopback interface */
#define	NFSMNT_BASETYPE	0x8000	/* base type for statvfs */
#define	NFSMNT_NAMEMAX	0x10000	/* namemax for statvfs */

/*
 * File identifier struct for loopback (local, user-level) NFS servers.
 */
struct loopfid {
	u_short	lfid_len;	/* bytes in fid, excluding lfid_len */
	u_short	lfid_pad;	/* explicit struct padding */
	u_long	lfid_fno;	/* file number, from nodeid attribute */
};

#ifdef _KERNEL
/*
 * vfs pointer to mount info
 */
#define	vftomi(vfsp)	((struct mntinfo *)((vfsp)->vfs_data))

/*
 * vnode pointer to mount info
 */
#define	vtomi(vp)	((struct mntinfo *)(((vp)->v_vfsp)->vfs_data))

/*
 * NFS vnode to server's block size
 */
#define	vtoblksz(vp)	(vtomi(vp)->mi_bsize)

#endif /* _KERNEL */

#define	HOSTNAMESZ	32
#define	ACREGMIN	3	/* min secs to hold cached file attr */
#define	ACREGMAX	60	/* max secs to hold cached file attr */
#define	ACDIRMIN	30	/* min secs to hold cached dir attr */
#define	ACDIRMAX	60	/* max secs to hold cached dir attr */
#define	SYMTTL		3600	/* max secs to hold cached symlink */

#if defined _KERNEL || defined _KMEMUSER
/*
 * NFS private data per mounted file system
 */
struct mntinfo {
	struct sockaddr_in mi_addr;	/* server's address */
	struct vnode	*mi_rootvp;	/* root vnode */
	u_int		mi_hard : 1;	/* hard or soft mount */
	u_int		mi_printed : 1;	/* not responding message printed */
	u_int		mi_int : 1;	/* interrupts allowed on hard mount */
	u_int		mi_down : 1;	/* server is down */
	u_int		mi_noac : 1;	/* don't cache attributes */
	u_int		mi_private : 1;	/* cache assuming private export */
	u_int		mi_loopback: 1;	/* loopback mount; ok to export */
	u_int		mi_refct;	/* active vnodes for this vfs */
	u_int		mi_tsize;	/* transfer size (bytes) */
	u_int		mi_stsize;	/* server's max transfer size (bytes) */
	u_int		mi_bsize;	/* server's disk block size */
	u_int		mi_mntno;	/* kludge to set client rdev for stat*/
	u_long		mi_rootfsid;	/* remote root's attributed fsid */
	struct fsidmap	*mi_fsidmaps;	/* nohide exports fsid mappings */
	u_int		mi_timeo;	/* inital timeout in 10th sec */
	u_int		mi_retrans;	/* times to retry request */
	char		mi_hostname[HOSTNAMESZ];	/* server's hostname */
	int		mi_acregmin;	/* min secs to hold cached file attr */
	int		mi_acregmax;	/* max secs to hold cached file attr */
	int		mi_acdirmin;	/* min secs to hold cached dir attr */
	int		mi_acdirmax;	/* max secs to hold cached dir attr */
	u_int		mi_symttl;	/* cached symlink time-to-live */
	u_long		mi_rdestroys;	/* count of rnodes destroyed */
	struct rnode	*mi_rnodes;	/* list of active and cached rnodes */
	sema_t		mi_lock;	/* mutual exclusion */
	char		mi_basetype[FSTYPSZ];	/* base type for statvfs */
	u_int		mi_namemax;	/* namemax for statvfs */
};

struct fsidmap {
	u_long		fsid_remote;	/* remote attributed fsid */
	long		fsid_local;	/* local, client-unique fsid */
	struct fsidmap	*fsid_next;	/* other mappings */
};
#endif /* _KERNEL || _KMEMUSER */

#ifdef _KERNEL

/*
 * enum to specifiy cache flushing action when file data is stale
 */
enum staleflush {NOFLUSH, SFLUSH};

/*
 * Locking macros for SP and MP mntinfo locking.
 */
#define milock(mi)	psema(&(mi)->mi_lock, PINOD)
#define miunlock(mi)	vsema(&(mi)->mi_lock)
#define milock_mp(mi)	appsema(&(mi)->mi_lock, PINOD)
#define miunlock_mp(mi)	apvsema(&(mi)->mi_lock)

/*
 * NFS minor device number allocation bitmap.
 */
#define MINORMAPSIZE	(256/NBBY)

struct minormap {
	u_char	vec[MINORMAPSIZE];
	lock_t	lock;
};

extern struct minormap	nfs_minormap;
extern dev_t		nfs_major;
#define is_nfs_dev(x)	(getemajor(x) == nfs_major)
#define NFS_SWAPFILE	"/swap/_swap"

extern int		vfs_getnum(struct minormap *);
extern void		vfs_putnum(struct minormap *, int);

struct cred;
struct nfsfattr;
struct vattr;
struct vnode;

/*
 * Attribute cache operations exported from nfs_vnodeops.c to other NFS
 * client-side modules.
 */
extern struct vnode	*specvp(struct vnode *, dev_t, vtype_t, struct cred *);
extern mode_t	setdirmode(struct vnode *, mode_t);
extern gid_t	setdirgid(struct vnode *);
extern dev_t	expdev(dev_t dev);
extern dev_t	nfs_cmpdev(dev_t dev);
extern dev_t	nfs_expdev(dev_t dev);
extern void	nfs_attrcache(struct vnode *, struct nfsfattr *,
			enum staleflush);
extern void	setdiropargs(struct nfsdiropargs *, char *, struct vnode *);
extern void	pflushinvalvp(struct vnode *, off_t, off_t);
extern void	nfs_rwunlock(struct vnode *);
extern void	nfs_rwlock(struct vnode *);
extern void	pdflush(vnode_t *, int);
extern void	initrnodes(void);
extern void	vrelvm(void);
extern char	*inet_ntoa(struct in_addr);
extern char	*newname(void);
extern int	nfs_getattr(struct vnode *, struct vattr *, int, struct cred *);
extern int	makenfsroot(struct vfs *, struct sockaddr_in *, fhandle_t *,
			char *, struct vnode **, int);
extern int	nfs_lockctl(struct vnode *, struct flock *,
			int, struct cred *, int);
extern int	rfscall(struct mntinfo *, int, xdrproc_t, caddr_t,
			xdrproc_t, caddr_t, struct cred *);
extern int	nattr_to_iattr(struct nfsfattr *, struct vnode *);
extern int	vattr_to_sattr(struct vattr *, struct nfssattr *);
extern int	nattr_to_vattr(struct vnode *, struct nfsfattr *,
			struct vattr *);
extern int	do_bio(struct buf *, struct rnode *);
extern int	pflushvp(struct vnode *, off_t, int);
extern int	nfs_rwlock_nowait(struct rnode *);
extern int	nfs_rmount(struct vfs *, enum whymountroot);

#endif	/* _KERNEL */
#endif /* !__SYS_NFS_CLNT_H__ */
