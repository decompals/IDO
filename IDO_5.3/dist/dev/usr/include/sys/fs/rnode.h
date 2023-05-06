/*      @(#)rnode.h	1.4 88/07/15 NFSSRC4.0 from 1.21 88/02/08 SMI      */
/*	Copyright (C) 1988, Sun Microsystems Inc. */

#ifndef __NFS_RNODE_H__
#define __NFS_RNODE_H__

#define NFS_FHANDLE_LEN 64

typedef struct nfs_fhandle {
	int fh_len;
	char fh_buf[NFS_FHANDLE_LEN];
} nfs_fhandle;


typedef struct symlink_cache {
	char *contents;         /* contents of the symbolic link */
	int len;                /* length of the contents */
	int size;               /* size of the allocated buffer */
} symlink_cache;

struct access_cache;
struct rddir_cache;
/*
 * Remote file information structure.
 * The rnode is the "inode" for remote files.  It contains all the
 * information necessary to handle remote file on the client side.
 */
struct rnode {
	struct rnode	*r_next;	/* active rnode list */
	struct rnode	**r_prevp;	/* back-pointer in active list */
	struct rnode	*r_mnext;	/* next rnode in mntinfo list */
	struct rnode	**r_mprevp;	/* and previous rnode's &r_mnext */
	struct vnode	*r_vnode;	/* associated vnode */
	union {
	    fhandle_t	r_fh2;		/* file handle */
	    nfs_fhandle	r_fh3;
	} r_ufh;
	u_short		r_flags;	/* flags, see below */
	short		r_error;	/* async write error */
	daddr_t		r_lastr;	/* last block read (read-ahead) */
	long		r_mapcnt;	/* total count of mapped pages */
	lock_t		r_flaglock;	/* to protect r_flags and r_iocount */
	struct cred	*r_cred;	/* current credentials */
	union {
	    struct {
		struct cred	*ruu_unlcred;	/* unlinked credentials */
		char		*ruu_unlname;	/* unlinked regular file name */
		struct vnode	*ruu_unldvp;	/* unlinked file's parent dir */
	    } ru_unl;
	    struct {
		time_t		rus_symtime;	/* symlink expiration time */
		u_int		rus_symlen;	/* symlink value length */
		char		*rus_symval;	/* cached symlink contents */
	    } ru_sym;
	} r_u;
	scoff_t		r_size;		/* client's idea of file size */
	scoff_t		r_localsize;	/* client's highest local mapping */
	struct nfsfattr	r_nfsattr;	/* cached nfs attributes */
	struct timeval	r_nfsattrtime;	/* time attributes become invalid */
	int		r_iocount;	/* io operations in progress */
	sema_t		r_iowait;	/* io synchronization semaphore */
#ifdef DEBUG
	short		r_lockpid;	/* pid of process owning r_lock */
#endif
	short		r_rwlockpid;	/* pid of process owning r_rwlock */
	short		r_locktrips;	/* number of r_rwlock reacquisitions */
	sema_t		r_rwlock;	/* vop_rwlock for nfs */
	sema_t		r_lock;		/* semaphore guarding attributes */
	sema_t		r_statelock;	/* protects (most of) rnode contents */
	struct access_cache *r_acc;	/* cache of access results */
	struct rddir_cache *r_dir;	/* cache of readdir responses */
	struct rddir_cache *r_direof;	/* pointer to the EOF entry */
	lloff_t		r_cookie;	/* cookie of last entry in directory */
	mrlock_t        r_3rwlock;      /* read-write lock for NFS3 */
	symlink_cache	r_symlink;	/* cached readlink response */
	cnt_t		r_iocnt;	/* # of outstanding async write ops */
	writeverf3	r_verf;		/* version 3 write verifier */
};
typedef struct rnode	rnode_t;
#define r_fh	r_ufh.r_fh2

#define r_unlcred	r_u.ru_unl.ruu_unlcred
#define r_unlname	r_u.ru_unl.ruu_unlname
#define r_unldvp	r_u.ru_unl.ruu_unldvp
#define r_symval	r_u.ru_sym.rus_symval
#define r_symlen	r_u.ru_sym.rus_symlen
#define r_symtime	r_u.ru_sym.rus_symtime

/*
 * Flags
 */
#define RLOCKED		0x0001		/* rnode is in use */
#define RWANT		0x0002		/* someone wants a wakeup */
#define RATTRVALID	0x0004		/* Attributes in the rnode are valid */
#define REOF		0x0008		/* EOF encountered on read */
#define RDIRTY		0x0010		/* dirty buffers may be in buf cache */
#define RNOCACHE	0x0020		/* don't cache read and write blocks */
#define RIOWAIT		0x0040		/* rnode waiting for io completion */
#define RGONE		0x0080		/* rnode really isn't here */
#define RVNODEWAIT	0x0100		/* waiting for iget in makenfsnode */
#define RTEXT		0x0200		/* rnode mapped for execution */

/*
 * Maximum allowed file offset.
 */
#define	NFS_MAX_FILE_OFFSET	0x7fffffff

/*
 * Convert between vnode and rnode
 */
#define rtov(rp)	((rp)->r_vnode)
#define vtor(vp)	((struct rnode *)((vp)->v_data))
#define vtofh(vp)	(&(vtor(vp)->r_fh))
#define rtofh(rp)	(&(rp)->r_fh)

#ifdef _KERNEL
#define vtorfsid(vp)	(vtor(vp)->r_nfsattr.na_fsid)
#define vtornodeid(vp)	(vtor(vp)->r_nfsattr.na_nodeid)

/*
 * Lock and unlock rnodes.
 */
#define rnode_is_locked(rp)	(valusema(&(rp)->r_lock) <= 0)
#define vnode_is_locked(rp)	(valusema(&(rp)->r_rwlock) <= 0)

extern void	rlock(struct rnode *);
extern int	rlock_nowait(struct rnode *);
extern void	runlock(struct rnode *);
extern void	initrnodes();
extern void	rdestroy(struct rnode *, struct mntinfo *);
extern int	rflush(struct mntinfo *);
extern int	makenfsnode(struct vfs *, fhandle_t *, struct nfsfattr *,
			    struct vnode **);

/*
 * Set and clear rnode flags.
 */
#define rsetflag(rp, f)		{ int _s = splock((rp)->r_flaglock); \
				  (rp)->r_flags |= (f); \
				  spunlock((rp)->r_flaglock, _s); }
#define rclrflag(rp, f)		{ int _s = splock((rp)->r_flaglock); \
				  (rp)->r_flags &= ~(f); \
				  spunlock((rp)->r_flaglock, _s); }

#define nfsattr_inval(vp)	(vtor(vp)->r_nfsattrtime.tv_sec = 0)


#endif /* _KERNEL */
#endif /* __NFS_RNODE_H__ */
