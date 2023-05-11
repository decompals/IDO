/*	Copyright (c) 1990, 1991 UNIX System Laboratories, Inc.	*/
/*	Copyright (c) 1984, 1986, 1987, 1988, 1989, 1990 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF     	*/
/*	UNIX System Laboratories, Inc.                     	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * 		PROPRIETARY NOTICE (Combined)
 * 
 * This source code is unpublished proprietary information
 * constituting, or derived under license from AT&T's UNIX(r) System V.
 * In addition, portions of such source code were derived from Berkeley
 * 4.3 BSD under license from the Regents of the University of
 * California.
 * 
 * 
 * 
 * 		Copyright Notice 
 * 
 * Notice of copyright on this source code product does not indicate 
 * publication.
 * 
 * 	(c) 1986,1987,1988,1989  Sun Microsystems, Inc
 * 	(c) 1983,1984,1985,1986,1987,1988,1989  AT&T.
 * 	          All rights reserved.
 *  
 */

#ifndef _FS_VNODE_H	/* wrapper symbol for kernel use */
#define _FS_VNODE_H	/* subject to change without notice */

/*#ident	"@(#)uts-3b2:fs/vnode.h	1.23"*/
#ident	"$Revision: 1.81 $"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/sema.h>
#include <sys/uio.h>

/*
 * The vnode is the focus of all file activity in UNIX.
 * A vnode is allocated for each active file, each current
 * directory, each mounted-on file, and the root.
 */

/*
 * vnode types.  VNON means no type.  These values are unrelated to
 * values in on-disk inodes.
 */
typedef enum vtype {
	VNON	= 0,
	VREG	= 1,
	VDIR	= 2,
	VBLK	= 3,
	VCHR	= 4,
	VLNK	= 5,
	VFIFO	= 6,
	VXNAM	= 7,
	VBAD	= 8,
	VSOCK	= 9
} vtype_t;

#define ISVDEV(t) \
	((t) == VCHR || (t) == VBLK || (t) == VFIFO || (t) == VXNAM)

/*
 * The prefix of a vnode struct is its freelist linkage.  The freelist
 * header has two pointers so we can insert at either end.
 */
typedef struct vnlist {
	struct vnode	*vl_next;
	struct vnode	*vl_prev;
} vnlist_t;

/*
 * MP locking protocols:
 *	v_flag, v_count, v_vfsmountedhere	VN_LOCK/VN_UNLOCK
 *	v_vfsmountedhere			VN_LOCK covered vnode before
 *						acquiring vfslock around any
 *						v_vfsmountedhere dereference
 *	v_vfsp					VN_LOCK/VN_UNLOCK
 *	v_stream				streams monitor
 *	v_pgcnt					mem_lock/mem_unlock
 *	v_type					read-only or fs-dependent
 *	v_filocks				vfilockmon, in reclock()
 *	v_list, v_hash				freelist lock
  *	v_intpcount				VN_LOCK/VN_UNLOCK
 */
typedef struct vnode {
	struct vnlist	v_list;			/* freelist linkage */
	uint		v_flag;			/* vnode flags (see below) */
	cnt_t		v_count;		/* reference count */
	u_short		v_what;			/* interest mask for imon */
	u_short		v_namecap;		/* name cache capability */
	enum vtype	v_type;			/* vnode type */
	dev_t		v_rdev;			/* device (VCHR, VBLK) */
	struct vfs	*v_vfsmountedhere;	/* ptr to vfs mounted here */
	struct vnodeops	*v_op;			/* vnode operations */
	struct vfs	*v_vfsp;		/* ptr to containing VFS */
	struct stdata	*v_stream;		/* associated stream */
	void		*v_data;		/* private data for fs */
	struct filock	*v_filocks;		/* ptr to filock list */
	mutex_t		*v_filocksem;		/* ptr to mutex for list */
	u_long		v_version;		/* vnode number capability */
	long		v_number;		/* in-core vnode number */
	short		v_listid;		/* free list id */
	cnt_t		v_intpcount;		/* interp. refcount for imon */

	/*
	 * Used only by global cache.
	 */
	struct vnode	*v_hash;		/* hash list for lookup */

	/*
	 * Values manipulated only by VM and
	 * the page/buffer caches.
	 */
	struct pregion	*v_mreg;		/* mapped file region pointer */
	int		v_dbuf;			/* delwri buffer count */
	pgno_t		v_pgcnt;		/* pages hashed to vnode */
	struct pfdat	*v_dpages;		/* delwri pages */
	struct buf	*v_buf;			/* vnode buffer tree head */
	unsigned int	v_bufgen;		/* buf list generation number */
#ifdef VNODE_TRACING
	struct ktrace 	*v_trace;		/* trace header structure */
#endif
} vnode_t;

#define	v_next		v_list.vl_next
#define	v_prev		v_list.vl_prev

/*
 * vnode flags.
 */
#define VROOT		0x01	/* root of its file system */
#define VNOMAP		0x02	/* file cannot be mapped/faulted */
#define VDUP		0x04	/* file should be dup'ed rather then opened */
#define VNOSWAP		0x08	/* file cannot be used as virtual swap device */
#define VNOMOUNT 	0x10	/* file cannot be covered by mount */
#define VISSWAP		0x20	/* vnode is part of virtual swap device */
#define VINACT		0x40	/* vnode is being inactivated */
#define VRECLM		0x80	/* vnode is being reclaimed */
#define VLOCK		0x100	/* lock bit for vnode */
/* available 		0x200		*/
#define VWAIT		0x400	/* waiting for VINACT or VRECLM to finish */
#define VSHARE		0x800	/* vnode part of global cache */
#define VKILLED		0x1000	/* active vnode has been killed */
#define VFLUSH		0x2000	/* vnode being flushed */
#define VGONE		0x4000	/* vnode isn't really here */
#define VXLOCKED	0x8000	/* Xenix frlock */
#define	VNOCACHE	0x10000	/* no cache flag for NFS3 */
#define	VMOUNTING	0x20000	/* mount in progress on vnode */
#define VLOCKHOLD	0x40000	/* VN_HOLD for remote locks */
#define VDFSCONVERTED	0x80000 /* vnodeops have been converted to DFS format */
#define	VNOINTUNLOCK	0x100000 /* no vop_rwunlock on interrupt */
#define VROOTMOUNTOK	0x200000 /* ok to mount on top of this root */
#define VSEMAPHORE	0x400000 /* vnode represents a Posix named semaphore */
#define VUSYNC		0x800000 /* vnode aspace represents usync objects */

/*
 * Flags for vnode operations.
 */
enum rm		{ RMFILE, RMDIRECTORY };	/* rm or rmdir (remove) */
enum symfollow	{ NO_FOLLOW, FOLLOW };		/* follow symlinks (or not) */
enum vcexcl	{ NONEXCL, EXCL };		/* (non)excl create */
enum create	{ CROPEN = -1, CRCREAT, CRMKNOD, CRMKDIR, CRCORE };
						/* reason for create */
enum vrwlock	{ VRWLOCK_READ, VRWLOCK_WRITE, VRWLOCK_WRITE_DIRECT };
						/* type of rw lock */

typedef enum rm		rm_t;
typedef enum symfollow	symfollow_t;
typedef enum vcexcl	vcexcl_t;
typedef enum create	create_t;
typedef enum vrwlock	vrwlock_t;

/*
 * FROM_VN_KILL is a special 'kill' flag to VOP_CLOSE to signify a call 
 * from vn_kill. This is passed as the lastclose field
 */
typedef enum { L_FALSE, L_TRUE, FROM_VN_KILL } lastclose_t;

/*
 * Operations on vnodes.
 */
struct cred;
struct vfs;
struct buf;
struct vattr;
struct flock;
struct pathname;
struct bmapval;
struct pollhead;
struct fid;
union rval;
struct attrlist_cursor_kern;

typedef struct vnodeops {
	int	(*vop_open)(vnode_t **, mode_t, struct cred *);
	int	(*vop_close)(vnode_t *, int, lastclose_t, off_t, struct cred *);
	int	(*vop_read)(vnode_t *, struct uio *, int, struct cred *);
	int	(*vop_write)(vnode_t *, struct uio *, int, struct cred *);
	int	(*vop_ioctl)(vnode_t *, int, void *, int, struct cred *, int *);
	int	(*vop_setfl)(vnode_t *, int, int, struct cred *);
	int	(*vop_getattr)(vnode_t *, struct vattr *, int, struct cred *);
	int	(*vop_setattr)(vnode_t *, struct vattr *, int, struct cred *);
	int	(*vop_access)(vnode_t *, int, int, struct cred *);
	int	(*vop_lookup)(vnode_t *, char *, vnode_t **,
			      struct pathname *, int, vnode_t *, struct cred *);
	int	(*vop_create)(vnode_t *, char *, struct vattr *,
			      enum vcexcl, int, vnode_t **, struct cred *);
	int	(*vop_remove)(vnode_t *, char *, struct cred *);
	int	(*vop_link)(vnode_t *, vnode_t *, char *, struct cred *);
	int	(*vop_rename)(vnode_t *, char *, vnode_t *, char *,
			      struct pathname *npnp, struct cred *);
	int	(*vop_mkdir)(vnode_t *, char *, struct vattr *,
			     vnode_t **, struct cred *);
	int	(*vop_rmdir)(vnode_t *, char *, vnode_t *, struct cred *);
	int	(*vop_readdir)(vnode_t *, struct uio *, struct cred *, int *);
	int	(*vop_symlink)(vnode_t *, char *, struct vattr *, char *,
			       struct cred *);
	int	(*vop_readlink)(vnode_t *, struct uio *, struct cred *);
	int	(*vop_fsync)(vnode_t *, int, struct cred *);
	void	(*vop_inactive)(vnode_t *, struct cred *);
	int	(*vop_fid)(struct vnode *, struct fid **);
	int	(*vop_fid2)(struct vnode *, struct fid *);
	void	(*vop_rwlock)(vnode_t *, vrwlock_t);
	void	(*vop_rwunlock)(vnode_t *, vrwlock_t);
	int	(*vop_seek)(vnode_t *, off_t, off_t*);
	int	(*vop_cmp)(vnode_t *, vnode_t *);
	int	(*vop_frlock)(vnode_t *, int, struct flock *, int, off_t,
			      struct cred *);
	int	(*vop_realvp)(vnode_t *, vnode_t **);
	int	(*vop_bmap)(vnode_t *, off_t, ssize_t,
			    int, struct cred *,
			    struct bmapval *, int *); /* getpage in svr4 */
	void	(*vop_strategy)(vnode_t *, struct buf *); /* putpage in svr4 */
	int	(*vop_map)(vnode_t *, off_t, struct pregion *, char **,
			   size_t, u_int, u_int, u_int, struct cred *);
	int	(*vop_addmap)(vnode_t *, off_t, struct pregion *, addr_t,
			      size_t, u_int, u_int, u_int, struct cred *);
	int	(*vop_delmap)(vnode_t *, off_t, struct pregion *, addr_t,
			      size_t, u_int, u_int, u_int, struct cred *);
	int	(*vop_poll)(vnode_t *, short, int, short *, struct pollhead **);
	int	(*vop_dump)(vnode_t *, caddr_t, daddr_t, u_int);
	int	(*vop_pathconf)(struct vnode *, int, u_long *, struct cred *);
	int	(*vop_allocstore)(struct vnode *, off_t, size_t, struct cred *);

	/* sgi extensions */
	int	(*vop_fcntl)(vnode_t *, int, void *, int, off_t,
			     struct cred *, union rval *);
	int	(*vop_reclaim)(vnode_t *, int);
	int	(*vop_attr_get)(vnode_t *, char *, char *, int *, int,
				struct cred *);
	int	(*vop_attr_set)(vnode_t *, char *, char *, int, int,
				struct cred *);
	int	(*vop_attr_remove)(vnode_t *, char *, int, struct cred *);
	int	(*vop_attr_list)(vnode_t *, char *, int, int,
				 struct attrlist_cursor_kern *,
				 struct cred *);
} vnodeops_t;

#define	VOP_OPEN(vpp, mode, cr) (*(*(vpp))->v_op->vop_open)(vpp, mode, cr)
#define	VOP_CLOSE(vp, f, c, o, cr) (*(vp)->v_op->vop_close)(vp, f, c, o, cr)
#define	VOP_READ(vp,uiop,iof,cr) (*(vp)->v_op->vop_read)(vp,uiop,iof,cr)
#define	VOP_WRITE(vp,uiop,iof,cr) (*(vp)->v_op->vop_write)(vp,uiop,iof,cr)
#define	VOP_IOCTL(vp,cmd,a,f,cr,rvp) (*(vp)->v_op->vop_ioctl)(vp,cmd,a,f,cr,rvp)
#define	VOP_SETFL(vp, f, a, cr) (*(vp)->v_op->vop_setfl)(vp, f, a, cr)
#define	VOP_GETATTR(vp, vap, f, cr) (*(vp)->v_op->vop_getattr)(vp, vap, f, cr)
#define	VOP_SETATTR(vp, vap, f, cr) (*(vp)->v_op->vop_setattr)(vp, vap, f, cr)
#define	VOP_ACCESS(vp, mode, f, cr) (*(vp)->v_op->vop_access)(vp, mode, f, cr)
#define	VOP_LOOKUP(vp,cp,vpp,pnp,f,rdir,cr) \
		(*(vp)->v_op->vop_lookup)(vp,cp,vpp,pnp,f,rdir,cr)
#define	VOP_CREATE(dvp,p,vap,ex,mode,vpp,cr) \
		(*(dvp)->v_op->vop_create)(dvp,p,vap,ex,mode,vpp,cr)
#define	VOP_REMOVE(dvp,p,cr) (*(dvp)->v_op->vop_remove)(dvp,p,cr)
#define	VOP_LINK(tdvp,fvp,p,cr) (*(tdvp)->v_op->vop_link)(tdvp,fvp,p,cr)
#define	VOP_RENAME(fvp,fnm,tdvp,tnm,tpnp,cr) \
		(*(fvp)->v_op->vop_rename)(fvp,fnm,tdvp,tnm,tpnp,cr)
#define	VOP_MKDIR(dp,p,vap,vpp,cr) (*(dp)->v_op->vop_mkdir)(dp,p,vap,vpp,cr)
#define	VOP_RMDIR(dp,p,cdir,cr) (*(dp)->v_op->vop_rmdir)(dp,p,cdir,cr)
#define	VOP_READDIR(vp,uiop,cr,eofp) (*(vp)->v_op->vop_readdir)(vp,uiop,cr,eofp)
#define	VOP_SYMLINK(dvp,lnm,vap,tnm,cr) \
		(*(dvp)->v_op->vop_symlink) (dvp,lnm,vap,tnm,cr)
#define	VOP_READLINK(vp, uiop, cr) (*(vp)->v_op->vop_readlink)(vp, uiop, cr)
#define	VOP_FSYNC(vp,f,cr) (*(vp)->v_op->vop_fsync)(vp,f,cr)
#define	VOP_INACTIVE(vp, cr) (*(vp)->v_op->vop_inactive)(vp, cr)
#define	VOP_FID(vp, fidpp) (*(vp)->v_op->vop_fid)(vp, fidpp)
#define	VOP_FID2(vp, fidp) (*(vp)->v_op->vop_fid2)(vp, fidp)
#define	VOP_RWLOCK(vp,i) (*(vp)->v_op->vop_rwlock)(vp, i)
#define	VOP_RWUNLOCK(vp,i) (*(vp)->v_op->vop_rwunlock)(vp, i)
#define	VOP_SEEK(vp, ooff, noffp) (*(vp)->v_op->vop_seek)(vp, ooff, noffp)
#define	VOP_CMP(vp1, vp2) (*(vp1)->v_op->vop_cmp)(vp1, vp2)
#define	VOP_FRLOCK(vp,cmd,a,f,o,cr) (*(vp)->v_op->vop_frlock)(vp,cmd,a,f,o,cr)
#define	VOP_REALVP(vp1, vp2) (*(vp1)->v_op->vop_realvp)(vp1, vp2)
#define	VOP_BMAP(vp,of,sz,rw,cr,b,n) \
		(*(vp)->v_op->vop_bmap)(vp,of,sz,rw,cr,b,n)
#define	VOP_STRATEGY(vp, bp) (*(vp)->v_op->vop_strategy)(vp, bp)
#define	VOP_MAP(vp,of,pr,a,sz,p,mp,fl,cr) \
		(*(vp)->v_op->vop_map) (vp,of,pr,a,sz,p,mp,fl,cr)
#define	VOP_ADDMAP(vp,of,pr,a,sz,p,mp,fl,cr) \
		(*(vp)->v_op->vop_addmap) (vp,of,pr,a,sz,p,mp,fl,cr)
#define	VOP_DELMAP(vp,of,pr,a,sz,p,mp,fl,cr) \
		(*(vp)->v_op->vop_delmap) (vp,of,pr,a,sz,p,mp,fl,cr)
#define	VOP_POLL(vp, events, anyyet, reventsp, phpp) \
		(*(vp)->v_op->vop_poll)(vp, events, anyyet, reventsp, phpp)
#define	VOP_DUMP(vp,addr,bn,count) (*(vp)->v_op->vop_dump)(vp,addr,bn,count)
#define	VOP_PATHCONF(vp, cmd, valp, cr) \
		(*(vp)->v_op->vop_pathconf)(vp, cmd, valp, cr)
#define VOP_ALLOCSTORE(vp,off,len,cr) \
		(*(vp)->v_op->vop_allocstore) (vp,off,len,cr)
#define	VOP_FCNTL(vp,cmd,a,f,of,cr,rvp) \
		(*(vp)->v_op->vop_fcntl)(vp,cmd,a,f,of,cr,rvp)
#define	VOP_RECLAIM(vp, flag) \
		(*(vp)->v_op->vop_reclaim)(vp, flag)
#define	VOP_ATTR_GET(vp, name, value, valuelenp, flags, cred) \
		(*(vp)->v_op->vop_attr_get)(vp,name,value,valuelenp,flags,cred)
#define	VOP_ATTR_SET(vp, name, value, valuelen, flags, cred) \
		(*(vp)->v_op->vop_attr_set)(vp,name,value,valuelen,flags,cred)
#define	VOP_ATTR_REMOVE(vp, name, flags, cred) \
		(*(vp)->v_op->vop_attr_remove)(vp,name,flags,cred)
#define	VOP_ATTR_LIST(vp, buffer, buflen, flags, cursor, cred) \
		(*(vp)->v_op->vop_attr_list)(vp,buffer,buflen,flags,cursor,cred)

/*
 * I/O flags for VOP_READ and VOP_WRITE.
 */
#define IO_APPEND	0x001	/* append write (VOP_WRITE) */
#define IO_SYNC		0x002	/* sync file I/O (VOP_WRITE) */
#define IO_DIRECT	0x004	/* bypass page cache */
#define IO_IGNCACHE	0x008	/* ignore page cache coherency when doing i/o
					(IO_DIRECT) */
#define IO_GRIO		0x010	/* this is a guaranteed rate request */
#define IO_INVIS	0x020	/* don't update inode timestamps */
#define	IO_DSYNC	0x040	/* sync data I/O (VOP_WRITE) */
#define IO_RSYNC	0x080	/* sync data I/O (VOP_READ) */
#define IO_NFS          0x100   /* this is from the NFS server */
#define	IO_TRUSTEDDIO   0x200	/* direct I/O from a trusted client
     				   so block zeroing is unnecessary */

/*
 * Flags for VOP_LOOKUP.
 */
#define LOOKUP_DIR	0x01	/* want parent dir vp */

/*
 * Vnode attributes.  A bit-mask is supplied as part of the
 * structure to indicate the attributes the caller wants to
 * set (setattr) or extract (getattr).
 */
typedef struct vattr {
	int		va_mask;	/* bit-mask of attributes */
	vtype_t		va_type;	/* vnode type (for create) */
	mode_t		va_mode;	/* file access mode */
	uid_t		va_uid;		/* owner user id */
	gid_t		va_gid;		/* owner group id */
	dev_t		va_fsid;	/* file system id (dev for now) */
	ino_t		va_nodeid;	/* node id */
	nlink_t		va_nlink;	/* number of references to file */
	off_t		va_size;	/* file size in bytes */
	timestruc_t	va_atime;	/* time of last access */
	timestruc_t	va_mtime;	/* time of last modification */
	timestruc_t	va_ctime;	/* time file ``created'' */
	dev_t		va_rdev;	/* device the file represents */
	u_long		va_blksize;	/* fundamental block size */
	blkcnt_t	va_nblocks;	/* # of blocks allocated */
	u_long		va_vcode;	/* version code */
	struct mac_label *va_mac;	/* MAC label for B1 and beyond */
	struct inf_label *va_inf;	/* information label for CMW */
	struct capability *va_cap;	/* capability (privilege) set */
	struct acl	*va_acl;	/* Access Control List */
	u_long		va_xflags;	/* random extended file flags */
	u_long		va_extsize;	/* file extent size */
	u_long		va_nextents;	/* number of extents in file */
	u_long		va_anextents;	/* number of attr extents in file */
} vattr_t;

/*
 * Attributes of interest to the caller of setattr or getattr.
 */
#define	AT_TYPE		0x00000001
#define	AT_MODE		0x00000002
#define	AT_UID		0x00000004
#define	AT_GID		0x00000008
#define	AT_FSID		0x00000010
#define	AT_NODEID	0x00000020
#define	AT_NLINK	0x00000040
#define	AT_SIZE		0x00000080
#define	AT_ATIME	0x00000100
#define	AT_MTIME	0x00000200
#define	AT_CTIME	0x00000400
#define	AT_RDEV		0x00000800
#define AT_BLKSIZE	0x00001000
#define AT_NBLOCKS	0x00002000
#define AT_VCODE	0x00004000
#define AT_MAC		0x00008000
#define AT_UPDATIME	0x00010000
#define AT_UPDMTIME	0x00020000
#define AT_UPDCTIME	0x00040000
#define AT_ACL		0x00080000
#define AT_CAP		0x00100000
#define AT_INF		0x00200000
#define	AT_XFLAGS	0x00400000
#define	AT_EXTSIZE	0x00800000
#define	AT_NEXTENTS	0x01000000
#define	AT_ANEXTENTS	0x02000000

#define	AT_ALL	(AT_TYPE|AT_MODE|AT_UID|AT_GID|AT_FSID|AT_NODEID|\
		AT_NLINK|AT_SIZE|AT_ATIME|AT_MTIME|AT_CTIME|AT_RDEV|\
		AT_BLKSIZE|AT_NBLOCKS|AT_VCODE|AT_MAC|AT_ACL|AT_CAP|\
		AT_INF|AT_XFLAGS|AT_EXTSIZE|AT_NEXTENTS|AT_ANEXTENTS)

#define	AT_STAT	(AT_MODE|AT_UID|AT_GID|AT_FSID|AT_NODEID|AT_NLINK|\
		AT_SIZE|AT_ATIME|AT_MTIME|AT_CTIME|AT_RDEV)

#define	AT_TIMES (AT_ATIME|AT_MTIME|AT_CTIME)

#define	AT_UPDTIMES (AT_UPDATIME|AT_UPDMTIME|AT_UPDCTIME)

#define	AT_NOSET (AT_NLINK|AT_RDEV|AT_FSID|AT_NODEID|AT_TYPE|\
		 AT_BLKSIZE|AT_NBLOCKS|AT_VCODE|AT_NEXTENTS|AT_ANEXTENTS)
	
/*
 *  Modes.  Some values same as S_xxx entries from stat.h for convenience.
 */
#define	VSUID		04000		/* set user id on execution */
#define	VSGID		02000		/* set group id on execution */
#define VSVTX		01000		/* save swapped text even after use */

/*
 * Permissions.
 */
#define	VREAD		00400
#define	VWRITE		00200
#define	VEXEC		00100

#define	MODEMASK	07777		/* mode bits plus permission bits */
#define	PERMMASK	00777		/* permission bits */

/*
 * Check whether mandatory file locking is enabled.
 */

/* #ifdef MERGE */
#define MANDLOCK(vp, mode)	\
	((vp)->v_type == VREG && ((((mode) & (VSGID|(VEXEC>>3))) == VSGID) \
	  || ((vp)->v_flag & VXLOCKED) == VXLOCKED))
#if 0 /* MERGE else case */
#define MANDLOCK(type, mode)	\
	((type) == VREG && ((mode) & (VSGID|(VEXEC>>3))) == VSGID)
#endif
/* #endif MERGE*/

/*
 * VOP_BMAP result parameter type.
 *
 * The bn, offset and length fields are expressed in BBSIZE blocks
 * (defined in sys/param.h).
 * The length field specifies the size of the underlying backing store
 * for the particular mapping.
 * The bsize, pbsize and pboff fields are expressed in bytes and indicate
 * the size of the mapping, the number of bytes that are valid to access
 * (read or write), and the offset into the mapping, given the offset
 * parameter passed to VOP_BMAP.
 *
 * When a request is made to read beyond the logical end of the object,
 * pbsize may be set to 0, but offset and length should be set to reflect
 * the actual amount of underlying storage that has been allocated, if any.
 */
struct bmapval {
	daddr_t	bn;		/* block number in vfs */
	off_t	offset;		/* logical block offset of this mapping */
	int	length;		/* length of this mapping in blocks */
	int	bsize;		/* length of this mapping in bytes */
	int	pbsize;		/* bytes in block mapped for i/o */
	int	pboff;		/* byte offset into block for i/o */
	dev_t	pbdev;		/* real ("physical") device for i/o */
	char	eof;		/* last mapping of object */
};

/*
 * The eof field of the bmapval structure is really a flags
 * field.  Here are the valid values.
 */
#define	BMAP_EOF		0x01	/* mapping contains EOF */
#define	BMAP_HOLE		0x02	/* mapping covers a hole */
#define	BMAP_DELAY		0x04	/* mapping covers delalloc region */
#define	BMAP_FLUSH_OVERLAPS	0x08	/* Don't flush overlapping buffers */
#define BMAP_READAHEAD		0x10	/* return NULL if necessary */

#ifdef _KERNEL

/*
 * This macro determines if a write is actually allowed
 * on the node.  This macro is used to check if a file's
 * access time can be modified.
 */
#define	WRITEALLOWED(vp, cr) \
	((vp)->v_vfsp && ((vp)->v_vfsp->vfs_flag & VFS_RDONLY) == 0)

/*
 * Public vnode manipulation functions.
 */

extern int	vn_open(char *, enum uio_seg, int, mode_t, struct vnode **,
			enum create);
extern int	vn_create(char *, enum uio_seg, struct vattr *, enum vcexcl,
			  int mode, struct vnode **, enum create);
extern int	vn_rdwr(enum uio_rw, struct vnode *, caddr_t, int, off_t,
			enum uio_seg, int, off_t, struct cred *, int *);
extern int	vn_link(char *, char *, enum uio_seg);
extern int	vn_rename(char *, char *, enum uio_seg);
extern int	vn_remove(char *, enum uio_seg, enum rm);

/*
 * Global vnode allocation:
 *
 *	vp = vn_alloc(&fs_vnodeops, vfsp, type, rdev, v_data);
 *	vn_free(vp);
 *
 * Inactive vnodes are kept on an LRU freelist managed by vn_alloc, vn_free,
 * vn_get, vn_kill, vn_purge, and vn_rele.  When vn_rele inactivates a vnode,
 * it puts the vnode at the end of the list unless the v_data member is NULL,
 * which tells vn_rele to insert at the beginning of the freelist.  When vn_get
 * acquires an inactive vnode, it unlinks the vnode from the list;
 * vn_purge puts inactive dead vnodes at the front of the list for rapid reuse.
 *
 * If the freelist is empty, vn_alloc dynamically allocates another vnode.
 * Call vn_free to destroy a vn_alloc'd vnode that has no other references
 * and no valid private data.  Do not call vn_free from within VOP_INACTIVE;
 * just set v_data to NULL and vn_rele will do the right thing.
 *
 * A vnode might be deallocated after it is put on the freelist (after
 * a VOP_RECLAIM, of course).  In this case, the vn_epoch value is
 * incremented to define a new vnode epoch.
 */
extern vnode_t	*vn_alloc(struct vnodeops *, struct vfs *, enum vtype, dev_t,
			  void *);
extern void	vn_free(struct vnode *);

/*
 * Acquiring and invalidating vnodes:
 *
 *	if (vn_get(vp, version))
 *		...;
 *	vn_purge(vp, version);
 *
 * vn_get, vn_purge,  and vn_kill must be called with vmap_t arguments,
 * sampled while a lock that the vnode's VOP_RECLAIM function acquires is
 * held, to ensure that the vnode sampled with the lock held isn't
 * recycled (VOP_RECLAIMed) or deallocated between the release of the lock
 * and the subsequent vn_get or vn_kill.
 */

extern int		vn_epoch;		/* incore vnode capability -- */
						/* changed when any vnodes */
						/* are deallocated */
/*
 * vnode_map structures _must_ match vn_epoch and vnode structure sizes.
 */
typedef struct vnode_map {
	u_long		v_version;		/* vnode number capability */
	long		v_number;		/* in-core vnode number */
	int		v_epoch;		/* epoch in vnode history */
	int		v_id;			/* freeelist id */
} vmap_t;

#define	VMAP(vp, vmap)	{(vmap).v_version = (vp)->v_version, \
			 (vmap).v_number = (vp)->v_number, \
			 (vmap).v_id = (vp)->v_listid, \
			 (vmap).v_epoch = vn_epoch; }

extern vnode_t *vn_get(struct vnode *, vmap_t *);
extern vnode_t *vn_get_flags(struct vnode *, vmap_t *, uint);
extern void	vn_kill(struct vnode *, vmap_t *);
extern void	vn_gone(struct vnode *);
extern void	vn_purge(struct vnode *, vmap_t *);

/*
 * Flags for vn_get_flags().
 */
#define	VN_GET_NOWAIT	0x1	/* Don't wait for inactive or reclaim */

/*
 * Vnode reference counting functions (and macros for compatibility).
 */
extern vnode_t	*vn_hold(struct vnode *);
extern void	vn_rele(struct vnode *);

#ifdef VNODE_TRACING
#define VN_HOLD(vp)		\
	((void)vn_hold(vp), \
	 (vnode_do_trace ? vn_trace_hold(vp, __FILE__, __LINE__) : (void)0))
#define VN_RELE(vp)		\
	((vnode_do_trace ? vn_trace_rele(vp, __FILE__, __LINE__) : (void)0), \
	 vn_rele(vp))
#else
#define VN_HOLD(vp)		((void)vn_hold(vp))
#define VN_RELE(vp)		(vn_rele(vp))
#endif

/*
 * Vnode spinlock manipulation.
 */
#define	VN_LOCK(vp)		mutex_bitlock(&(vp)->v_flag, VLOCK)
#define	VN_UNLOCK(vp,s)		mutex_bitunlock(&(vp)->v_flag, VLOCK, s)
#define VN_FLAGSET(vp,b)	bitlock_set(&(vp)->v_flag, VLOCK, b)
#define VN_FLAGCLR(vp,b)	bitlock_clr(&(vp)->v_flag, VLOCK, b)

/*
 * lock the vfs given the vnode pointer
 */
#define VN_VFSLOCK(vp)		vfs_lock((vp)->v_vfsp)
#define VN_VFSUNLOCK(vp)	vfs_unlock((vp)->v_vfsp)

/*
 * Some useful predicates.
 */
#define	VN_MAPPED(vp)	((vp)->v_mreg != NULL)
#define	VN_DIRTY(vp)	((vp)->v_dbuf || (vp)->v_dpages)
#define	VN_CACHED(vp)	((vp)->v_pgcnt != 0)

/*
 * Compare two vnodes for equality.  In general this macro should be used
 * in preference to calling VOP_CMP directly.
 */
#define VN_CMP(VP1,VP2)	((VP1) == (VP2) || \
	((VP1) && (VP2) && (VP1)->v_op == (VP2)->v_op ? VOP_CMP(VP1,VP2) : 0))

/*
 * Flags to VOP_SETATTR/VOP_GETATTR.
 */
#define	ATTR_UTIME	0x01	/* non-default utime(2) request */
#define	ATTR_EXEC	0x02	/* invocation from exec(2) */
#define	ATTR_COMM	0x04	/* yield common vp attributes */
#define	ATTR_DMI	0x08	/* invocation from a DMI function */
#define	ATTR_LAZY	0x80	/* set/get attributes lazily */

/*
 * Flags to VOP_FSYNC and VOP_RECLAIM.
 */
#define FSYNC_NOWAIT	0	/* asynchronous flush */
#define FSYNC_WAIT	0x1	/* synchronous fsync or forced reclaim */
#define FSYNC_INVAL	0x2	/* flush and invalidate cached data */
#define FSYNC_DATA	0x4	/* synchronous fsync of data only */

/*
 * Generally useful macros.
 */
#define	VBSIZE(vp)	((vp)->v_vfsp->vfs_bsize)
#define	NULLVP		((struct vnode *)0)
#define	NULLVPP		((struct vnode **)0)

struct pathname;
extern int lookupname(char *, enum uio_seg, enum symfollow, vnode_t **,
		      vnode_t **);
extern int lookuppn(struct pathname *, enum symfollow, vnode_t **, vnode_t **);
extern int traverse(vnode_t **);

extern int chklock(struct vnode *, int, off_t, int, int);
extern void cleanlocks(struct vnode *, pid_t, __uint32_t);

extern struct vnode *rootdir;	/* pointer to vnode of root directory */
extern lock_t mreg_lock;	/* protects all v_mreg manipulation */

extern int namesetattr(char *, enum symfollow, struct vattr *, int);

/*
 * Imon support.
 */
struct irix5_stat;
struct irix5_64_stat;
struct stat64;
extern int xcstat(struct vnode *, void *, struct cred *);
extern int xcstat64(struct vnode *, void *, struct cred *);
extern int irix5_64_xcstat(vnode_t *, struct irix5_64_stat *, struct cred *);
extern void (*imon_hook)(struct vnode *, dev_t, ino_t);
extern void (*imon_event)(struct vnode *, struct cred *cr, int);
extern int imon_enabled;
extern void vn_unplug(vnodeops_t *);

#define IMON_EVENT(vp,cr,ev) if (imon_enabled) { (*imon_event)(vp,cr,ev); }
#define	IMON_CHECK(vp,dev,ino)	if (imon_enabled) { (*imon_hook)(vp,dev,ino); }

/*
 * Vnode list ops.
 */
#define	vn_append(vp,vl)	vn_insert(vp, (struct vnlist *)(vl)->vl_prev)

extern void vn_initlist(struct vnlist *);
extern void vn_insert(struct vnode *, struct vnlist *);
extern void vn_unlink(struct vnode *);

#ifdef VNODE_TRACING
/*
 * Tracing entries.
 */
#define	VNODE_KTRACE_ENTRY	1
#define	VNODE_KTRACE_HOLD	2
#define	VNODE_KTRACE_REF	3
#define	VNODE_KTRACE_RELE	4

#define	VNODE_TRACE_SIZE	64		/* number of trace entries */
extern void vn_trace_entry(struct vnode *, char *);
extern void vn_trace_hold(struct vnode *, char *, int);
extern void vn_trace_ref(struct vnode *, char *, int);
extern void vn_trace_rele(struct vnode *, char *, int);
#define	VN_TRACE(vp)		vn_trace_ref(vp, __FILE__, __LINE__)
extern int vnode_do_trace;
#else
#define	vn_trace_entry(a,b)
#define	vn_trace_hold(a,b,c)
#define	vn_trace_ref(a,b,c)
#define	vn_trace_rele(a,b,c)
#define	VN_TRACE(vp)
#endif	/* VNODE_TRACING */

#endif	/* _KERNEL */

#endif	/* _FS_VNODE_H */


