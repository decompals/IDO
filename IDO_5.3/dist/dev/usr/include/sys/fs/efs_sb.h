/**************************************************************************
 *									  *
 * 		 Copyright (C) 1988, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

#ifndef	__EFS_SB_
#define	__EFS_SB_

#ident "$Revision: 3.22 $"

/*
 * Structure of the extent filesystem superblock
 */

/*
 * Software summary structure per cylinder group.  When a filesystem is
 * mounted, the system builds a cylinder group table for containing summary
 * allocation information.
 */
struct	cg {
	daddr_t	cg_firstbn;		/* first bn in cg */
	daddr_t	cg_firstdbn;		/* first data block bn */
	ino_t	cg_firsti;		/* first inode # in cg */

	/*
	 * Inode allocation summary information.  We keep a simple rotor
	 * which tracks the lowest free inode for a given cg.
	 * We also keep a 'generation number' to control updating of this:
	 * if, during the search, an ifree occurred while we had reliquished
	 * the fs_semlock, our info is no longer up to date so we should
	 * not advance it.
	 */
	ino_t	cg_lowi;		/* lowest inode # that's free */
	unsigned cg_gen;

	/*
	 * Data allocation summary information.  When the cg is scanned,
	 * we build up information about the total number of free data
	 * blocks, as well the first free data block.
	 */
	long	cg_dfree;		/* count of free data blocks */
	daddr_t	cg_firstdfree;		/* first free data bn */
};

/* structure of the super-block for the extent filesystem */
struct efs {
	/*
	 * This portion is read off the volume
	 */
	__int32_t	fs_size;	/* size of filesystem, in sectors */
	__int32_t	fs_firstcg;	/* bb offset to first cg */
	__int32_t	fs_cgfsize;	/* size of cylinder group in bb's */
	short		fs_cgisize;	/* bb's of inodes per cylinder group */
	short		fs_sectors;	/* sectors per track */
	short		fs_heads;	/* heads per cylinder */
	short		fs_ncg;		/* # of cylinder groups in filesystem */
	short		fs_dirty;	/* fs needs to be fsck'd */
	__int32_t	fs_time;	/* last super-block update */
	__int32_t	fs_magic;	/* magic number */
	char		fs_fname[6];	/* file system name */
	char		fs_fpack[6];	/* file system pack name */
	__int32_t	fs_bmsize;	/* size of bitmap in bytes */
	__int32_t	fs_tfree;	/* total free data blocks */
	__int32_t	fs_tinode;	/* total free inodes */
	__int32_t	fs_bmblock;	/* bitmap location. */
	__int32_t	fs_replsb;	/* Location of replicated superblock. */
	__int32_t	fs_lastialloc;	/* last allocated inode */
	char		fs_spare[20];	/* space for expansion - MUST BE ZERO */
	__int32_t	fs_checksum;	/* checksum of volume portion of fs */

	/*
	 * The remainder is used for in-core manipulation.  During
	 * super-block creation, and possible writing in the root's case,
	 * these fields will be written to disk.  It is assumed when
	 * the super-block is read in that these fields contain trash,
	 * and are accordingly initialized.
	 */
	char	fs_readonly;	/* device is read-only */
	char	fs_fmod;	/* filesystem has been modified */
	char	fs_corrupted;	/* fs is corrupted; no more write's */
	struct cg *fs_cgrotor;	/* free-block rotor */
	long	fs_diskfull;	/* disk is framented, do quick allocs */
	long	fs_freedblock;	/* freed a block since going diskfull */
	dev_t	fs_dev;		/* device fs is mounted on */
	short	fs_inopchunk;	/* # of inodes in an inode chunk */
	daddr_t	fs_inopchunkbb;	/* # of bb's in an inode chunk, rounded up */
	short	fs_minfree;	/* min # of free blocks for file placement */
	short	fs_mindirfree;	/* min # of free blocks for dir placement */
	ino_t	fs_ipcg;	/* # of inodes per cg */
	ino_t	fs_lastinum;	/* last inum in fs */
	ushort	fs_lbshift;	/* logical block shift */
	ushort	fs_bmbbsize;	/* size of bit map in bbs */
	struct	sema_s *fs_semlock;	/* file system semaphore */

	/*
	 * An array of cg structs is managed here.  During mounting
	 * the size of this structure plus the number of cg structs
	 * that will be needed, minus 1 (because of the one defined below),
	 * is dynamically allocated.  This makes it easy to do quick looking
	 * through the cg structs for allocation/de-allocation.
	 */
	struct	cg fs_cgs[1];	/* actually, there are more here */
};

/*
 * dfs_lastialloc is the disk version of the fs_lastialloc field.  It is
 * stored on disk in the area following the on-disk superblock, because
 * that area is cleared by prior kernels everytime the superblock is
 * written out.  This allows a non-zero dfs_lastialloc to be relied on,
 * regardless of what kernel ran the file system previously.
 */
/* overlays the cg structure on disk */
#define dfs_lastialloc(fs) (((ino_t *)(fs + 1))[0])

#define	EFS_MAGIC	0x072959L

/* IRIX 3.3 filesystems need a new
 * magic number to ensure there's no attempt to (disasterously!) use
 * them on a pre-3.3 system.
 */

#define EFS_NEWMAGIC	0x07295a
#define IS_EFS_MAGIC(x)	((x == EFS_MAGIC) || (x == EFS_NEWMAGIC))

/*
 * Values for fs_dirty.  If a filesystem was cleanly unmounted, and started
 * clean before being mounted then fs_dirty will be EFS_CLEAN.  Otherwise,
 * the filesystem is suspect in one of several ways.  If it was a root
 * filesystem and had to be mounted even though it was dirty, the fs_dirty
 * flag gets set to EFS_ACTIVEDIRT so that user level tools know to
 * clean the root filesystem.  If the filesystem was clean and is mounted,
 * then the fs_dirty flag gets set to EFS_ACTIVE.  EFS_DIRTY is a particular
 * value to assign fs_dirty to when a filesystem is known to be dirty.
 */
#define	EFS_CLEAN	0x0000		/* unmounted && clean */
#define	EFS_ACTIVEDIRT	0x0BAD		/* mounted a dirty fs (root only) */
#define	EFS_ACTIVE	0x7777		/* mounted && clean */
#define	EFS_DIRTY	0x1234		/* random value for dirtyness */

#define EFS_MAXLBSIZE	65536		/* largest logical block value */

#if defined _KERNEL || defined _KMEMUSER

#include <sys/sema.h>

struct mount {
	struct vfs	*m_vfsp;	/* back-pointer to vfs */
	struct vnode	*m_devvp;	/* open block device vnode pointer */
	sema_t		m_lock;		/* mount table entry lock */
	u_long		m_flags;	/* flags -- see below */
	u_long		m_ireclaims;	/* count of inodes reclaimed */
	struct inode	*m_inodes;	/* list of all inodes */
	struct inode	*m_rootip;	/* pointer to root directory */
	struct inode	*m_quotip;	/* pointer to quota file */
	u_long		m_btimelimit;	/* block time limit */
	u_long		m_ftimelimit;	/* file time limit */
	u_long		m_qindex;	/* index size of quotas file */
	struct efs	m_efs;		/* dynamic size, must be last */
};

#define M_QENABLED	0x0800	/* quotas are enabled */
#define M_QACTIVE	0x1000	/* quotas being changed, but not enabled */

#ifdef _KERNEL
/*
 * Uni- and multi-processor locking macros.
 */
#define	mlock(mp)	psema(&(mp)->m_lock, PINOD)
#define	munlock(mp)	vsema(&(mp)->m_lock);
#define	mlock_mp(mp)	appsema(&(mp)->m_lock, PINOD)
#define	munlock_mp(mp)	apvsema(&(mp)->m_lock);

#define	vfstom(vfsp)	((struct mount *) (vfsp)->vfs_data)
#define	vfstoefs(vfsp)	mtoefs(vfstom(vfsp))
#define	mtoefs(mp)	(&(mp)->m_efs)

extern void efs_setcorrupt(struct vfs *);

#endif	/* _KERNEL */
#endif	/* _KERNEL || _KMEMUSER */

#endif	/* __EFS_SB_ */
