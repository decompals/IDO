#ifndef __SYS_SHM_H__
#define __SYS_SHM_H__

#ifdef __cplusplus
extern "C" {
#endif
#include <standards.h>

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
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident	"$Revision: 3.31 $"

/*
**	IPC Shared Memory Facility.
*/

#include <sys/ipc.h>

/*
**	Implementation Constants.
*/

#define	SHMLBA	(256*1024)	/* segment low boundary address multiple */
				/* (SHMLBA must be a power of 2) */

#define	SHMSUS	(64*1024*1024)	/* SPT: segment unit size for SHM_SHATTR */

/*
**	Permission Definitions.
*/

#define	SHM_R	0400	/* read permission */
#define	SHM_W	0200	/* write permission */

/*
**	ipc_perm Mode Definitions.
*/

#define	SHM_INIT	01000	/* grow segment on next attach */

/*
**	Message Operation Flags.
*/

#define	SHM_RDONLY	010000	/* attach read-only (else read-write) */
#define	SHM_RND		020000	/* round attach address to SHMLBA */
#define SHM_SHATTR	040000	/* try to use shared attributes */

typedef ulong_t shmatt_t;

/*
**	Structure Definitions.
*/

/*
**	There is a shared mem id data structure for each segment in the system.
*/

struct shmid_ds {
	struct ipc_perm	shm_perm;	/* operation permission struct */
#if (_MIPS_SZLONG == 32)
	int		shm_segsz;	/* size of segment in bytes */
	char		*shm_amp;	/* ptr to nothing - MIPSABI */
	short		shm_lkcnt;	/* xxx MIPSABI */
	char		shm_pad[2];	/* for swap compatibility */
#else
	/*
	 * for 64 bit, osegsz is filled for binary compat.
	 * A new size_t typed field is added to support >4Gb shms
	 */
	int		shm_osegsz;	/* size of segment in bytes */
	size_t		shm_segsz;	/* size in bytes (works >4Gb) */
	char		shm_pad[4];	/* compatibility */
#endif
	pid_t		shm_lpid;	/* pid of last shmop */
	pid_t		shm_cpid;	/* pid of creator */
	shmatt_t	shm_nattch;	/* used only for shminfo */
	ulong_t		shm_cnattch;	/* used only for shminfo */
	time_t		shm_atime;	/* last shmat time */
	long		shm_pad1;	/* reserved for time_t expansion */
	time_t		shm_dtime;	/* last shmdt time */
	long		shm_pad2;	/* reserved for time_t expansion */
	time_t		shm_ctime;	/* last change time */
	long		shm_pad3;	/* reserved for time_t expansion */
#if _KERNEL
	struct region	*shm_reg;
	long		shm_pad4[3];	/* reserve area  */
#else
	long		shm_pad4[4];	/* reserve area  */
#endif
};

#if defined(_KERNEL) || defined(_KMEMUSER)
struct	shminfo {
	size_t	shmmax;		/* max shared memory segment size */
	size_t	shmmin;		/* min shared memory segment size */
	int	shmmni,		/* # of shared memory identifiers */
		shmseg,		/* max attached shared memory	  */
				/* segments per process		  */
		shmall;		/* no longer used */
};
#endif	/* _KMEMUSER */


/*
 * Shared memory control operations
 */

#define SHM_LOCK	3	/* Lock segment in core */
#define SHM_UNLOCK	4	/* Unlock segment */

#ifdef _KERNEL
/*
 * Macro to generate shmid header lockptr
 */

#define	SHMADDR(X)	&shmsem[X]

#ifdef USE_LGPAGES
extern int      lpg_shmget(sysarg_t,
			   usysarg_t,
			   usysarg_t,
			   int,
			   int,
			   union rval *);
#endif

#endif /* _KERNEL */

#ifndef _KERNEL

extern void	*shmat(int, const void *, int);
extern int	shmdt(const void *);
extern int	shmget(key_t, size_t, int);

#if _NO_XOPEN4
extern int 	shmctl(int, int, ...);
#else	/* _XOPEN4 */
extern int	shmctl(int, int, struct shmid_ds *);
#endif	/* _NO_XOPEN4 */

#endif /* !_KERNEL */

#ifdef __cplusplus
}
#endif

#endif /* !__SYS_SHM_H__ */