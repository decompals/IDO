/**************************************************************************
 *									  *
 * 		 Copyright (C) 1986-1995 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
#ifndef __SYS_SEMA_H__
#define __SYS_SEMA_H__

#ident	"$Revision: 3.73 $"

#include <sys/types.h>

/*
 * Mutual exclusion locks and synchronization routines.
 *
 * The Irix kernel supports both sleeping and spinning mutual exclusion locks,
 * synchronization variables, multi-access/single-update mutual exclusion locks,
 * plus counting semaphores which can be used for both mutual exclusion and
 * synchronization.
 */

/*
 * Sleeping mutex lock routines.
 *
 * Sleeping mutexes implement priority-inheritance: a thread of execution
 * (process) which owns a mutex can inherit a better priority from another
 * thread of execution waiting for the mutex; the inherited priority
 * is lost when the mutex is released.
 *
 * Mutexes can not be acquired by interrupt handlers, and a mutex can only
 * be released by the thread of execution that acquired it -- semaphore
 * routines can be used, instead, by clients than cannot follow these rules. 
 */

typedef struct mutex {
	__psunsigned_t	m_bits;
	struct proc	*m_queue;	/* sleep queue head */
} mutex_t;

/*
 * Types for mutex_init(), mutex_alloc()
 */
#define MUTEX_DEFAULT	0x0

#define METER_NAMSZ	8

struct proc;

#ifdef _KERNEL
/*
 * void	mutex_lock(mutex_t *mp, int flags);
 * int	mutex_trylock(mutex_t *mp); 		1 => success; 0 => failure
 * void	mutex_unlock(mutex_t *mp);
 *
 * flags:
 *	flags & PMASK (sys/param.h): priority at which the process will run
 *	in the kernel while it holds the lock -- if it had to sleep waiting
 *	for the lock.
 *
 *	PRECALC (sys/param.h): ignore priority bits, and instead use
 *	current priority when determining calculating priority after
 *	sleeping on mutex.
 */
extern void	mutex_lock(mutex_t *, int);
extern int	mutex_trylock(mutex_t *);
extern void	mutex_unlock(mutex_t *);

/*
 * void mutex_init(mutex_t *mp, int type, char *name);
 *
 * Name may be null -- it is only used when metering mutexes are installed.
 * Only METER_NAMSZ-1 characters are recorded (see makesname() below).
 */
extern void	mutex_init(mutex_t *, int, char *);
extern void	mutex_destroy(mutex_t *);

/*
 * mutex_t *mutex_alloc(int type, int flags, char *name);
 *
 * Name can be null.
 * Flags: only KM_SLEEP/KM_NOSLEEP are recognized (sys/kmem.h).
 */
extern mutex_t	*mutex_alloc(int, int, char *);
extern void	mutex_dealloc(mutex_t *);

/*
 * Set name for already-initialized mutex.
 * The name is only used when metering mutexes are installed.
 */
extern void	mutex_initname(mutex_t *, char *);

/*
 * Debugging routines.
 *
 * mutex_owner -- returns pointer to owning process (sleeping mutexes only)
 * mutex_owned -- returns non-zero if mutex is currently owned.
 * mutex_mine -- returns non-zero if current thread of execution owns the mutex.
 * mutex_rmproc -- returns non-zero if proc was not on the queue, zero otherwise.
 */
extern struct proc *mutex_owner(mutex_t *);
extern int	mutex_owned(mutex_t *);
extern int	mutex_mine(mutex_t *);
extern int	mutex_rmproc(mutex_t *, struct proc *);
#endif /* _KERNEL */


/*
 * Spinning mutex lock routines.
 */

/*
 * lock_t *spinlock_alloc(int, char *);
 * void spinlock_init(lock_t *lp, char *name);
 * void meter_spinlock(lock_t *lp, char *name);
 *
 * Allocate+initialize/initialize spinning lock.
 * Name may be null -- it is only used when debugging spinlocks are installed.
 * Only METER_NAMSZ-1 characters are recorded (see makesname() below).
 *
 * meter_spinlock is used to force spinlock metering for the named spinlock
 * when the metering spinlock package is installed, but global spinlock
 * metering is disabled.
 *
 * void spinlock_dealloc(lock_t *lp);
 * void spinlock_destroy(lock_t *lp);
 *
 * Decommission+deallocate/decommission spinning lock.
 */
#ifdef _KERNEL
extern lock_t	*spinlock_alloc(int, char *);
extern void	spinlock_dealloc(lock_t *);
extern void	spinlock_init(lock_t *, char *);
extern void	spinlock_destroy(lock_t *);
extern void	meter_spinlock(lock_t *, char *);

typedef		int (*splfunc_t)(void);

/*
 * int	mutex_spinlock(lock_t *sp);
 * int	mutex_spintrylock(lock_t *sp);
 * int	mutex_spinlock_spl(lock_t *sp, splfunc_t);
 *
 *	return an opaque ``cookie'' that is passed to mutex_spinunlock();
 *	mutex_spinlock and mutex_spintrylock set interrupt level to splhi;
 *	mutex_spintrylock returns 0 on failure, and doesn't set ipl;
 *	mutex_spinlock_spl calls the specified ipl routine;
 *
 * void	mutex_spinunlock(lock_t *sp, int rv);
 * int	mutex_spintrylock(lock_t *sp, int rv);
 *	returns non-zero on success, 0 on failure
 */
#if MP
extern int	mutex_spintrylock_spl(lock_t *, splfunc_t);
extern int	mutex_spinlock(lock_t *);
extern int	mutex_spintrylock(lock_t *);
extern void	mutex_spinunlock(lock_t *, int);
extern int	mutex_spinlock_spl(lock_t *, splfunc_t);
extern int	spinlock_islocked(lock_t *);
extern int	spinlock_initialized(lock_t *);
#else
extern int			splhi(void);
extern void			splx(int);
#define mutex_spinlock(l)	splhi()
#define mutex_spintrylock(l)	splhi()
#define mutex_spinunlock(l,x)	splx(x)
#define mutex_spinlock_spl(x,y)	y()
#define mutex_spintrylock_spl(x,y)	y()

#define	spinlock_islocked(l)	issplhi(getsr())
#define	spinlock_initialized(l)	1
#endif

#endif /* _KERNEL */

/*
 * Sync variables.
 *
 * Wait for a condition/state change.
 */
typedef __psunsigned_t	sv_t;

/*
 * void	sv_wait(sv_t *svp, int flags, void *lp, int rv);
 * int	sv_wait_sig(sv_t *svp, int flags, void *lp, int rv);
 *
 * Place caller on svp's wait-queue,
 * releasing (spinning or sleeping) mutex lock.
 *
 * If passed lock is a sleeping mutex, rv must be 0;
 * otherwise it is the return value from mutex_spinlock call --
 *
 * flags:
 *	PRECALC (sys/param.h): ignore priority bits, and instead use
 *	current priority when determining calculating priority after
 *	sleeping on mutex.
 *
 *	PNOSTOP (sys/param.h): return if signal is pending, but do not
 *	handle the signal (sv_wait_sig only).
 *
 *	flags & TIMER_SHIFT(index) (sys/timers.h): sleep state to which
 *	process is charged while waiting on this sync variable.
 */
extern void	sv_wait(sv_t *, int, void *, int);
extern int	sv_wait_sig(sv_t *, int, void *, int);

/*
 * sv_signal -- wake up, at most, one process waiting on sync-variable
 * sv_broadcast -- wake up all processes waiting on sync-variable
 *
 * return: number of processes wakened
 */
extern int	sv_signal(sv_t *);
extern int	sv_broadcast(sv_t *);

/*
 * Initialize sync variables.
 *
 * void	sv_init(sv_t *svp, int type, char *name);
 *
 * Name may be null; used only when metering routines are installed.
 */
extern void	sv_init(sv_t *, int, char *);

#define SV_FIFO		0x0		/* sv_t is FIFO type */
#define SV_LIFO		0x2		/* sv_t is LIFO type */
#define SV_PRIO		0x4		/* sv_t is PRIO type */
#define SV_DEFAULT	SV_FIFO

/*
 * sv_t *sv_alloc(int type, int flags, char *name)
 *
 * Name may be null.
 * Type: SV_DEFAULT or SV_LIFO.
 * Flags: only KM_SLEEP/KM_NOSLEEP are recognized (sys/kmem.h).
 */
extern sv_t	*sv_alloc(int, int, char *);

/*
 * sv_destroy -- uninitialize sync variable
 * sv_dealloc -- uninitialize and deallocate sync variable
 */
extern void	sv_destroy(sv_t *);
extern void	sv_dealloc(sv_t *);

/*
 * sv_initname -- set name for already-initialized sync variable.
 */
extern void	sv_initname(sv_t *, char *);

/*
 * wsycnv  -- remove p from svp's wait-queue, and place on scheduler run queue
 * unsycnv -- remove p from svp's wait-queue
 *
 * return: 1 if process was found on wait-queue, 0 otherwise.
 */
extern int	wsyncv(sv_t *, struct proc *);
extern int	unsyncv(sv_t *, struct proc *);

/*
 * Debugging routines:
 *
 * int	sv_waitq(sv_t *svp) -- returns the # of processes sleeping on svp
 * void	sv_meter(sv_t *svp, char *name) -- enable metering for svp iff metering
 *					   package is installed
 */
extern int	sv_waitq(sv_t *);
extern void	sv_meter(sv_t *, char *);


/*
 * Counting semaphores.
 *
 * Multi-use synchronizing objects.
 *
 * Can be used for mutual exclusion by clients which break basic
 * mutex_lock rules, or which don't want priority inheritance.
 * Semaphores _are_ breakable, depending on the flags argument to psema(),
 * and _do_ support non-local goto (via longjmp if PCATCH isn't set.
 * Mutual exclusing semaphore must be initialized to 1.
 *
 * Semaphores can also be used as synchronizing objects.  The semantics
 * differ from sync variables, in that a mutual exclusion lock isn't
 * implicitly released when the sync semaphore is ``acquired''.
 */

/*
 * Semaphore structure.
 */
typedef struct sema_s {
	union {
		struct {
			short	count;
			short	flags;
		} s_st;
		uint_t	s_lock;
	} s_un;
	struct proc	*s_queue;	/* sleep queue head */
} sema_t;

/*
 * sema flags -- depend on big-endian ordering to work
 */
#define SEMA_NOHIST	0x0001	/* don't collect semaphore history */
#define SEMA_LOCK	0x0004	/* lock bit */
#define SEMA_METER	0x0008	/* collect semaphore metering */

#define SEMA_NAMSZ	METER_NAMSZ	/* obsolete */

#ifdef _KERNEL
/*
 * void	initsema(sema_t *sp, int count);
 * void	initnsema(sema_t *sp, int count, char *name);
 * void	freesema(sema_t *sp);
 * sema_t sema_alloc(int vmflags, int count, char *name);
 * void	sema_dealloc(sema_t *sp);
 *
 * count: 1 for mutual-exclusion semaphores; 0 for synchronizing semaphores
 */
extern void	initsema(sema_t *, int);
extern void	initnsema(sema_t *, int, char *);
extern void	freesema(sema_t *);
extern sema_t	*sema_alloc(int, int, char *);
extern void	sema_dealloc(sema_t *);
extern short	valusema(sema_t *);

#define initnsema_mutex(S, N)	initnsema(S, 1, N)
#define initnsema_synch(S, N)	initnsema(S, 0, N)

/*
 * semaphore-equivalents of sv_wait and sv_waitsig -- these take pointers
 * to locked semaphores as the mutual exclusion lock to release after the
 * caller is placed on svp's sleep queue.
 *
 * void	sv_sema_wait(sv_t *svp, int flags, sema_t *sp);
 * int	sv_sema_wait_sig(sv_t *svp, int flags, sema_t *sp);
 *
 * The arguments are:
 *	sync-variable on which to sleep;
 *	flags for sync-variable;
 *	address of semaphore to release.
 */
extern void	sv_sema_wait(sv_t *, int, sema_t *);
extern int	sv_sema_wait_sig(sv_t *, int, sema_t *);

/*
 * char *makesname(char *name, const char *prefix, int ival);
 *
 *	char name[METER_NAMSZ];
 *	for (i = 0, bp = buf; i < nbuf; i++, bp++)
 *		initnsema(&bp->b_lock, 1, makesname(name, "buf", i));
 */
extern char	*makesname(char *, const char *, int);

/*
 * initsemaname -- (re)-initialize semaphore name.
 * semameteron -- enable metering for the specified semaphore
 * semameteroff -- disable metering for the specified semaphore
 *
 * These routines do nothing if metering packing isn't installed.
 * Metering package recognizes only METER_NAMSZ-1 characters.
 */
extern void	initsemaname(sema_t *, char *);
extern int	semameteron(sema_t *);
extern void	semameteroff(sema_t *);

#ifdef DEBUG
extern int	ownsema(sema_t *);
#endif

/*
 * int	psema(sema_t *sp, int flags);
 *
 * Called to acquire a semaphore unconditionally.
 * Returns:
 *	0 - semaphore ``acquired'' (may sleep)
 *	If (flags & PMASK) > PZERO:
 *         -1 - PCATCH set and signal occurred
 *         -2 - PCATCH set and signal woke up process
 *		but has since been canceled
 *	   longjmp() if PCATCH not set and signal occurred
 */
extern int	psema(sema_t *, int);

/*
 * int cpsema(sema_t *sp);
 *
 * Returns:	1 if semaphore acquired;
 *		0 if semaphore not acquired.
 *
 * Acquiring semaphore decrements its count;
 * semaphore acquired only if semaphore count was > 0 on entry.
 */
extern int	cpsema(sema_t *);

/*
 * int vsema -- unconditionally increment counter, wakes up first queued process
 * int cvsema -- conditionally increments counter and wake up first process
 *
 * return 1 if a queued process was woken; 0 otherwise.
 */
extern int	vsema(sema_t *);
extern int	cvsema(sema_t *);

/*
 * int	wsema -- remove process from sema's wait-queue, and schedule to run
 *
 * return 1 if process was found on wait-queue, 0 otherwise.
 */
extern int	wsema(sema_t *, struct proc *);

/*
 * Turn semaphore history mechanism off/on.
 * Ignored when metering system isn't installed.
 */
extern void semahistoff(sema_t *);
extern void semahiston(sema_t *);

/*
 * If metering is installed, and defaultsemameter is set, _all_ semaphores,
 * sleeping mutexes, sync-variables and mrlocks are metered.
 * This variable is set iff DEFAULTSEMAMETER is defined in the system's
 * irix.sm config file.
 */
extern unsigned int	defaultsemameter;

#endif /* _KERNEL */


/*
 * Multi-reader locks
 */
typedef struct mrlock_s {
	int	mr_bits;
	sema_t	mr_wait;	/* wait for access/update lock */
} mrlock_t;

/*
 * Arguments to mrlock()
 */
#define MR_ACCESS	0x1
#define MR_UPDATE	0x2

#ifdef _KERNEL
extern void	mrdemote(mrlock_t *);
extern void	mrunlock(mrlock_t *);
extern int	cmrlock(mrlock_t *, int);
extern void	mrlock(mrlock_t *, int, int);
extern void	mrinit(mrlock_t *, char *);
extern void	mrfree(mrlock_t *);
extern int	mrmeteron(mrlock_t *);
extern void	mrmeteroff(mrlock_t *);
extern int	cmrpromote(mrlock_t *);
extern int	ismrlocked(mrlock_t *, int);
extern int	mrlocked_acc(mrlock_t *);
extern int	mrlocked_wait(mrlock_t *);

/*
 * mrunlock-equivalents of sv_wait and sv_waitsig.
 *
 * void	sv_mrlock_wait(sv_t *svp, int flags, mrlock_t *mrp);
 * int	sv_mrlock_wait_sig(sv_t *svp, int flags, mrlock_t *mrp);
 *
 * The arguments are:
 *	sync-variable on which to sleep;
 *	flags for sync-variable;
 *	address of mrlock to release.
 */
extern void	sv_mrlock_wait(sv_t *, int, mrlock_t *);
extern int	sv_mrlock_wait_sig(sv_t *, int, mrlock_t *);
#endif /* _KERNEL */


/*
 * A number of locking primitives are provided that are compiled out
 * on non-MP systems.
 * These aren't intended for DDI/DKI-conforming drivers.
 */
#ifdef _KERNEL
/*
 * sleeping and spinning mutexes, and sync-variables
 */
#ifdef MP
#define	mp_mutex_lock(m,f)		mutex_lock(m,f)
#define	mp_mutex_unlock(m)		mutex_unlock(m)
#define	mp_mutex_trylock(m)		mutex_trylock(m)
#define	mp_sv_wait(sv,f,m,s)		sv_wait(sv,f,m,s)
#define	mp_sv_wait_sig(sv,f,m,s)	sv_wait_sig(sv,f,m,s)
#define	mp_mutex_spinlock(m)		mutex_spinlock(m)
#define	mp_mutex_spinunlock(m,s)	mutex_spinunlock(m,s)

/*
 * mp_spin-routines provide no interrupt protection, and are
 * compiled out on uniprocessor systems.  Useful only in interrupt
 * handlers where further same-level interrupts are already blocked.
 */
extern void mp_spinlock(lock_t *);
extern void mp_spinunlock(lock_t *);

/*
 * The nested_spin routines do not provide protection against
 * premption or interrupts, and can be only used when another
 * mutex_spinlock is already held -- the debugging spinlock code
 * enforces this.
 */
extern void nested_spinlock(lock_t *);
extern int nested_spintrylock(lock_t *);
extern void nested_spinunlock(lock_t *);

#else /* !MP */

#define mp_mutex_lock(m,f)
#define mp_mutex_unlock(m)
#define mp_mutex_trylock(m)		1
#define	mp_sv_wait(sv,f,m,s)		sv_wait(sv,f,0,0)
#define	mp_sv_wait_sig(sv,f,m,s)	sv_wait_sig(sv,f,0,0)
#define	mp_mutex_spinlock(m)	1
#define	mp_mutex_spinunlock(m,s)
#define	mp_spinlock(m)
#define	mp_spinunlock(m)
#define	nested_spinlock(m)
#define nested_spintrylock(m)	1
#define	nested_spinunlock(m)

#endif	/* !MP */

/*
 * Multi-access/single-update locks.
 */
#ifdef MP
#define		mp_ismrlocked(a,b)		ismrlocked(a,b)
#define		mp_mrlock(a,b,c)		mrlock(a,b,c)
#define		mp_mrunlock(a)			mrunlock(a)
#else /* !MP */
#define mp_ismrlocked(a,b)			1
#define mp_mrlock(a,b,c)			0
#define mp_mrunlock(a)
#endif	/* !MP */

/*
 * mp_psema and mp_vsema are cast as voids because the UP case below
 * defines away to nothing.  The void cast here makes the MP
 * case match those semantics.
 * 
 * Why, in the UP case, not just define mp_psema and mp_vsema as 0?
 * Because that causes lots of 'statement has no effect' warnings,
 * which we don't want to WOFF.
 */
#ifdef MP
#define		mp_psema(x,y)			(void)psema(x,y)
#define		mp_vsema(x)			(void)vsema(x)
#define		mp_cpsema(x)			cpsema(x)
#define		mp_cvsema(x)			cvsema(x)
#else
#define mp_psema(x,y)
#define mp_vsema(x)
#define mp_cpsema(x)				1
#define mp_cvsema(x)				0
#endif


/*
 * Bit locks -- these are similar to the mutex_spinlock suite,
 * except the lock is a single bit in a 32-bit or 64-bit word.
 *
 * int	mutex_bitlock(uint_t *lock, uint_t lock-bit);
 * int	mutex_bittrylock(uint_t *lock, uint_t lock-bit);
 * void	mutex_bitunlock(uint_t *lock, uint_t lock-bit, int rv);
 *
 * nested_bitlock can only be called when ``protected'' by another
 * mutex_bitlock or mutex_spinlock.
 *
 * mutex_bittrylock returns 0 on failure, cookie for unlock on success.
 *
 * int	bitlock_islocked(uint *lock, uint lock-bit) -- debug
 */

extern void		init_bitlock(uint *, uint, char *);
extern void		destroy_bitlock(uint *);
extern void		init_64bitlock(__uint64_t *, __uint64_t, char *);
extern void		destroy_64bitlock(__uint64_t *);
extern void		meter_bitlock(uint *, char *);
extern void		meter_64bitlock(__uint64_t *, char *);

#if MP
extern int		mutex_bitlock(uint *, uint);
extern int		mutex_bittrylock(uint *, uint);
extern void		mutex_bitunlock(uint *, uint, int);

extern int		mutex_64bitlock(__uint64_t *, __uint64_t);
extern int		mutex_64bittrylock(__uint64_t *, __uint64_t);
extern void		mutex_64bitunlock(__uint64_t *, __uint64_t, int);

extern void		nested_bitlock(uint *, uint);
extern int		nested_bittrylock(uint *, uint);
extern void		nested_bitunlock(uint *, uint);

extern void		nested_64bitlock(__uint64_t *, __uint64_t);
extern int		nested_64bittrylock(__uint64_t *, __uint64_t);
extern void		nested_64bitunlock(__uint64_t *, __uint64_t);

extern int		bitlock_islocked(uint_t *, uint_t);
extern int		mutex_bitlock_spl(uint_t *, uint_t, splfunc_t);

#define mp_mutex_bitlock(l,b)		mutex_bitlock(l,b)
#define mp_mutex_bitunlock(l,b,x)	mutex_bitunlock(l,b,x)

/*
 * mp_spin-routines provide no interrupt protection, and are
 * compiled out on uniprocessor systems.  Useful only in interrupt
 * handlers where further same-level interrupts are already blocked.
 */
extern void mp_bitlock(uint_t *, uint_t);
extern void mp_bitunlock(uint_t *, uint_t);
extern void mp_64bitlock(__uint64_t *, __uint64_t);
extern void mp_64bitunlock(__uint64_t *, __uint64_t);

#else /* !MP */

#define mutex_bitlock(l,b)	splhi()
#define mutex_bittrylock(l,b)	splhi()
#define mutex_bitunlock(l,b,x)	splx(x)

#define mutex_64bitlock(l,b)	splhi()
#define mutex_64bittrylock(l,b)	splhi()
#define mutex_64bitunlock(l,b,x)	splx(x)

#define nested_bitlock(l,b)
#define nested_bittrylock(l,b)	1
#define nested_bitunlock(l,b)

#define nested_64bitlock(l,b)
#define nested_64bittrylock(l,b)	1
#define nested_64bitunlock(l,b)

#define bitlock_islocked(l,b)		issplhi(getsr())
#define mutex_bitlock_spl(x,b,y)	y()

#define mp_mutex_bitlock(l,b)	1
#define mp_mutex_bitunlock(l,b,x)

#define	mp_bitlock(l,b)
#define	mp_bitunlock(l,b)
#define	mp_64bitlock(l,b)
#define	mp_64bitunlock(l,b)
#endif /* !MP */

#if (_MIPS_SZPTR == 32)
#define mutex_psbitlock(L,B)		mutex_bitlock(L,B)
#define mutex_psbittrylock(L,B)		mutex_bittrylock(L,B)
#define mutex_psbitunlock(L,B,X)	mutex_bitunlock(L,B,X)
#define nested_psbitlock(L,B)		nested_bitlock(L,B)
#define nested_psbittrylock(L,B)	nested_bittrylock(L,B)
#define nested_psbitunlock(L,B)		nested_bitunlock(L,B)
#endif

#if (_MIPS_SZPTR == 64)			/* _MIPS_SIZELONG had better be 64 */
#define mutex_psbitlock(L,B)		mutex_64bitlock(L,B)
#define mutex_psbittrylock(L,B)		mutex_64bittrylock(L,B)
#define mutex_psbitunlock(L,B,X)	mutex_64bitunlock(L,B,X)
#define nested_psbitlock(L,B)		nested_64bitlock(L,B)
#define nested_psbittrylock(L,B)	nested_64bittrylock(L,B)
#define nested_psbitunlock(L,B)		nested_64bitunlock(L,B)
#endif

/*
 * Fast bitlock/set routines.
 *
 * bitlock_set(uint_t *bits, uint_t lock_bit, uint_t set_bits);
 * bitlock_clr(uint_t *bits, uint_t lock_bit, uint_t clear_bits);
 *
 * These routines atomically set/clear bits in the memory word,
 * respecting the lock_bit -- these are the fast-path equivalents
 * of acquiring the corresponding mutex_bitlock, setting/clearing
 * bits, and releasing the mutex_bitlock.
 */
extern uint_t	bitlock_clr(uint_t *, uint_t, uint_t);
extern uint_t	bitlock_set(uint_t *, uint_t, uint_t);

/*
 * mutex_bitlock-equivalents of sv_wait and sv_waitsig.
 * Only implemented currently for integer-size bitlocks.
 *
 * (Only the uint_t-sized versions are currently implemented.)
 *
 * The arguments are:
 *	sync-variable on which to sleep;
 *	flags for sync-variable;
 *	address of bit-lock to unlock;
 *	the lock bit;
 *	return value from mutex_bitlock call.
 */
extern void	sv_bitlock_wait(sv_t *, int, uint_t *, uint_t, int);
extern int	sv_bitlock_wait_sig(sv_t *, int, uint_t *, uint_t, int);

#ifdef MP
#define	mp_sv_bitlock_wait(s,f,l,b,x)		sv_bitlock_wait(s,f,l,b,x)
#define	mp_sv_bitlock_wait_sig(s,f,l,b,x)	sv_bitlock_wait_sig(s,f,l,b,x)
#else /* !MP */
#define	mp_bitlock_wait(s,f,l,b,x)		sv_wait(s,f,0,0)
#define	mp_bitlock_wait_sig(s,f,l,b,x)		sv_wait_sig(s,f,0,0)
#endif	/* !MP */
#endif /* _KERNEL */

/*
 * Monitor definitions
 */
struct monitor;
struct mon_state;

typedef struct mon_func {
	void		(*mf_init)(struct monitor *);
	void		(*mf_service)(void *);
	void		(*mf_p_mon)(struct monitor *);
	void		(*mf_v_mon)(struct monitor *);
	void		(*mf_q_mon_sav)(struct monitor *, void **);
	void		(*mf_q_mon_rst)(struct monitor *, void *);
	void		(*mf_r_mon)(struct monitor *, struct mon_state *);
	void		(*mf_a_mon)(struct monitor *, struct mon_state *);
} mon_func_t;

#define MON_LOCKED	0x01	/* monitor in use */
#define MON_WAITING	0x02	/* someone waiting for monitor */
#define MON_TIMEOUT	0x04	/* timeout to run mon_runq pending */

typedef struct monitor {
	struct monitor	*mon_next;
	struct monitor	*mon_prev;
	lock_t		mon_lock;		/* private (local) lock */
	uchar_t		mon_lock_flags;
	sv_t		mon_wait;		/* wait here to lock */
	struct monitor	**mon_monpp;
	lock_t		*mon_monp_lock;
	pid_t		mon_pid;		/* current owner's pid */
	int		mon_trips;		/* number of trips */
	void		*mon_p_arg;		/* arg to last pmonitor */
	void		**mon_queue;		/* queue of entries */
	mon_func_t	*mon_funcp;		/* private functions */
	void		*mon_private;		/* private data */
} mon_t;

typedef struct mon_state {
	mon_t		*ms_mon;
	mon_t		**ms_monpp;		/* monitor */
	lock_t		*ms_monp_lock;
	pid_t		ms_pid;			/* owner pid */
	int		ms_trips;		/* # of trips */
	void		*ms_p_arg;		/* arg to pass to pmonitor
						   when re-acquired */
} mon_state_t;

/*
 * NOTE - mon_queue points to anything - assumes that first entry
 * is a list pointer.
 */

#ifdef _KERNEL
extern void	initmonitor(mon_t *, char *, mon_func_t *);
extern int	pmonitor(mon_t *, int, void *);
extern int	qmonitor(mon_t *, void *, void *);
extern void	vmonitor(mon_t *);
extern int	amonitor(mon_state_t *);
extern void	rmonitor(mon_t *, mon_state_t *);
extern mon_t	*allocmonitor(char *, mon_func_t *);
extern void	freemonitor(mon_t *);
#endif /* _KERNEL */

/*
 * Multi-access, single-update spinlocks.
 */
typedef struct mrsplock_s {
	int		mrs_bits;
	short		mrs_spcount;	/* spin this much before retry */
	short		mrs_depth;	/* for recursive write lock */

	struct proc	*mrs_lastupd;	/* last process to update */

	/*
	 * The following are only set when running debug kernels.
	 */
	inst_t		*mrs_lastupc;	/* last routine to update */
	struct proc	*mrs_lastacc;	/* last process to acquire */
	inst_t		*mrs_lastapc;	/* last routine to access */
	union {
		struct {
			short	upd_waiters;	/* metering stats */
			short	acc_waiters;	/* metering stats */
		} wt;
		int	waiters;
	} mrs_wt;
	unsigned int	mrs_updates;	/* metering stats */
	unsigned int	mrs_accesses;	/* metering stats */
} mrsplock_t;

#ifdef _KERNEL
#ifdef MP
extern void	mrinit_splock(mrsplock_t *, char *, short);
extern void	mrsplock(mrsplock_t *, int);
extern void	mrspunlock(mrsplock_t *);
extern void	mrspfree(mrsplock_t *);
extern int	ismrsplocked(mrsplock_t *, int);
#endif /* MP */
#endif /* _KERNEL */


#if _KERNEL && !_STANDALONE
/*
 * splock spinlocks -- these forms are obsolete.
 */
#ifdef MP
#define spsema(x) mp_spinlock(&(x))
#define svsema(x) mp_spinunlock(&(x))
#else
#define spsema(x)
#define svsema(x)
#endif

#define initlock(l)	spinlock_init(l,0)
#define initnlock(l,n)	spinlock_init(l,n)
#define	freesplock(l)	spinlock_destroy(&(l))

#ifdef MP
#define	ownlock(l)	spinlock_islocked(&(l))
#define	splock(l)	mp_mutex_spinlock(&(l))
#define	spunlock(l,s)	mp_mutex_spinunlock(&(l),s)
#define	splockspl(l,f)	mutex_spinlock_spl(&(l),f)
#define	spunlockspl(l,s) mutex_spinunlock(&(l),s)
#define	io_splock(l)	mp_mutex_spinlock(&(l))
extern void		mutex_io_spinunlock(lock_t *, int);
#define	io_spunlock(l,s) mutex_io_spinunlock(&(l),s)
#define	io_splockspl(l,f) mutex_spinlock_spl(&(l),f)
#define	io_spunlockspl(l,s) mutex_io_spinunlock(&(l),s)
#define _trylock(l,f)	mutex_spintrylock_spl(&(l),f)
#else
#define ownlock(x)			1
#define splock(x)			1
#define spunlock(x, y)
#define splockspl(x, y)			y()
#define spunlockspl(x, y)		splx(y)
#define io_splock(x)			1
#define io_spunlock(x, y)
#define io_splockspl(x, y)		y()
#define io_spunlockspl(x, y)		splx(y)
#define mutex_io_spinunlock(l,y)	splx(y)
#define _trylock(l,y)			y()
#endif

/*
 * time_t sv_timedwait(sv_t *svp, mutex_t *mp, time_t time);
 *
 * Puts process on svp's wait-queue, releasing sleeping mutex mp.
 * Attempts wsyncv() at clock tick ``time'' (via timeout()).
 * Returns # of unexpired ticks.
 */
extern time_t	sv_timedwait(sv_t *, mutex_t *, time_t);

extern int	spinunlock_pmonitor(lock_t *, int, mon_t **, int, void *);
extern int	spinunlock_qmonitor(lock_t *, int, mon_t **, void *, void *, int);

/*
 * int	sv_threshold(sv_t *svt, int threshold);
 *
 */
extern int	sv_threshold(sv_t *, int);

/*
 * Aliases for mutexes and sync variables
 * for ease of porting Sun MP code
 */
#define kmutex_t	mutex_t
#define mutex_enter(m)	mutex_lock(m, PZERO)
#define mutex_tryenter(m) mutex_trylock(m)
#define mutex_exit(m)	mutex_unlock(m)

#define kcondvar_t	sv_t
#define cv_init(cv, nm, f, i)	sv_init(cv, SV_DEFAULT, nm)
#define cv_wait(cv, mp)	{ \
	sv_wait(cv, PZERO, mp, 0); \
	mutex_lock(mp, PZERO); \
}
#define cv_signal(cv)		sv_signal(cv)
#define cv_wait_sig(cv,mp)	sv_wait_sig(cv,PZERO,mp,0)
#define cv_broadcast(cv)	sv_broadcast(cv)
#define cv_destroy(cv)		sv_destroy(cv)

/*
 * aliases for read/write locks
 * for ease of porting for Sun MP code
 */
#define RW_READER	MR_ACCESS
#define RW_WRITER	MR_UPDATE

#define krwlock_t	mrlock_t

#define rw_init(r, nm, f, i)	mrinit(r, nm)
#define rw_enter(r, a)			mrlock(r, a, PZERO | PNOSTOP)
#define rw_exit(r)				mrunlock(r)
#define rw_tryupgrade(r)		cmrpromote(r)
#define rw_downgrade(r)			mrdemote(r)
#define rw_destroy(r)			mrfree(r)
#define RW_WRITE_HELD(r)		ismrlocked(r, MR_UPDATE)
#define RW_READ_HELD(r)			ismrlocked(r, MR_ACCESS)
#endif /* _KERNEL */

#endif /* __SYS_SEMA_H__ */
