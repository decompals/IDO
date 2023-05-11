/**************************************************************************
 *									  *
 * 		 Copyright (C) 1989-1993 Silicon Graphics, Inc.		  *
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
#ifndef __SYS_PROC_H__
#define __SYS_PROC_H__

#ident	"$Revision: 3.279 $"

/*	One structure allocated per active process. It contains all
**	data needed about the process while the process may be swapped
**	out.  Other per process data (user.h) may swapped with the
**	process.
*/

#include <sys/types.h>

#ifdef USE_LGPAGES
#include <sys/pfdat.h>
#endif

#ifdef _VM_WS_SWAPPING
/*
 * Working set swapping information structure, for proc and shaddr
 * structures.
 *
 * Most fields are protected only by atomic update operations.
 */

typedef struct ws_proc_s {
	/*
	 * Virtual Memory Working Set Swapping 
	 */
	time_t	ws_slice_limit;		/* working set slice time 	*/
					/* limit (future lbolt value)	*/
	time_t	ws_active_time; 	/* last time process was	*/
					/* runnable			*/
	pri_t	ws_pri;			/* working set priority 	*/
	pri_t	ws_upri;		/* working set priority 	*/
	int	ws_flags;		/* working set state flags	*/

#define P_WSF_HAS_SWAPPED_PAGES 0x0001	/* one or more pages in pmap 	*/
					/* with WS_SENTRY in pg_pfn	*/
#define P_WSF_SWAPPING_IN       0x0002	/* in ws_swapin_process()	*/
#define P_WSF_SWAPPING_OUT	0x0004	/* in ws_swapout_process()	*/
#define P_WSF_CLEAR_SLOAD       0x0008	/* clear SLOAD when possible	*/
#define P_WSF_CLEAR_SLOAD_READY 0x0010	/* ready for SLOAD to be cleared */
#define P_WSF_SET_SLOAD		0x0020	/* SLOAD set after begin cleared */

} ws_proc_t;
#endif /* _VM_WS_SWAPPING */

/*
 * Proc structure and maintenance variables description.
 *
 * The fields in a proc entry are protected by an interwoven set of
 * locks and semaphores.  Separate locks and semaphores are used to
 * squeeze decent performance out of proc table and entry handling.
 * The locks and semaphores, and what they protect, are:
 *
 * The following locks are used solely within a single proc entry,
 * and are thus contained within that entry:
 *
 * SIGLCK -   This bit in p_flag lock is held during signal handling.  It
 *	      protects the fields p_sig, p_sigmask, p_phold, and p_nexit.
 *	      The lock must be held before examining or modifying any of
 *	      these variables at all times.  This lock also is used to
 *	      protect the p_stat, p_flag and p_flag2 fields where
 *	      protection is necessary.  Protects p_sonproc field which
 *	      marks which processors the given proc entry has threads
 *	      running on.  Also synchronizes updates to p_cred pointer so
 *	      that other processes trying to look at our uid's (to check
 *	      permissions to send signals and such) don't get a stale cred
 *	      pointer while we're changing cred structs.
 *
 * p_sema   - This semaphore protects the parent-child-sibling chain
 *	      rooted at the current proc entry.
 *	      Therefore in order to access one's parent, the parent p_sema
 *	      must be obtained - though while doing so the parent may have
 *	      exitted.
 *
 *	      It is important
 *	      to realize that some fields in the proc structure can only
 *	      be modified by the process itself, and thus there is no
 *            protection needed, and the semaphore should not be acquired.
 *
 * p_parlck - This (spinning) mutex protects the p_parent field in the
 *	      current proc entry.
 *
 * p_wait   - This semaphore is used exclusively for the wait(2) system call
 *
 * p_aspacelock -
 *	      Grab the lock for MR_UPDATE, if the operation will change:
 *		1) pregion list
 *		2) regva, pgoff, pgsz, r_list
 *	      Otherwise grab it MR_ACCESS if accessing any translations
 *
 * Links:
 *	      Free proc structures are threaded on p_link (== p_flink)
 *	      Runnable procs are double threaded on p_flink & p_blink
 *	      Sleeping procs are double threaded on p_flink & p_blink
 *	      Active procs are threaded on p_active
 *	      Exiting procs are threaded on p_link
 *
 * The following locks are used in global proc table management:
 *
 * pfreelck - This lock protects the free process list.  This is a list
 *            of all free process slots, to avoid an unnecessary search
 *	      when creating a new process.  The free process list is
 *	      strung through the p_active field of the proc structure.
 *
 * pactlck  - This lock protects the active process list.  This is a list
 *	      of all active processes, and is primarly used when creating
 *	      a new process to reduce the search time for a new PID.  It
 *	      is strung through the p_active field of the proc structure.
 *
 * nextpidlock -
 *	      This lock protects the variables nextpid, nextinuse, and
 *	      pidinuse which are used to generate the next valid PID.
 */

#include <sys/param.h>
#include <sys/sema.h>
#include <sys/time.h>
#include <sys/timers.h>
#include <sys/ptimers.h>
#include <sys/immu.h>
#include <sys/ufchunk.h>
#include <sys/poll.h>

#if defined(_KERNEL) || defined(_KMEMUSER)

#include <sys/region.h>
#include <sys/kabi.h>

struct session;		/* look in session.h for real definition */

#include <sys/kthread.h>

#define p_flink		p_kthread.k_flink
#define p_blink		p_kthread.k_blink
#define p_flag		p_kthread.k_flag
#define p_flag2		p_kthread.k_flag2
#define p_wchan		p_kthread.k_wchan
#define p_w2chan	p_kthread.k_w2chan
#define p_pid		p_kthread.k_pid
#define p_stime		p_kthread.k_stime
#define p_stat		p_kthread.k_stat

#define p_pri		p_kthread.k_pri
#define p_upri		p_kthread.k_upri
#define p_saveupri	p_kthread.k_saveupri
#define p_mblock	p_kthread.k_mblock
#define p_pilist	p_kthread.k_pilist
#define p_pilck		p_kthread.k_pilck
#define p_sonproc	p_kthread.k_sonproc
#define p_mustrun	p_kthread.k_mustrun
#define p_onrq		p_kthread.k_onrq
#define p_lastrun	p_kthread.k_lastrun
#define p_sqself	p_kthread.k_sqself
#define p_rtpri		p_kthread.k_rtpri
#define p_tslice	p_kthread.k_tslice
#define p_dvec		p_kthread.k_dvec
#define p_runqinfo	p_kthread.k_runqinfo
#define p_start		p_kthread.k_start

typedef struct	proc {
	kthread_t	p_kthread;	/* kernel thread common fields */
	char		p_cpu;		/* cpu usage for scheduling */
	char		p_nice;		/* nice for cpu usage */
	char		p_time;		/* resident time for scheduling */
	char		p_hasprda;	/* proc has touched its prda page */
	gid_t		p_pgrp;		/* name of process group leader */
	pid_t		p_ppid;		/* process id of parent*/
	lock_t		p_parlck;	/* lock to protect p_parent */
	struct proc	*p_parent;	/* ptr to parent process */
	struct proc	*p_child;	/* ptr to first child process */
	struct proc	*p_sibling;	/* ptr to next sibling proc on chain */
	struct proc	*p_active;	/* active process chain	    */

	pgno_t		p_size;		/* size of swappable image in pages */
	pgno_t		p_rss;		/* resident size of swappable image  */
					/* in pages.		    */
	pgno_t		p_maxrss;	/* max rss - from u_limit	*/
	unsigned char	*p_tlbpid;	/* pointer to tlbpid array	*/
	unsigned char	*p_icachepid;	/* pointer to tlbpid array	*/
	/*
	 * The following 2 fields are extended if EXTUSIZE == 1 to
	 * accomodate kernel stack extension page.
	 */
	pde_t		p_upgs[USIZE+EXTUSIZE];	/* pdes for upage/user kstk */
	sm_swaphandle_t p_uswap[USIZE+EXTUSIZE]; /* swap handles for upage/ustk	*/
	unsigned short	p_resident;	/* process cannot be swapped	*/
	unsigned short	p_reglockindx;	/* pregion lock index */
	preg_set_t	p_region;	/* process regions		*/
	pgno_t		p_totpregsize;	/* total size of regions. 	*/
	struct pregion	*p_tsave;	/* list of 'saved' text pregions */
	pgno_t		p_nlockpg;	/* # locked pages		*/
	pid_t		p_epid;		/* effective pid; normally same as
					 * p_pid; for servers, the system that
					 * sent the msg */
	sysid_t		p_sysid;	/* normally - same as sysid */
					/* if server - system that sent msg */

	int	p_dismissed_exc_cnt;	/* how many speculative instructions */
					/* have caused exceptions? */
	char		p_vmsched;	/* sched info from VM system	*/
	char		p_prtn;		/* semaphore return value	*/
	short		p_nexit;	/* delay exit */
	struct proc	*p_nexit_waiter;/* process (parent) waiting on ZOMB */

	k_sigset_t	p_sigmask;	/* tracing signal mask for /proc */
	k_sigset_t	*p_phold;	/* pointer to hold signal bit mask */
	k_sigset_t	p_khold;	/* p_phold points here or to prda */
	k_sigset_t	p_sig;		/* signals pending to this	*/
					/* process			*/
	k_sigset_t	p_sigwait;	/* set of signals we are waiting on */
	struct sigvec_s *p_sigvec;	/* pointer to signal vector info */

	mutex_t		p_sema;		/* semaphore on entry		*/
	sv_t		p_wait;		/* waiting for something to happen */
	ushort		p_whystop;	/* Reason for process stop */
	ushort		p_whatstop;	/* More detailed reason */
	time_t		p_utime;	/* user time in ticks */
	ushort		p_pollrotor;	/* wakeup rotor for poll() */
	short		p_xstat;	/* exit status for wait */ 
	struct rusage	*p_ru;
	struct itimer_info {
		struct	itimerval realtimer; /* time to next real alarm */
		lock_t	itimerlock;	/* ITIMER_REAL handling lock */
		__uint64_t interval_tick;
		__uint64_t next_timeout;
		int	interval_unit;
	} p_ii;
	struct ssleep_s	*p_block;	/* block struct for blockproc() */
	struct proc	*p_slink;	/* share group link */
	struct shaddr_s	*p_shaddr;	/* pointer to shared group desc */
	uint		p_shmask;	/* share group share mask */
	char		p_exitsig;	/* signal to send on exit of sharegrp */
	char		p_fp;		/* generate SIGFPE on fp interrupts */
	u_char		p_abi;		/* what abi are we running? */
	char		p_fpflags;	/* fp emulation flags */
	struct syscallsw *p_syscall;	/* pointer to syscall handler info */
	int		p_cpfaultcnt;	/* count the number of cop1 faults */
	struct prda	*p_prda;	/* K0 addr of locked-down prda */

	struct pmap	*p_pmap;	/* page map pointer */
	pde_t		**p_segtbl;	/* primary segment table pointer */
	pde_t		**p_shrdsegtbl;	/* overlap segment table pointer */
#ifndef _K64U64
	pde_t		**p_segtbl_wired;	/* wired VA for p_segtbl */
	pde_t		**p_shrdsegtbl_wired;	/* wired VA for p_shrdsegtbl */
#endif /* !K64U64 */
	ushort		p_segflags;	/* segment table flags */
	ushort		p_jwhatstop;	/* job control stop signal */
	mrlock_t	p_alock;	/* non shared group aspace lock */
	mrlock_t	*p_aspacelock;	/* address space lock */
	struct pwatch_s	*p_watch;	/* watchpoint list */
	struct proc 	*p_pgflink;	/* process group chain */
	struct proc 	*p_pgblink;	/* process group chain */
	struct pgroup_s *p_pgroup;	/* process group structure */
	struct pollhead	p_poll;		/* process select/polling via /proc */
	struct session	*p_session;	/* session */
	struct uidinfo_s *p_uidinfo;	/* count of procs with this uid, ... */
	struct cred	*p_cred;	/* process credentials		*/
	int		p_wdata;	/* current wait return value */
	int		p_wcode;	/* current wait code, 1 byte only */
	caddr_t		p_brkbase;	/* base addr of heap for brk/sbrk */
	size_t		p_brksize;	/* size of heap */
	caddr_t		p_stkbase;	/* base addr of stack (lowest adddr) */
	size_t		p_stksize;	/* size of stack */
	struct user 	*p_user;	/* K2SEG address for u area */
	struct sigqueue *p_curinfo;     /* siginfo for current signal */
	sv_t		p_sigpollwait;	/* waiting in sigpoll() */
	int		p_utlbmissswtch;/* utlbmiss handler index */
	volatile u_long p_procref;	/* #of driver refs to proc */
	struct sat_ev_mask *p_satmask;	/* process-local audit event mask */
	struct vnode	*p_trace;	/* pointer to /proc vnode */
	struct vnode	*p_pstrace;	/* pointer to /proc/pinfo vnode */
	int             p_proctimer;    /* index in to array below  */
	unsigned	p_secsnap;	/* sec snap shot for sleep timers */
#define u_ptimer	u_procp->p_timertab
	ktimer_t        p_timertab[MAX_PROCTIMER]; /*accounting states timers */
	ktimer_t        p_ctimertab[MAX_PROCTIMER]; /*  "          "     '' */
	struct vnode	*p_exec;	/* vnode for original exec */
	time_t		p_slptime;	/* lbolt when going to SSLEEP|SSTOP */
	time_t		p_pmaptrimtime;	/* timestamp for pmap trimming */
	struct arsess	*p_arsess;	/* array session info */

#ifdef _VM_WS_SWAPPING
	ws_proc_t	p_ws;		/* working set swapping variables */
#endif /* _VM_WS_SWAPPING */
#ifdef	_SHAREII
	struct ShadProc_t *p_shareP;	/* pointer to shadow proc entry */
#endif	/* _SHAREII */
	lock_t		p_kobjlock;	/* protect p_kobj lists */
	kobj_t		*p_kobj[MAX_MODULES];
#ifdef ULI
	struct uli	*p_uli;		/* chain of ULIs for this proc */
#endif
#ifdef	_HIBERNATORII
	lock_t		p_hib_lock;	/* Hibernator lock - do we need it? */
	ushort		fill1;
	struct bias	*p_hib_bias;	/* Hibernator accounting bias */
	int		p_hib_set;	/* Hibernator checkpoint-set id */
	int		p_hib_flags;    /* Hibernator checkpoint flags */
	int		p_hib_spare[3]; /* future use */
#endif	/* _HIBERNATORII */

#ifdef USE_LGPAGES
	spec_pglist_t   *p_spages;      /* special pages to wire into tlb */
#endif
	struct exit_callback *p_ecblist; /* funcs to call on exit */
	ptimer_info_t	*p_ptimers;	/* array of posix timers */
#ifdef SWASH
	caddr_t		p_swash_ptes;	/* SWASH page tables */
	caddr_t		p_swash_hndlr;  /* SWASH callback */
	caddr_t		p_swash_state;  /* SWASH callback arg */
#endif
    	__psint_t	p_ppolicy;	/* posix scheduling policy */
} proc_t;

typedef union thread {
	kthread_t	kthread;
	proc_t		proc;
} thread_t;

#define	IS_KTHREAD(t)		(((thread_t *)t)->kthread.k_flag2 & SKTHREAD)

#define p_siglock(p)		mutex_bitlock(&(p)->p_flag, SIGLCK)
#define p_siglock_spl(p,f)	mutex_bitlock_spl(&(p)->p_flag, SIGLCK,f)
#define p_sigunlock(p,rv)	mutex_bitunlock(&(p)->p_flag, SIGLCK, rv)
#define p_flagset(p,b)		bitlock_set(&(p)->p_flag, SIGLCK, b)
#define p_flagclr(p,b)		bitlock_clr(&(p)->p_flag, SIGLCK, b)
#define p_sleep(p,s,f,rv)	sv_bitlock_wait(s, f, &(p)->p_flag, SIGLCK, rv)
#define p_sleepsig(p,s,f,rv) \
			sv_bitlock_wait_sig(s, f, &(p)->p_flag, SIGLCK, rv)
#define p_issiglocked(p) 	bitlock_islocked(&(p)->p_flag, SIGLCK)
#define p_bitlock		p_flag

/* The nested-lock routines are for use only
 * by locking primitives and scheduler!
 */
#define p_nested_siglock(p)	nested_bitlock(&(p)->p_flag, SIGLCK)
#define p_nested_sigunlock(p)	nested_bitunlock(&(p)->p_flag, SIGLCK)


#define origpri(p) ((p)->p_rtpri ? (p)->p_rtpri : (p)->p_saveupri)

#define proc_to_user(p) ((p)->p_user)

#define	p_link		p_flink
#define p_minwd		ps.rs.minwd
#define p_rlink		ps.rs.rlink
#define p_realtimer	p_ii.realtimer
#define p_itimerlock	p_ii.itimerlock
#define p_itimer_tick	p_ii.interval_tick
#define p_itimer_unit	p_ii.interval_unit
#define p_itimer_next	p_ii.next_timeout

#ifdef _K64U64
/* In 64-bit kernels all memory is directly addressable, so there's no need
 * to maintain a separate entry for the wired address of the segtbl(s).
 */
#define	p_segtbl_wired		p_segtbl
#define	p_shrdsegtbl_wired	p_shrdsegtbl
#endif /* _K64U64 */

#ifndef _IRIX_LATER
/* The following macros are here to ease the transition to cred's so
 * we don't have to rewrite all the existing code.  Their values can
 * be read at any time, but stores into them should be done with care
 * since cred structs are shared among processes.
 */
#include <sys/cred.h>
#define p_uid		p_cred->cr_ruid
#define p_suid		p_cred->cr_suid
#define p_sgid		p_cred->cr_sgid
#define p_mac		p_cred->cr_mac
#define p_inf		p_cred->cr_inf
#define p_cap		p_cred->cr_cap
#endif

/* p_itimerunit */
#define FAST_TICK	1
#define SLOW_TICK	0

/* p_segflags - a bitmask to enable fast tests */
#define PSEG_SEG	0x1	/* use segment table */
#define PSEG_TRILEVEL	0x2	/* use tri-level segment table */

/* cpu isn't on (wasn't on) any processor */
#define CPU_NONE (cpuid_t)-1

/* flag codes */

#define	SSYS	0x00000001	/* System (resident) process.	*/
#define	STRC	0x00000002	/* Process is being traced.	*/
#define	SBIT0	0x00000004	/* reserved for atomic-bit0 */
#define SNWAKE	0x00000008	/* Process cannot wakeup by a signal.*/
#define SLOAD	0x00000010	/* In core                      */
#define SULOAD	0x00000020	/* ublock in core		*/
#define	SATMSIM	0x00000040	/* Look for atomic operation simulation */
#define	SIGLCK	0x00000080	/* Lock bit for flags, etc. */
#define SPRSTOP	0x00000100	/* process is being stopped via /proc */
#define SPROCTR	0x00000200	/* sysent/exit/fault/signal tracing via /proc */
#define	SPARSYS	0x00000400	/* tracing sys calls via par */
#define SEXECING 0x0000800	/* process is execing */
#define SPROPEN	0x00001000	/* process is open via /proc	*/
#define SUWANT	0x00002000	/* want ublock in core		*/
#define	SNOCTTY	0x00004000	/* no control terminal */
#define	SOWEUPC	0x00008000	/* owe user an addupc call (profiling tick) */
#define SWSYS	0x00010000	/* process got watchpoint in system */
#define SSHBSYN	0x00020000	/* process needs to sync brkbase/size */
#define SEXIT	0x00040000	/* process is exiting		*/
#define SBSDSTOP 0x00080000	/* proces did BSDsetpgrp so send CLD */
#define SWSRCH	0x00100000	/* process is scanning in wait */
#define SSHDSYN 0x00200000	/* process needs to sync current dir */
#define SSHUSYN 0x00400000	/* process needs to sync uids	*/
#define SSHLSYN 0x00800000	/* process needs to sync ulimit	*/
#define SSHMSYN 0x01000000	/* process needs to sync umask	*/
#define SBLOCK	0x02000000	/* process may need to block itself */
#define SPROF	0x04000000	/* process is profiling */
#define SSTEP	0x08000000	/* process is single stepping */
#define SNOSIG	0x10000000	/* process can't receive any signals */
#define SRECALC 0x40000000	/* short term or long term wait */
#define SGRAPH  0x80000000	/* member of share group just given graphics */

#define PTRACED(p)	((p)->p_flag&(STRC|SPROCTR|SPROPEN))
#define PSYNCFLAGS	(SSHDSYN|SSHUSYN|SSHLSYN|SSHMSYN|SSHBSYN)

/* values for p_flag2 */
#define SNOSWAP	    0x00000001	/* communicate no swap condition vhand->proc */
#define SCEXIT	    0x00000002	/* this process will die when parent exits */
#define SBBLST	    0x00000004	/* its been blasted by sched deadlock scan */
#define SPRPIOPEN   0x00000008	/* process is open via /proc/pinfo */
#define SEXECED	    0x00000010	/* process successfully exec'ed (for setpgid) */
#define	SFIXADE	    0x00000020	/* fixup unaligned address errors */
#define SPARTRC     0x00000040	/* par is tracing this */
#define SWAITCHAN   0x00000080	/* hold wchan */
#define SONCESTOPPED 0x0000100	/* won't run in user space again */
#define SWRTLOCK    0x00000200  /* p_wchan object is rtlock */
#define SWMUTEX	    0x00000400	/* p_wchan object is mutex */
#define SWSV	    0x00000800	/* p_wchan object is sync variable */
#define SLONGJMPOK  0x00001000	/* longjmp permitted during current syscall */
#define SMPCWAROFF  0x00002000	/* turn off libmpc WAR */
#define SPIPE	    0x00004000	/* turn on stream pipe */
#define SISOLATE    0x00008000	/* process is mustrun on an isolated cpu */
#define SPROF32	    0x00010000	/* profiling process uses 32-bit buckets */
#define SOPENPOST   0x00020000	/* process opened Oracle postwait driver */
#define SMUSTRUNLCK 0x00040000	/* process locked to mustrun cpu */
#define SPROFFAST   0x00100000	/* profiling process 10 times per tick */
#define SUSERVME    0x00200000	/* process has/had VME space mapped */
#define SNOMRUNINH  0x00400000	/* children shouldn't inherit mustrun */
#define SCOREPID    0x00800000	/* when dumping core, add pid */
#define	SPRELOADFP  0x01000000	/* preload FP on process switch */
#define SJSTOP	    0x02000000	/* process wants to job control stop */
#define	SWTED	    0x04000000	/* Stopped process has been given */
				/* to parent by wait system call. */
				/* Don't return this process to parent */
				/* again until it runs first.	*/
#define	STLBMISS    0x10000000	/* running with non-standard UTLBMISS handler */
#define SKTHREAD    0x20000000	/* Thread has no user struct */
#define	SASYNCIO    0x80000000	/* Using kernel async i/o */

#ifdef	_HIBERNATORII
/* Hibernator internal flags (p_hib_flags) */
#define PRGRPOPEN	0x00000001	/* open in group */
#define PRGRPWAIT	0x00000002	/* event waited for */
#define PREXIT		0x00000004	/* exit requested via /proc */

#define PRISBLOCKED	0x00000010	/* process is blocked */
#define PRSYSENTRY	0x00000020	/* process is blocked at syscall entry */
#endif	/* _HIBERNATORII */

/*
 * The defines for p_abi has been moved to kabi.  This is to allow
 * kernel modules which do not include proc.h but include xlate.h,
 * which needs these defines, to compile.
 */

#if (_MIPS_SZPTR == 32)
#define	ABI_ICODE	ABI_IRIX5	/* ABI for icode init */
#else
#define	ABI_ICODE	ABI_IRIX5_64	/* ABI for icode init */
#endif

/*
 * These two are temporary. They will be established by lboot in the future
 */
#define	_MIN_ABI	ABI_IRIX5	/* lower bound of syscallsw[] */
#define	_MAX_ABI	ABI_IRIX5_N32	/* upper bound of syscallsw[] */

/* flags for p_fp */
#define	P_FP_SIGINTR1	1
#define	P_FP_SIGINTR2	2

/* Flags for newproc() */
#define NP_FAILOK	0x1	/* don't panic if cannot create process */
#define NP_NOLAST	0x2	/* don't use last process slot */
#define	NP_SYSPROC	0x4	/* system (resident) process */
#endif /* _KERNEL || _KMEMUSER */

/* stat codes */
#define	SSLEEP	1		/* Awaiting an event.		*/
#define	SRUN	2		/* Running.			*/
#define	SZOMB	3		/* Process terminated but not	*/
				/* waited for.			*/
#define	SSTOP	4		/* Process stopped by signal	*/
				/* since it is being traced by	*/
				/* its parent.			*/
#define	SIDL	5		/* Intermediate state in	*/
				/* process creation.		*/
#define SXBRK	7		/* process being xswapped       */

/* Reasons for stopping (values of p_whystop) */
#define	REQUESTED	1
#define	SIGNALLED	2
#define	SYSENTRY	3
#define	SYSEXIT		4
#define FAULTED		5
#define JOBCONTROL	6

/* Reasons for stopping (p_whatstop) */
#define FAULTEDWATCH	1	/* whystop == FAULTED */
#define FAULTEDKWATCH	2	/* whystop == FAULTED */
#define FAULTEDPAGEIN	3	/* whystop == FAULTED */
#define FAULTEDSTACK	4	/* whystop == FAULTED */
#define FAULTEDSCWATCH	5	/* whystop == FAULTED */

/* flags for p_fpflags */
#define	P_FP_IMPRECISE_EXCP	0x01	/* TFP imprecise expeptions */
#define	P_FP_PRESERVE		0x02	/* Preserve p_fpflags across exec */
#define	P_FP_FR			0x04	/* SR_FR bit is set for this process */
#define	P_FP_SMM		0x08	/* FPU is in Sequential Memory Mode */
#define	P_FP_SMM_HDR		0x10	/* SMM specified in elf header */
#define	P_FP_PRECISE_EXCP_HDR	0x20	/* prec exc specified in elf header */
#define P_FP_SPECULATIVE	0x40	/* process executing speculatively */
#define P_FP_OLD_SOFTFP		0x80	/* use old (assembly) softfp package */

#ifdef _KERNEL
/* Special delivery instructions for issig() and fsig() */
#define SIG_ALLSIGNALS		0x0
#define SIG_NOSTOPDEFAULT	0x1	/* Don't deliver stopdefault signals */
#define SIG_NOPTRESCHED		0x2	/* Don't deliver SIGPTRESCHED. */

#define ISSIG(l, w)	(sigisready() && issig(l, w))

/*
 * Flags to prlock() and procfind()
 */
#define	ZNO	0	/* Fail on encountering a zombie process. */
#define	ZYES	1	/* Allow zombies. */
#define EXECNO	0x02	/* fail on encountering an execing process */

extern struct proc	*proc;		/* the proc table itself */

/* fault trace variables */
extern proc_t		*trproc;	/* process being traced */
extern int		trflags;	/* trace flags */

#define	isresident(p)	(p)->p_resident

#if EXTUSIZE == 1
/* Flags for stackext_alloc() */
#define STACKEXT_NON_CRITICAL	0x1	/* Don't use the resvered space */
#define STACKEXT_CRITICAL	0x2	/* Use the resvered space */
#endif /* EXTUSIZE == 1 */

struct rusage;
extern int newproc(int, uint);
extern int procfork(int, uint, pid_t *, caddr_t *, size_t);
extern struct user *uballoc(proc_t *, int);
extern void ubfree(proc_t *, int);
extern void setup_wired_tlb_notme(proc_t *);
extern void setup_wired_tlb(proc_t *);
#ifdef _HIBERNATORII
extern int procnew(int, int, proc_t **, pid_t *);
#else /* _HIBERNATORII */
extern int procnew(int, int, proc_t **);
#endif /* _HIBERNATORII */
extern int procscan(int ((*)(proc_t *, void *, int)), void *);
extern void *proccscan(proc_t *, void *((*)(proc_t *, void *)), void *);
extern proc_t *procfind(pid_t, int);
extern proc_t *proclock(proc_t *, int);
extern void procunlock(proc_t *);
extern void procfree(proc_t *);
extern void procdeact(proc_t *);
extern void procchain(proc_t *, proc_t *);
extern void procunchain(proc_t *);
extern int proc1chain(proc_t *);
extern int inferior(proc_t *);
extern int ancestor(proc_t *);
extern void prochashtables(void);
extern void recalcrss(proc_t *);

struct k_siginfo;
struct sigqueue;
union rval;

extern int wstat(int, int);
extern void winfo(proc_t *, struct k_siginfo *, int);
extern void signal(int, int);
extern void psignal(proc_t *, int);
extern void sigtoproc(struct proc *, int, int);
extern void sigdeq(proc_t *, int, struct sigqueue **);
extern int ksigqueue(struct proc *, int, int, const union sigval, int);
extern int issig(int, uint);
extern int isfatalsig(proc_t *);
extern int sendsig(void (*hdlr)(), int, struct k_siginfo *, int);
extern void hold_nonfatalsig(k_sigset_t *);
extern void release_nonfatalsig(k_sigset_t *);
extern void assign_ksigset(proc_t *, k_sigset_t *, k_sigset_t *);
extern int psig(int);
extern int checkstop(void);
extern int wait_checkstop(proc_t *, int);
extern void stop(proc_t *, ushort, ushort);
extern int fsig(proc_t *, int);
extern void freeproc(proc_t *, int, union rval *);
extern int wait4(int *, int, struct rusage *, pid_t, union rval *);
extern void setprun(proc_t *, int);
extern void setrun(proc_t *);
extern char chgrtpri(proc_t *, char);
extern int chgpri(int, int);
extern int resident(proc_t *);
extern void nonresident(proc_t *);
extern struct user *getub(proc_t *, struct user *);
extern void forceinub(proc_t *);
extern void checkfp(proc_t *, int);
extern char *finduname(proc_t *);
extern void pexit(void);
extern void releasefd(void);

typedef void (*ecbfunc_t)(void*);
extern int add_exit_callback(proc_t *, ecbfunc_t, void *);

#define SYNC_SIGNAL	1
#define ASYNC_SIGNAL	2

/* context switch routines */
extern void qswtch(int);
extern void pswtch(int);
extern void swtch(int);
extern void presume(proc_t *, int);
extern int save(k_machreg_t *);
#if R4000 || R10000
extern void resume(proc_t *, int, unsigned char);
extern void resumekthread(proc_t *, int, unsigned char);
#endif
#if TFP
extern void resume(proc_t *, int, unsigned char, unsigned char);
extern void resumekthread(proc_t *, int, unsigned char, unsigned char);
#endif
extern void resumeidle(int);
#if EXTUSIZE == 1
extern uint stackext_alloc(int);
extern void stackext_free(uint);
extern int setup_stackext(proc_t *);
#endif
#endif

#if defined(_KERNEL) || defined(_KMEMUSER)
/*
 * shared group descriptor
 * Each process that sproc()'s has one of these
 *
 * Locks/Semaphores:
 *
 *	s_listlock - also for modifying/accessing the list s_plink: use shared
 *		shared lock for long term access, or use this for short term
 *
 * 	s_detachsem - a mutex semaphore that serializes access to parts of
 *		detachshaddr(). If a P() on this semaphore is successful,
 *		then, guaranteed that no member of the share group may
 *		remove detach regions of the address space.
 */

typedef struct shaddr_s {
	/* the following are all to handle VM */
	preg_set_t 	s_region;	/* processes' shared pregions */
	pgno_t		s_totpregsize;	/* total size of shared regions. */
	mrlock_t	s_aspacelock;	/* address space lock */
	/* generic fields for handling share groups */
	struct proc	*s_orig;	/* creater (or NULL) */
	struct proc	*s_plink;	/* link to processes in share group */
	struct pmap	*s_pmap;	/* page map pointer */
	unsigned int	s_flag;		/* flags */
	lock_t		s_listlock;	/* protects s_plink */
	ushort		s_refcnt;	/* # process in list */
	/* semaphore for single threading open directory updating */
	short		s_fwaiting;	/* # waiting for inuse descriptors */
	mutex_t		s_fupdsema;	/* wait for opening directory */
	mrlock_t	s_fsync;	/* syncing file descriptors */
	sema_t		s_fwait;	/* wait for inuse descriptors */
	int		s_nofiles;	/* number of open files */
	lock_t		s_fwaitlock;	/* protect s_fwaiting */
	/* lock for updating misc things that don't need a semaphore */
	lock_t		s_rupdlock;	/* update lock */
	struct ufchunk	*s_flist;	/* open file descriptors and flags */
	struct vnode	*s_cdir;	/* current directory */
	struct vnode	*s_rdir;	/* root directory */
	/* hold values for other sharing options */
	unchar		s_sched;	/* scheduling mode of share gp */
	short		s_cmask;	/* mask for file creation */
	off_t		s_limit;	/* maximum write address */
	cred_t		*s_cred;	/* shared ID's */
	pgno_t		s_nlockpg;	/* # locked pages		*/
	/* share group scheduling information (protected by s_rupdlock) */
	struct proc	*s_master;	/* proc that runs when in master mode */
	mutex_t		s_brksema;	/* lock to serialize sbrk calls */
	caddr_t		s_brkbase;	/* base addr of heap for brk/sbrk */
	size_t		s_brksize;	/* size of heap */
	caddr_t		s_lastspbase;	/* hint for stack allocation */
	/* fields for isolated sproc support */
	cpumask_t	s_isomask;	/* mask of isolated cpus in use */
					/* by share group */
	/* mutex for single threading parts of detachshaddr() */
	mutex_t		s_detachsem;
	/* fields to manage pthread exit sequencing */
	struct proc	*s_spid;	/* pthread leader */
	sv_t		s_detached;	/* wait for last detach */
	int		s_why;		/* exit status */
	int		s_what;		/* exit status */
#ifdef _VM_WS_SWAPPING
	ws_proc_t	s_ws;		/* working set swapping variables */
#endif
#ifdef _SHAREII
	struct kklnode_t *s_lnodeP;	/* Pointer to controlling lnode */
#endif
	lock_t		s_kobjlock;
	kobj_t		*s_kobj[MAX_MODULES];
} shaddr_t;


/* shaddr_t s_flag values */

#define	SHDF_DATLOCK	0x0002		/* data space has been locked */
#define	SHDF_SPIDEXIT	0x0004		/* spid group exitting */

#endif

#ifdef _KERNEL
/* types for grabbing aspacelock */
#define SH_ACC		MR_ACCESS
#define SH_UPD		MR_UPDATE

#define IS_LCKUPD(p)		isaspacelock(p, SH_UPD)
#define IS_LCKACC(p)		isaspacelock(p, SH_ACC)
#define shdlock(p,t)		aspacelock(p,t)
#define shdunlock(p)		aspaceunlock(p)
#define cshdlock(p,t)		caspacelock(p,t)

/* lock and unlock shared directory descriptor updating */
#define shddlock()	{ if (curprocp->p_shaddr) sharedd_lock(); }
#define shddunlock()	{ if (curprocp->p_shaddr) sharedd_unlock(); }
#define IS_DLCK(p)	(mutex_owner(&(p)->p_shaddr->s_fupdsema) == curprocp)
#define ISSHDFD(p, sa)	(sa && (((p)->p_shmask & PR_SFDS) != 0))

/* flags for detachshaddr */
#define SHDEXEC		0x0001	/* process is execing */
#define SHDEXIT		0x0002	/* process is exitting */

struct user;
extern int allocshaddr(proc_t *, uint);
extern void attachshaddr(proc_t *, proc_t *);
extern void detachshaddr(int, int *, int *);
extern void sharedd_lock(void);
extern void sharedd_unlock(void);
extern void setshdsync(shaddr_t *, int, int);
extern void dosync(int);
extern void aspacelock(proc_t *, int);
extern int caspacelock(proc_t *, int);
extern void aspaceunlock(proc_t *);
extern int isaspacelock(proc_t *, int);
#endif

#if defined(_KERNEL) || defined(_KMEMUSER)
typedef struct pgroup_s {
        pid_t   	pg_pgrp;        /* name of process group leader */
	ushort		pg_flag;	/* process group status */
	ushort		pg_refcnt;	/* count of process group */
	struct proc	*pg_chain;	/* process group chain */
	struct pgroup_s *pg_next;	/* hash chain */
} pgroup_t;

#define	PGORPHANED	0x0001		/* this process detached from ctty */
#define PGINUSE		0x0002		/* this process group was sent signal*/

#endif  /* _KERNEL || _KMEMUSER */

/* session stuff now contained in "session.h" */

#ifdef _KERNEL

typedef struct uidinfo_s {
	struct uidinfo_s *ui_next;	/* uidinfo hash chain */
	struct uidinfo_s *ui_prev;	/* uidinfo hash chain */
	uid_t		ui_uid;		/* uid of uid we're keeping track of */
	int		ui_cnt;		/* count of procs with this uid */
} uidinfo_t;

extern void dropuidinforef(uidinfo_t *);
extern uidinfo_t *takeuidref(uid_t);

#define changeuidinfo() \
	if (curprocp->p_cred->cr_ruid != curprocp->p_uidinfo->ui_uid) { \
		dropuidinforef(curprocp->p_uidinfo); \
		curprocp->p_uidinfo = takeuidref(curprocp->p_cred->cr_ruid); \
	} \

enum jobcontrol {
        JCTLREAD,	                 /* read data on a ctty */
        JCTLWRITE,	                 /* write data to a ctty */
        JCTLGST		                 /* get ctty parameters */
};

extern mutex_t	pglobal;		/* global process group mutex */
extern lock_t	pgchainlck;		/* global process chain lock */

struct vnode;
typedef int (*pgrpscanfunc_t)(proc_t *, long);
extern int pgrpscan(pid_t, pgrpscanfunc_t, long);
extern void detachildren(proc_t *);
extern void joinpg(proc_t *, pid_t, int);
extern void leavepg(proc_t *, int);
extern pgroup_t *findpg(pid_t);
extern int pgrpverify(proc_t *, pid_t);
extern int memberspg(proc_t *, pid_t);

typedef unsigned char tlbpid_t;
typedef unsigned char icachepid_t;

/* process tlb routines */
extern void new_tlbpid(proc_t *, int);
#if TFP
extern void new_icachepid(proc_t *, int);
extern void icacheinfo_init(void);
extern int icachepid_is_usable(proc_t *);
extern void icachepid_use(proc_t *, tlbpid_t);
#endif
extern void newptes(proc_t *, caddr_t, uint, uint);
extern int tlbsync(proc_t *, int);
extern void tlbdirty(proc_t *);
extern void tlbclean(proc_t *, __psunsigned_t, cpumask_t);
extern void set_tlbpids(proc_t *, unsigned char);
extern void tlbinfo_init(void);
extern int tlbpid_is_usable(proc_t *);
extern void tlbpid_use(proc_t *, tlbpid_t);
extern void tlb_onesec_actions(void);
extern void tlb_tick_actions(void);
#ifdef ULI
tlbpid_t alloc_private_tlbpid(struct proc *);
void free_private_tlbpid(tlbpid_t);
#ifdef TFP
icachepid_t alloc_private_icachepid(struct proc *);
void free_private_icachepid(icachepid_t);
#endif
#endif

struct cred;
extern int hasprocperm(proc_t *, struct cred *);

#endif /* _KERNEL */

#if defined(_KERNEL) || defined(_KMEMUSER)
#include <sys/signal.h>			/* for NUMSIGS */
typedef struct sigvec_s {
	uint		sv_flags;	/* SNOCLDSTOP, SNOWAIT and lock */
	uint		sv_refcnt;	/* reference count */
	k_sigset_t	sv_sig;		/* set of pending async signals */
	void		(*sv_hndlr[NUMSIGS])();	/* signal dispositions */
	k_sigset_t	sv_sigmasks[NUMSIGS];	/* mask during handlers */
	k_sigset_t	sv_sigign;	/* signals to be ignored */
	k_sigset_t	sv_sigcatch;	/* signals to be caught */
	k_sigset_t	sv_sigrestart;	/* signals to restart sys calls */
	k_sigset_t	sv_signodefer;	/* signals deferred when caught */
	k_sigset_t	sv_sigresethand;/* signals reset when caught */
	k_sigset_t	sv_sainfo;	/* signals delivered w/ siginfo */
	struct sigqueue	*sv_sigqueue;	/* queued siginfo structures */
	struct sigqueue	*sv_sigqfree;	/* free list of siginfo structures */
} sigvec_t;

/* Flags for sv_flags */

#define SIGVEC_LOCK	0x00000001	/* Bit used for structure lock */
#define SNOCLDSTOP	0x00000002	/* SIGCHLD not sent when child stops */
#define SNOWAIT		0x00000004	/* children never become zombies */

#define sigvec_lock(sv)		mutex_bitlock(&(sv)->sv_flags, SIGVEC_LOCK)
#define sigvec_unlock(sv, rv)	mutex_bitunlock(&(sv)->sv_flags,SIGVEC_LOCK,rv)
#define sigvec_is_locked(sv) 	bitlock_islocked(&(sv)->sv_flags, SIGVEC_LOCK)
#define nested_sigvec_lock(sv)	nested_bitlock(&(sv)->sv_flags, SIGVEC_LOCK)
#define nested_sigvec_unlock(sv) nested_bitunlock(&(sv)->sv_flags, SIGVEC_LOCK)
#endif /* _KERNEL || _KMEMUSER */

#endif /* __SYS_PROC_H__ */
