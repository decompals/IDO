/* Copyright (C) 1989-1992 Silicon Graphics, Inc. All rights reserved.  */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef _SYS_SIGNAL_H
#define _SYS_SIGNAL_H

#ifdef __cplusplus
extern "C" {
#endif
#include <standards.h>
#ident	"$Revision: 3.130 $"

/* ANSI C Notes:
 *
 * - THE IDENTIFIERS APPEARING OUTSIDE OF #ifdef's IN THIS
 *   standard header ARE SPECIFIED BY ANSI!  CONFORMANCE WILL BE ALTERED
 *   IF ANY NEW IDENTIFIERS ARE ADDED TO THIS AREA UNLESS THEY ARE IN ANSI's
 *   RESERVED NAMESPACE. (i.e., unless they are prefixed by __[a-z] or
 *   _[A-Z].  For external objects, identifiers with the prefix _[a-z]
 *   are also reserved.)
 *
 * - Section 4.7 indicates that identifiers beginning with SIG or SIG_
 *   followed by an upper-case letter are added to the reserved namespace
 *   when including <signal.h>.
 * POSIX Notes:
 *	Alas, POSIX permits SIG_ but not SIG
 */

/*
 * Signal Numbers.
 */
#define	SIGHUP	1	/* hangup */
#define	SIGINT	2	/* interrupt (rubout) */
#define	SIGQUIT	3	/* quit (ASCII FS) */
#define	SIGILL	4	/* illegal instruction (not reset when caught)*/
#if _NO_POSIX
#define	SIGTRAP	5	/* trace trap (not reset when caught) */
#define	SIGIOT	6	/* IOT instruction */
#endif
#define SIGABRT 6	/* used by abort, replace SIGIOT in the  future */
#if _NO_POSIX
#define	SIGEMT	7	/* EMT instruction */
#endif
#define	SIGFPE	8	/* floating point exception */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#if _POSIX93 || _XOPEN4UX
#define	SIGBUS	10	/* bus error */
#endif
#define	SIGSEGV	11	/* segmentation violation */
#define	SIGSYS	12	/* bad argument to system call */
#define	SIGPIPE	13	/* write on a pipe with no one to read it */
#define	SIGALRM	14	/* alarm clock */
#define	SIGTERM	15	/* software termination signal from kill */
#define	SIGUSR1	16	/* user defined signal 1 */
#define	SIGUSR2	17	/* user defined signal 2 */
#if _NO_POSIX
#define	SIGCLD	18	/* death of a child */
#endif
#define SIGCHLD 18	/* 4.3BSD's/POSIX name */
#if _NO_POSIX
#define	SIGPWR	19	/* power-fail restart */
#define	SIGWINCH 20	/* window size changes */
#define	SIGURG	21	/* urgent condition on IO channel */
#define SIGPOLL 22	/* pollable event occurred */
#define	SIGIO	22	/* input/output possible signal */
#endif /* _NO_POSIX */
#define	SIGSTOP	23	/* sendable stop signal not from tty */
#define	SIGTSTP	24	/* stop signal from tty */
#define	SIGCONT	25	/* continue a stopped process */
#define	SIGTTIN	26	/* to readers pgrp upon background tty read */
#define	SIGTTOU	27	/* like TTIN for output if (tp->t_local&LTOSTOP) */
#if _NO_POSIX
#define SIGVTALRM 28	/* virtual time alarm */
#define SIGPROF	29	/* profiling alarm */
#define SIGXCPU	30	/* Cpu time limit exceeded */
#define SIGXFSZ	31	/* Filesize limit exceeded */
#define	SIG32	32	/* Reserved for kernel usage */
#endif
#define	SIGCKPT	33	/* Checkpoint warning (HibernatorII) */

#if _POSIX93
/*
 * Posix 1003.1b signals
 */
#define SIGRTMIN	49	/* Posix 1003.1b signals */
#define SIGRTMAX	64	/* Posix 1003.1b signals */
#endif

#if _POSIX1C
/*
 * Signals reserved for Posix 1003.1c.
 */
#define SIGPTINTR	47
#define SIGPTRESCHED	48
#endif

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/*
 * Basic defines and types. Ok for ANSI
 */
#if defined(_LANGUAGE_C_PLUS_PLUS) || !_SGIAPI
#define __sigargs	int
#else
#define __sigargs	
#endif /* _LANGUAGE_C_PLUS_PLUS) || !_SGIAPI */

/*
 * Almost everyone has gone to a void return type. This lets
 * old programs define this and get the old 'int' behviour
 */
#ifndef __sigret_t
#define	__sigret_t	void
#endif

typedef __sigret_t	(*SIG_PF) (__sigargs);

#define SIG_ERR		((SIG_PF)-1L)
#define	SIG_IGN		((SIG_PF)1L)
#if _POSIX90
#define SIG_HOLD	((SIG_PF)2L)
#endif
#define	SIG_DFL		((SIG_PF)0L)

#if _NO_ANSIMODE || defined(_BSD_COMPAT) || defined(_BSD_SIGNALS)
/* include types.h unless in ANSI mode unless they ask for BSD */
#include <sys/types.h>
#endif

#if _POSIX93 && _NO_ANSIMODE
/*
 * POSIX 1003.1b extensions
 */
#if (_MIPS_SZPTR == 32)
typedef union sigval {
	int	sival_int;
	void	*sival_ptr;
} sigval_t;
#endif
#if (_MIPS_SZPTR == 64)
typedef union sigval {
	long	sival_int;
	void	*sival_ptr;
} sigval_t;
#endif

/* XXX Not POSIX93 compliant (nisigno) */
typedef union notifyinfo {
	int	nisigno;			/* signal info */
	void	(*nifunc) (sigval_t);	 	/* callback data */
} notifyinfo_t;

typedef struct sigevent {
	int			sigev_notify;
	notifyinfo_t		sigev_notifyinfo;
	sigval_t		sigev_value;
	unsigned long		sigev_reserved[13];
	unsigned long		sigev_pad[6];
} sigevent_t;
#define sigev_func	sigev_notifyinfo.nifunc
#define sigev_signo	sigev_notifyinfo.nisigno

#define SIGEV_NONE	128
#define SIGEV_SIGNAL	129
#define SIGEV_CALLBACK	130

#endif /* _POSIX93 */

#if (_POSIX93 || _XOPEN4UX) && _NO_ANSIMODE
/*
 * POSIX 1003.1b / XPG4-UX extension for signal handler arguments
 * Note the double '_' - this is for XPG/POSIX symbol space rules. This
 * is different than MIPS ABI, but applications shouldn't be using anything
 * but the si_* names.
 */
#include <sys/siginfo.h>

#define SI_MAXSZ	128
#define SI_PAD		((SI_MAXSZ / sizeof(__int32_t)) - 3)

/*
 * ABI version of siginfo
 * Some elements change size in the 64 bit environment
 */
typedef struct siginfo {
	int	si_signo;		/* signal from signal.h	*/
	int 	si_code;		/* code from above	*/
	int	si_errno;		/* error from errno.h	*/
	union {

		int	si_pad[SI_PAD];	/* for future growth	*/

		struct {			/* kill(), SIGCLD	*/
			pid_t	__pid;		/* process ID		*/
			union {
				struct {
					uid_t	__uid;
				} __kill;
				struct {
					clock_t __utime;
					int __status;
					clock_t __stime;
				} __cld;
			} __pdata;
		} __proc;			

		struct {	/* SIGSEGV, SIGBUS, SIGILL and SIGFPE	*/
			void 	*__addr;	/* faulting address	*/
		} __fault;

		struct {			/* SIGPOLL, SIGXFSZ	*/
		/* fd not currently available for SIGPOLL */
			int	__fd;	/* file descriptor	*/
			long	__band;
		} __file;
#if _POSIX93
		union sigval	__value;
#define si_value	__data.__value
#endif

	} __data;

} siginfo_t;

#define si_pid		__data.__proc.__pid
#define si_status	__data.__proc.__pdata.__cld.__status
#define si_stime	__data.__proc.__pdata.__cld.__stime
#define si_utime	__data.__proc.__pdata.__cld.__utime
#define si_uid		__data.__proc.__pdata.__kill.__uid
#define si_addr		__data.__fault.__addr
#define si_fd		__data.__file.__fd
#define si_band		__data.__file.__band

/* for si_code, things that we use now */
#define SI_USER		0	/* user generated signal */
#define SI_KILL		SI_USER		/* kill system call */
#define SI_QUEUE	-1		/* sigqueue system call */
#define SI_ASYNCIO	-2		/* posix 1003.1b aio */
#define SI_TIMER	-3		/* posix 1003.1b timers */
#define SI_MESGQ	-4		/* posix 1003.1b messages */

#endif /* _POSIX93 || _XOPEN4UX && !ANSI */

#if _NO_ANSIMODE || defined(_BSD_COMPAT) || defined(_BSD_SIGNALS)
/*
 * POSIX90 added types (all except for ANSI)
 */
typedef struct {                /* signal set type */
        __uint32_t __sigbits[4];
} sigset_t;

/* these aren't technically in POSIX90, but are permitted.. */
typedef union __sighandler {
    	__sigret_t (*__sa_handler)(__sigargs); /* SIG_DFL, SIG_IGN, or *fn */
#if _POSIX93 && _NO_ANSIMODE
    	void (*__sa_sigaction)(int, siginfo_t *, void *);
#endif
} __sighandler_t;

typedef struct sigaction {
	int sa_flags;			/* see below for values		*/
    	__sighandler_t sa_sighandler;	/* function to handle signal */
	sigset_t sa_mask;		/* additional set of sigs to be	*/
					/* blocked during handler execution */
	int sa_resv[2];
} sigaction_t;
/*
 * Posix defined two types of handlers. sa_handler is the default type
 * for use by programs that are not requesting SA_SIGINFO.  Programs
 * requesting SA_SIGINFO need to use a handler of type sa_sigaction.
 */
#define sa_handler	sa_sighandler.__sa_handler
#define sa_sigaction	sa_sighandler.__sa_sigaction

/*
 * Definitions for the "how" parameter to sigprocmask():
 *
 * The parameter specifies whether the bits in the incoming mask are to be
 * added to the presently-active set for the process, removed from the set,
 * or replace the active set.
 */
#define SIG_NOP		0	/* Not using 0 will catch some user errors. */
#define SIG_BLOCK	1
#define SIG_UNBLOCK	2	
#define SIG_SETMASK	3
#define SIG_SETMASK32	256	/* SGI added so that BSD sigsetmask won't 
				   affect the upper 32 sigal set */

/* definitions for the sa_flags field */
/*
 * IRIX5/SVR4 ABI definitions
 */
#define SA_ONSTACK	0x00000001	/* handle this signal on sigstack */
#define SA_RESETHAND    0x00000002	/* reset handler */
#define SA_RESTART      0x00000004	/* restart interrupted system call */
#define SA_SIGINFO      0x00000008	/* provide siginfo to handler */
#define SA_NODEFER      0x00000010	/* do not block current signal */
/* The next 2 are only meaningful for SIGCHLD */
#define SA_NOCLDWAIT    0x00010000	/* don't save zombie children */
#define SA_NOCLDSTOP	0x00020000	/* if set don't send SIGCLD	*/
					/* to parent when child stop	*/

/* IRIX5 additions */
#define _SA_BSDCALL	0x10000000	/* don't scan for dead children when */
					/* setting SIGCHLD */
					/* SJCTRL bit in proc struct.	*/

#endif /* _NO_ANSIMODE || BSD */

#if (_XOPEN4UX && _NO_ANSIMODE) || defined(_BSD_COMPAT) || defined(_BSD_SIGNALS)
/*
 * Structure used in BSD sigstack call and in X/Open XPG4.
 */
struct sigstack {
	char	*ss_sp;			/* signal stack pointer */
	int	ss_onstack;		/* current status */
};

/* sigaltstack info */
#define MINSIGSTKSZ	512
#define SIGSTKSZ	8192

typedef struct sigaltstack {
	char	*ss_sp;
	int	ss_size;
	int	ss_flags;
} stack_t;

/* defines for ss_flags */
#define SS_ONSTACK	0x00000001
#define SS_DISABLE	0x00000002

#include <sys/ucontext.h>
#endif	/* _XOPEN4UX */

#if defined(_BSD_COMPAT) || defined(_BSD_SIGNALS)
/*
 * The next section contains declarations and typedefs for the BSD signal
 * library routines built on top of the POSIX system calls.  Two of them,
 * signal() and sigpause(), share names with their SysV counterparts, yet
 * have different semantics.  By default, the SysV versions are used.
 * In order to use the BSD versions, you may either:
 *  1) explicitly call BSDsignal() and BSDsigpause() in the program, or
 *  2) set one of the _BSD_SIGNALS or _BSD_COMPAT cpp constants before
 *     including the signal header file. There are 2 methods:
 *     a) modify the source file, e.g.,
 *	    #ifdef sgi
 *          #define _BSD_SIGNALS
 *          #endif
 *          #include <signal.h>
 *     b) use the cc(1) option -D_BSD_SIGNALS or -D_BSD_COMPAT.
 *        e.g., cc -D_BSD_SIGNALS foo.c -o foo
 * Note that _BSD_COMPAT affects other header files that deal with BSD
 * compatibility.
 */

struct sigvec {
  __sigret_t (*sv_handler)(__sigargs);	/* SIG_DFL, SIG_IGN, or *fn */
  int sv_mask;		/* mask to use during handler execution	*/
  int sv_flags;		/* see signal options below */
};


#define SV_ONSTACK	0x0001
#define SV_INTERRUPT	0x0002		/* not supported */
#define sv_onstack	sv_flags	/* compatibility with BSD */

#define NUMBSDSIGS	(32)  /* can't be expanded */
/* Convert signal number to bit mask representation */
#define sigmask(sig)	(1L << ((sig)-1))

#define signal		BSDsignal
#define sigpause	BSDsigpause

extern int	sigpause(int);
extern int	sigvec(int,struct sigvec *, struct sigvec *);
extern int	sigstack(struct sigstack *, struct sigstack *);
extern int	sigblock(int);
extern int	sigsetmask(int);
extern int	killpg(pid_t, int);
extern int	kill(pid_t, int);
#endif /* BSD */

#ifndef _KERNEL
#if _XOPEN4UX && _NO_ANSIMODE && (!defined(_BSD_COMPAT) && !defined(_BSD_SIGNALS))
/*
 * XPG4-UX adds a few BSD things
 */
#define sigmask(sig)	(1L << ((sig)-1))
extern void	(*bsd_signal(int, void (*)(int)))(int);
extern int	killpg(pid_t, int);
extern int	sigstack(struct sigstack *, struct sigstack *);

#endif	/* _XOPEN4UX */
#endif	/* !_KERNEL */

#ifndef _KERNEL
extern __sigret_t	(*signal(int,__sigret_t (*)(__sigargs)))(__sigargs);
#endif

#if _SGIAPI
/*
 * Information pushed on stack when a signal is delivered. This is used by
 * the kernel to restore state following execution of the signal handler.
 * It is also made available to the handler to allow it to properly restore
 * state if a non-standard exit is performed.
 *
 * sc_regmask is examined by the kernel when doing sigreturn()'s
 * and indicates which registers to restore from sc_regs
 * bit 0 == 1 indicates that all coprocessor state should be restored
 *	for each coprocessor that has been used
 * bits 1 - 31 == 1 indicate registers 1 to 31 should be restored by
 *	sigcleanup from sc_regs.
 */

/*
 * The IRIX5 version
 * sigcontext is not part of the ABI - so this version is used to
 * handle 32 and 64 bit applications - it is a constant size regardless
 * of compilation mode, and always returns 64 bit register values
 */
typedef struct sigcontext {
	__uint32_t	sc_regmask;	/* regs to restore in sigcleanup */
	__uint32_t	sc_status;	/* cp0 status register */
	__uint64_t	sc_pc;		/* pc at time of signal */
	/*
	 * General purpose registers
	 */
	__uint64_t	sc_regs[32];	/* processor regs 0 to 31 */
	/*
	 * Floating point coprocessor state
	 */
	__uint64_t	sc_fpregs[32];	/* fp regs 0 to 31 */
	__uint32_t	sc_ownedfp;	/* fp has been used */
	__uint32_t	sc_fpc_csr;	/* fpu control and status reg */
	__uint32_t	sc_fpc_eir;	/* fpu exception instruction reg */
					/* implementation/revision */
	__uint32_t	sc_ssflags;	/* signal stack state to restore */
	__uint64_t	sc_mdhi;	/* Multiplier hi and low regs */
	__uint64_t	sc_mdlo;
	/*
	 * System coprocessor registers at time of signal
	 */
	__uint64_t	sc_cause;	/* cp0 cause register */
	__uint64_t	sc_badvaddr;	/* cp0 bad virtual address */
	__uint64_t	sc_triggersave;	/* state of graphics trigger (SGI) */
	sigset_t	sc_sigset;	/* signal mask to restore */
	__uint64_t	sc_fp_rounded_result;	/* for Ieee 754 support */
	__uint64_t	sc_pancake[5];
	__uint64_t	sc_pad[26];
} sigcontext_t;

#if !defined(_KERNEL) && !defined(_KMEMUSER)
/* minor compatibility - sc_mask is the first 32 bits (for BSD sigsetmask) */
#define sc_mask	sc_sigset.__sigbits[0]
#endif

#ifdef _LINT
#undef SIG_ERR
#define SIG_ERR (void(*)())0
#undef SIG_IGN
#define	SIG_IGN	(void (*)())0
#undef SIG_HOLD
#define	SIG_HOLD (void (*)())0
#endif /* _LINT */

#endif /* _SGIAPI */

#else /* !(_LANGUAGE_C || _LANGUAGE_C_PLUS_PLUS) */

/* Define these for ASSEMBLY and FORTRAN */
#define SIG_ERR         (-1)
#define SIG_IGN         (1)
#define SIG_HOLD        (2)
#define SIG_DFL         (0)

#endif /* !(_LANGUAGE_C || _LANGUAGE_C_PLUS_PLUS) */

#if _SGIAPI
#define NSIG            65      /* valid signal numbers are from 1 to NSIG-1 */
#define MAXSIG		(NSIG-1)    /* actual # of signals */
#define	NUMSIGS		(NSIG-1)    /* for POSIX array sizes, true # of sigs */

#define	BRK_USERBP	0	/* user bp (used by debuggers) */
#define	BRK_KERNELBP	1	/* kernel bp (used by prom) */
#define	BRK_ABORT	2	/* abort(3) uses to cause SIGIOT */
#define	BRK_BD_TAKEN	3	/* for taken bd emulation */
#define	BRK_BD_NOTTAKEN	4	/* for not taken bd emulation */
#define	BRK_SSTEPBP	5	/* user bp (used by debuggers) */
#define	BRK_OVERFLOW	6	/* overflow check */
#define	BRK_DIVZERO	7	/* divide by zero check */
#define	BRK_RANGE	8	/* range error check */

#define BRK_PSEUDO_OP_BIT 0x80
#define BRK_PSEUDO_OP_MAX 0x3	/* number of pseudo-ops */

#define BRK_CACHE_SYNC	0x80	/* synchronize icache with dcache */
#define BRK_SWASH_FLUSH	0x81	/* SWASH flush */
#define BRK_SWASH_SWTCH	0x82	/* SWASH addrspace switch */

#define	BRK_MULOVF	1023	/* multiply overflow detected */
#endif /* _SGIAPI */

#ifdef __cplusplus
}
#endif

#endif /* !_SYS_SIGNAL_H */
