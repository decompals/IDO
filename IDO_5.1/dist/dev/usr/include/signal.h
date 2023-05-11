#ifndef __SIGNAL_H__
#define __SIGNAL_H__
#ifdef __cplusplus
extern "C" {
#endif
#ident "$Revision: 1.30 $"
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
/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <sys/signal.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
typedef int 	sig_atomic_t;

extern char *_sys_siglist[];
extern int _sys_nsig;

#if defined(_MODERN_C)

extern int raise(int);

#if defined(_SVR4_SOURCE) || defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE)
#include <sys/types.h>
#if !defined(_BSD_COMPAT) && !defined(_BSD_SIGNALS) 
extern int kill(pid_t, int);
extern int sigpause(int);
extern int sighold(int);
extern int sigrelse(int);
#if defined(_LANGUAGE_C_PLUS_PLUS) 
extern void (*sigset(int, void (*)(...)))(...);
extern int (*ssignal(int, int (*)(...)))(...);
#else
extern void (*sigset(int, void (*)()))();
extern int (*ssignal(int, int (*)()))();
#endif
#endif /* !BSD */

extern int sigaction(int, const struct sigaction *, struct sigaction *);
extern int sigpending(sigset_t *);
extern int sigprocmask(int, const sigset_t *, sigset_t *);
extern int sigsuspend(const sigset_t *);
extern int sigaddset(sigset_t *, int);
extern int sigdelset(sigset_t *, int);
extern int sigemptyset(sigset_t *);
extern int sigfillset(sigset_t *);
extern int sigismember(const sigset_t *, int);

#endif	/* _SVR4_SOURCE... */

#if defined(_SVR4_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE)
#include <sys/procset.h>
extern int gsignal(int);
extern int sighold(int);
extern int sigrelse(int);
extern int sigignore(int);
extern int sigaltstack(const stack_t *, stack_t *);
extern int sigsend(idtype_t, id_t, int);
extern int sigsendset(const procset_t *, int);
#endif

#if defined(_POSIX_4SOURCE) || defined(_SGI_SOURCE)
extern struct timespec;
extern struct siginfo;
extern int sigwaitrt(const sigset_t *set, struct siginfo *value);
extern int sigqueue(pid_t , int, const union sigval);
extern int sigtimedwait(const sigset_t *set, struct siginfo *value, struct timespec *ts);
#endif /* _POSIX_4SOURCE || _SGI_SOURCE */

#if defined(_SGI_SOURCE) && !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE)
/*
 * The signal-masking routine sgi_altersigs() contains an action 
 * parameter; the routine can either add or delete a set of signals.
 */
#define ADDSIGS         1
#define DELSIGS         2

/*
 * The following macro returns true if any of the signals in the sigset_t
 * pointed to by "setptr" are set.
 */
#define sgi_siganyset(setptr)   (sgi_sigffset(setptr,0) != 0)

extern int      sgi_altersigs(int,sigset_t *,int[]);
extern int      sgi_sigffset(sigset_t *,int);
extern int      sgi_dumpset(sigset_t *);

#endif 	/* _SGI_SOURCE */

#else	/* _MODERN_C */

extern  void(*sigset())();

#endif 	/* _MODERN_C */
#endif  /* _LANGUAGE_C */
#ifdef __cplusplus
}
#endif
#endif /* !__SIGNAL_H__ */
