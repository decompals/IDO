#ifndef __SYS_TIMERS_H__
#define __SYS_TIMERS_H__
#ifdef __cplusplus
extern "C" {
#endif
/*
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
 * rights reserved under the Copyright Lanthesaws of the United States.
 */

#ident "$Revision: 1.26 $ $Author: joecd $"


#if defined(_SGI_SOURCE)
#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
#ifndef _KERNEL
#include <time.h>
#endif /* _KERNEL */
#endif /* _LANGUAGE_C || _LANGUAGE_C_PLUS_PLUS */
#endif /* _SGI_SOURCE */


#if defined(_KERNEL) || defined(_KMEMUSER)
/*
 *	Definitions for accurate timers.  high_bits_check is a copy of
 *	high_bits that allows reader to verify that values read are
 *	ok.  In versions of IRIX up to 5.3 this structure/typedef was called
 *	timer_t. POSIX 1003.1b's inclusion of a	timer_t forced the name change.
 */
typedef struct ktimer {
	unsigned	low_bits;
	unsigned	high_bits;
	unsigned	high_bits_check;
	unsigned	tstamp;
} ktimer_t;

#define MAX_PROCTIMER	11		/* max # of per process timers */

#include <sys/ktime.h>			/* irix5_64_time_t, irix5_time_t */
#include <sys/signal.h>

#ifndef _TIMESPEC_T
#define _TIMESPEC_T
typedef struct timespec {
	time_t	tv_sec;		/* seconds */
	long	tv_nsec;	/* and nanoseconds */
} timespec_t;
#endif	/* _TIMESPEC_T */

/* Irix5, 32bit ABI */
typedef struct irix5_timespec {
	irix5_time_t	tv_sec;		/* seconds */
	app32_long_t	tv_nsec;	/* and nanoseconds */
} irix5_timespec_t;
#ifndef timespec_to_timeval
#define timespec_to_timeval(tval, tspec) \
      (tval)->tv_sec = (tspec)->tv_sec; \
      (tval)->tv_usec = (tspec)->tv_nsec / 1000;

#define timeval_to_timespec(tspec, tval) \
      (tspec)->tv_sec = (tval)->tv_sec; \
      (tspec)->tv_nsec = (tval)->tv_usec * 1000;
#endif

#define TIMER_MAX	0xffffffff	/* assumed 32 bits timer for now */

#endif /* _KERNEL || _KMEMUSER */

#ifdef _KERNEL

/*
 * Machine dependent setting based on hardware support.
 */
extern unsigned timer_freq;		/* freq of timestamping source */
extern unsigned timer_unit;		/* duration in nsec of timer tick */
extern unsigned timer_high_unit;	/* number of low bits in a high_bit */
extern unsigned timer_maxsec;		/* duration in sec before wrapping */
extern unsigned *timer_addr;		/* pointer to timestamping source */
					/* not used by R4000 machines */
/*
 * get_timestamp()
 * This returns the value of the timestamping source.
 */
#if R4000 || TFP || R10000
#define	get_timestamp()	_get_timestamp()
extern time_t _get_timestamp(void);
#endif

/*
 *	save structure for timer readings.  This is used to save timer
 *	readings for elapsed time computations.
 */
typedef struct timer_save {
	unsigned	low;
	unsigned	high;
} timer_save_t;

/*
 *	TIMER_GRAB() is a macro to retrieve the value of a timer.
 * 	This allows single writer, multiple readers w/o using lock.
 */

#define TIMER_GRAB(timer, save)						\
	do {								\
		(save)->high = (timer)->high_bits;			\
		(save)->low = (timer)->low_bits;			\
	/*								\
	 *	If the timer was normalized while we were doing this,	\
	 *	the high_bits value read above and the high_bits check	\
	 *	value won't match because high_bits_check is the first	\
	 *	field touched by the normalization procedure, and	\
	 *	high_bits is the last.					\
	 *
	 *	Additions to timer only touch low bits and 		\
	 *	are therefore atomic with respect to this.		\
	 */								\
	} while ( (save)->high != (timer)->high_bits_check);		

/*
 *	Convert a kernel timer value into an accum_t, which for timers
 *	amounts to a 64-bit value in units of nanoseconds. This is assumed
 *	to be done at exit time or when the timer is otherwise guaranteed
 *	to be unchanging, so the ktimer_t is used directly (i.e. without
 *	saving it via TIMER_GRAB). 
 */
#define TIMER_TO_ACCUM(timer)						      \
		((NSEC_PER_SEC * ((accum_t)(timer)->high_bits +		      \
				  ((accum_t)(timer)->low_bits / timer_freq))) \
		 + (((accum_t)(timer)->low_bits % timer_freq) * timer_unit))

/*
 *	Same as TIMER_TO_ACCUM but uses a specified kernel timer from
 *	p_timertab in a specified proc.
 */
#define PROCTIMER_TO_ACCUM(p,tmr)	\
		TIMER_TO_ACCUM(&p->p_timertab[PTIMER_INDEX(tmr)])

/*
 *	Same as TIMER_TO_ACCUM, but use a timer_save_t instead of a ktimer_t.
 *	Useful when dealing with a potentially active timer.
 */
#define TIMERSV_TO_ACCUM(timersv)					   \
		((NSEC_PER_SEC * ((accum_t)(timersv)->high +		   \
				  ((accum_t)(timersv)->low / timer_freq))) \
		 + (((accum_t)(timersv)->low % timer_freq) * timer_unit))

/*
 *	Mask to check if low_bits is in danger of overflowing
 */

#define	TIMER_LOW_FULL	0x80000000

/*
 *	Kernel timers array.  [Exported]
 */

extern ktimer_t	kernel_timer[];
extern ktimer_t	idle_timer[];

/* Per process accounting states timers */
#define	AS_USR_RUN	1		/* running in user mode */
#define	AS_SYS_RUN	2		/* running in system mode */
#define	AS_GFX_RUN	3	 	/* XXX running in system mode for gfx */
#define	AS_BIO_WAIT	4	 	/* wait for block I/O */
#define AS_MEM_WAIT	5		/* waiting for memory */
#define	AS_SELECT_WAIT	6	 	/* wait in select */
#define	AS_JCL_WAIT	7	 	/* stop because of job control */
#define	AS_RUNQ_WAIT	8	 	/* ready to run but still on run queue */ 
#define	AS_SLEEP_WAIT	9	 	/* sleep waiting for resource */
#define AS_STRMON_WAIT	10		/* sleep for the stream monitor */
#define	AS_PHYSIO_WAIT	11	 	/* wait for raw I/O */
#define AS_NONE		0		/* doesn't require a timer */

#define TIMER_SHIFT(x)	((x&0xff)<<16) /* shift before OR in with pri */
#define TIMER_SHIFTBACK(x)	(((x)>>16)&0xff)
#define TIMER_MASK	0xffff
#define PTIMER_INDEX(x)	(x-1)


/*
 *	Exported kernel interface to timers
 */
void timer_init(ktimer_t *);
void start_active_timer(ktimer_t *);
struct proc;
void nonactive_timer_switch(struct proc *, int );
void usr2sys_timer_switch(unsigned);
void back2usr_timer_switch(unsigned);
void inttrap_timer_switch(unsigned);
void int2kern_timer_return(unsigned);
void active_timer_switch(ktimer_t *);
struct timestruc;
void timer_read(ktimer_t *, struct timestruc *);
void read_proc_activetimer(struct proc *, struct timestruc *, struct timestruc *);
void timer_normalize(ktimer_t *timer);

#include <sys/xlate.h>
int irix5_to_timespec(enum xlate_mode, void *, int, xlate_info_t *);
int timespec_to_irix5(void *, int, xlate_info_t *);
#endif /* defined(_KERNEL) */

#ifdef __cplusplus
}
#endif
#endif /* !__SYS_TIMERS_H__ */
