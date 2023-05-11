#ifndef __SYS_PTIMERS_H__
#define __SYS_PTIMERS_H__
#ifdef __cplusplus
extern "C" {
#endif

#include <sys/signal.h>

#ifndef _TIMESPEC_T
#define _TIMESPEC_T
typedef struct timespec {
	time_t	tv_sec;		/* seconds */
	long	tv_nsec;	/* and nanoseconds */
} timespec_t;
#endif	/* _TIMESPEC_T */

typedef struct itimerspec {
	struct timespec it_interval;	/* timer period */
	struct timespec it_value;	/* timer expiration */
} itimerspec_t;
/*
 * structure to represent posix timers in the proc structure.
 */
#define _SGI_POSIX_TIMER_MAX 32		/* Number of posix timers per proc */

typedef struct ptimer_info {
	__int64_t	next_timeout;	/* Time/timeto next timeout */
    	__int64_t 	interval_tick;	/* Number of ticks in interval */
	int 		signo;		/* Signal to send */
	union sigval 	value;		/* Value to pass to signal */
	int 		clock_type;	/* What clock to use, 0 is unused */
    	toid_t		next_toid;	/* id of next timeout */
    	int		overrun_cnt;	/* count of extra timer expirations */
} ptimer_info_t;
/*
 * Clock definitions used by above POSIX clock functions
 */
#define CLOCK_REALTIME 	1		/* The time of day clock (POSIX) */
/* These are special SGI clocks, not standard to POSIX  */
#define CLOCK_SGI_CYCLE 2		/* Free running hardware counter */
#define CLOCK_SGI_FAST	3		/* Clock used to set fast timers */
/*
 * Flags used in timer_settime()
 */
#define TIMER_ABSTIME	0x00000001	/* Flag that time is absolute */

#ifdef __cplusplus
}
#endif
#endif /* !__SYS_PTIMERS_H__ */
