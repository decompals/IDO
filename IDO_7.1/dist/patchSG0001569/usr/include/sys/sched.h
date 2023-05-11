/**************************************************************************
 *									  *
 * 		 Copyright (C) 1996 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

#ifndef _SYS_SCHED_H_
#define _SYS_SCHED_H_

/*
 * POSIX Scheduling Header File.
 */


/*
 * Scheduling Policies
 */
#define SCHED_FIFO	1
#define SCHED_RR	2
#define SCHED_OTHER	3

/*
 * Priority Ranges
 */
#define	PX_PRIO_MIN	40
#define PX_PRIO_MAX	127
#define	PX_ADJUST	PX_PRIO_MIN+PX_PRIO_MAX

#define	set_posix_pri(pri)	(PX_ADJUST-(pri))

/*
 * Scheduling Parameter Structure
 */
struct sched_param {
	int sched_priority;
};

#endif
