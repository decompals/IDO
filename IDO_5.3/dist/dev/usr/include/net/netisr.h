/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)netisr.h	7.4 (Berkeley) 6/27/88
 */
#ident "$Revision: 3.15 $"

/*
 * The networking code runs off software interrupts.
 *
 * You can switch into the network by doing splnet() and return by splx().
 * The software interrupt level for the network is higher than the software
 * level for the clock (so you can enter the network in routines called
 * at timeout time).
 */

/*
 * Each ``address family'' input queue has a bit in a ``netisr'' status
 * word which is used to de-multiplex a single software
 * interrupt used for scheduling the network code to calls
 * on the lowest level routine of each protocol.
 */
#define	NETISR_IP	0		/* same as AF_INET */
#define	NETISR_RAW	1		/* same as AF_UNSPEC */
#define NETISR_RUNNING	31		/* network code is running */
					/* same as unused AF_IMPLINK */

#ifdef _KERNEL
#ifdef _MP_NETLOCKS
extern lock_t netisr_lock;
#define NETISR_LOCK()		spsema(netisr_lock)
#define NETISR_UNLOCK()		svsema(netisr_lock)
#define NETISR_ISLOCKED()	(ownlock(netisr_lock))

#else

#define NETISR_LOCK()
#define NETISR_UNLOCK()
#define NETISR_ISLOCKED()	(1)
#endif /* _MP_NETLOCKS */

#define SCHEDBIT(anisr)		(1<<(anisr))

extern void schednetisr(int anisr);

extern u_int	netisr;			/* scheduling bits for network */
extern int	netproc_mode;		/* if set, one packet per ipintr call */

typedef struct {
	int (*func)(void);
} netisr_tab_t;

extern netisr_tab_t netisr_tab[];

int  register_isr(int (*)(void));
void unregister_isr(int);

#endif /* _KERNEL */
