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
#ident "$Revision: 3.11 $"

/*
 * The networking code runs off software interrupts.
 *
 * You can switch into the network by doing splnet() and return by splx().
 * The software interrupt level for the network is higher than the software
 * level for the clock (so you can enter the network in routines called
 * at timeout time).
 */

/*
 * Each ``pup-level-1'' input queue has a bit in a ``netisr'' status
 * word which is used to de-multiplex a single software
 * interrupt used for scheduling the network code to calls
 * on the lowest level routine of each protocol.
 */
#define	NETISR_IP	0		/* same as AF_INET */
#define	NETISR_RAW	1		/* same as AF_UNSPEC */
#define NETISR_RUNNING	31		/* network code is running */
					/* same as unused AF_IMPLINK */

#ifdef _KERNEL

#define SCHEDBIT(anisr)		(1<<(anisr))

/* This has to avoid MP races.  We want to cause a soft interrupt the first
 *	time a bit is set.  We want to avoid causing an interrupt otherwise
 *	so that we do not uselessly interrupt other CPUs.
 */
#define	schednetisr(anisr) {			\
	int isr_s = splimp();			\
	unsigned int isr_n = netisr;		\
	if ((isr_n & SCHEDBIT(anisr)) == 0) {	\
		netisr |= SCHEDBIT(anisr);	\
		if (!isr_n)			\
			if (netproc_mode)	\
				netproc_intr();	\
			else			\
				setsoftnet();	\
	}					\
	splx(isr_s);				\
}

extern u_int	netisr;			/* scheduling bits for network */
extern int	netproc_mode;		/* if set, one packet per ipintr call */
extern void	netproc_intr(void);	/* wake up the netproc daemon */

typedef struct {
	int (*func)(void);
} netisr_tab_t;

extern netisr_tab_t netisr_tab[];

int  register_isr(int (*)(void));
void unregister_isr(int);

#endif /* _KERNEL */
