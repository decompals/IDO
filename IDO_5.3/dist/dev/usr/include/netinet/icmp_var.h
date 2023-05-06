/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)icmp_var.h	7.3 (Berkeley) 12/7/87
 */

/*
 * SGI: The statistics formerly in this file are now in sys/tcpipstats.h
 */
#ifdef _KERNEL

struct mbuf;
struct ifnet;
struct in_addr;
extern void icmp_error (struct mbuf *, int, int, struct ifnet *,
			struct in_addr, struct ifnet *);
#endif
