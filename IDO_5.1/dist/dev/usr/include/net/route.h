#ifndef __net_route__
#define __net_route__
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
 *	@(#)route.h	7.4 (Berkeley) 6/27/88
 *	plus portions of @(#)route.h	7.11 (Berkeley) 6/28/90
 */

#ifdef sgi
#ident "$Revision: 4.5 $"
#include <sys/socket.h>
struct rtentry;
#endif

/*
 * Kernel resident routing tables.
 * 
 * The routing tables are initialized when interface addresses
 * are set by making entries for all directly connected interfaces.
 */

/*
 * A route consists of a destination address and a reference
 * to a routing entry.  These are often held by protocols
 * in their control blocks, e.g. inpcb.
 */
struct route {
	struct	rtentry *ro_rt;
	struct	sockaddr ro_dst;
};

struct ip_provider;
#ifdef sgi
struct ifnet;
#endif

/*
 * We distinguish between routes to hosts and routes to networks,
 * preferring the former if available.  For each route we infer
 * the interface to use from the gateway address supplied when
 * the route was entered.  Routes that forward packets through
 * gateways are marked so that the output routines know to address the
 * gateway rather than the ultimate destination.
 */
struct rtentry {
	u_long	rt_hash;		/* to speed lookups */
	struct	sockaddr rt_dst;	/* key */
	struct	sockaddr rt_gateway;	/* value */
	short	rt_flags;		/* up/down?, host/net */
	short	rt_refcnt;		/* # held references */
	u_long	rt_use;			/* raw # packets forwarded */
	union {
		struct	ip_provider *rtu_prov; /* required for MIPS ABI */
		struct	ifnet *rtu_ifp;	/* the answer: interface to use */
	} rt_u;
#define rt_prov rt_u.rtu_prov		/* for STREAMS/libsocket version */
#define rt_ifp	rt_u.rtu_ifp		/* for native sockets */
};

#define	RTF_UP		0x1		/* route useable */
#define	RTF_GATEWAY	0x2		/* destination is a gateway */
#define	RTF_HOST	0x4		/* host entry (net otherwise) */
#define RTF_REINSTATE   0x8             /* re-instate route after timeout */
#define	RTF_DYNAMIC	0x10		/* created dynamically (by redirect) */
#define	RTF_MODIFIED	0x20		/* modified dynamically (by redirect) */
#define RTF_SWITCHED    0x40            /* this route must be switched */
#define RTF_SLAVE       0x80            /* slave switched route */
#define RTF_REMOTE      0x100           /* route usedfor forwarded packets */
#define RTF_TOSWITCH    0x200           /* gateway is switched route */

/*
 * These numbers are used by reliable protocols for determining
 * retransmission behavior and are included in the routing structure.
 * MTU (max. transmission unit or max packet size) and
 * the pipe sizes and limits are given in bytes.  The rtt (round
 * trip time) and rttvar estimates are given in microseconds.  
 * (Note that this limits the average rtt to at most 35 minutes --
 * As things have been going in the Internet, this might be optimistic).
 */
struct rt_metrics {
	u_long	rmx_locks;	/* Kernel must leave these values alone */
	u_long	rmx_mtu;	/* MTU for this path */
	u_long	rmx_hopcount;	/* max hops expected */
	u_long	rmx_expire;	/* lifetime for route, e.g. redirect */
	u_long	rmx_recvpipe;	/* inbound delay-bandwith product */
	u_long	rmx_sendpipe;	/* outbound delay-bandwith product */
	u_long	rmx_ssthresh;	/* outbound gateway buffer limit */
	u_long	rmx_rtt;	/* estimated round trip time */
	u_long	rmx_rttvar;	/* estimated rtt variance */
};
#define RTV_MTU		0x1	/* init or lock _mtu */
#define RTV_HOPCOUNT	0x2	/* init or lock _hopcount */
#define RTV_EXPIRE	0x4	/* init or lock _hopcount */
#define RTV_RPIPE	0x8	/* init or lock _recvpipe */
#define RTV_SPIPE	0x10	/* init or lock _sendpipe */
#define RTV_SSTHRESH	0x20	/* init or lock _ssthresh */
#define RTV_RTT		0x40	/* init or lock _rtt */
#define RTV_RTTVAR	0x80	/* init or lock _rttvar */

/*
 * rmx_rtt and rmx_rttvar are stored as microseconds;
 * RTTTOPRHZ(rtt) converts to a value suitable for use
 * by a protocol slowtimo counter.
 */
#define	RTM_RTTUNIT	1000000	/* units for rtt, rttvar, as units per sec */
#define	RTTTOPRHZ(r)	((r) / (RTM_RTTUNIT / PR_SLOWHZ))

/*
 * horrible kluge: this info is logically part of a rtentry but we
 * stick into a separate struct so the routing ioctls will still work.
 */
struct fullrtentry {	/*XXX*/
	struct	rtentry r;
	u_long	rt_inits;		/* which metrics we are initializing */
	struct	rt_metrics rt_rmx;	/* metrics used by rx'ing protocols */
};

/*
 * Routing statistics.
 */
struct	rtstat {
	short	rts_badredirect;	/* bogus redirect calls */
	short	rts_dynamic;		/* routes created by redirects */
	short	rts_newgateway;		/* routes modified by redirects */
	short	rts_unreach;		/* lookups which failed */
	short	rts_wildcard;		/* lookups satisfied by a wildcard */
};

#ifdef _KERNEL
#define	RTFREE(rt) \
	if ((rt)->rt_refcnt == 1) \
		rtfree(rt); \
	else \
		(rt)->rt_refcnt--;

#endif

#if defined(_KERNEL) || defined(_KMEMUSER)
#ifdef sgi
#define	RTHASHSIZ	64
#else /* sgi */
#ifdef	GATEWAY
#define	RTHASHSIZ	64
#else
#define	RTHASHSIZ	8
#endif
#endif /* sgi */
#if	(RTHASHSIZ & (RTHASHSIZ - 1)) == 0
#define RTHASHMOD(h)	((h) & (RTHASHSIZ - 1))
#else
#define RTHASHMOD(h)	((h) % RTHASHSIZ)
#endif
#endif /* _KERNEL || _KMEMUSER */

#ifdef _KERNEL
#ifdef sgi
extern struct mbuf *rthost[RTHASHSIZ];
extern struct mbuf *rtnet[RTHASHSIZ];
extern struct rtstat rtstat;
extern void rtalloc(struct route *);
extern void rtinit(struct sockaddr *, struct sockaddr *, int, int);
extern void rtfree(struct rtentry *);
extern void rtredirect(struct sockaddr *, struct sockaddr *, int, struct sockaddr *);
extern int rtrequest(int, struct rtentry *);
#else /* sgi */
struct	mbuf *rthost[RTHASHSIZ];
struct	mbuf *rtnet[RTHASHSIZ];
struct	rtstat	rtstat;
#endif /* sgi */
#endif /* _KERNEL */
#endif	/* __net_route__ */
