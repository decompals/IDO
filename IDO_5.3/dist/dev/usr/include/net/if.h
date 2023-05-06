#ifndef __net_if__
#define __net_if__
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
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
 *	@(#)if.h	7.3 (Berkeley) 6/27/88 plus MULTICAST 1.1
 *	plus portions of 7.9 (Berkeley) 6/28/90
 */

#include <sys/socket.h>
#include <sys/time.h>

/*
 * Structures defining a network interface, providing a packet
 * transport mechanism (ala level 0 of the PUP protocols).
 *
 * Each interface accepts output datagrams of a specified maximum
 * length, and provides higher level routines with input datagrams
 * received from its medium.
 *
 * Output occurs when the routine if_output is called, with three parameters:
 *	(*ifp->if_output)(ifp, m, dst)
 * Here m is the mbuf chain to be sent and dst is the destination address.
 * The output routine encapsulates the supplied datagram if necessary,
 * and then transmits it on its medium.
 *
 * On input, each interface unwraps the data received by it, and either
 * places it on the input queue of a internetwork datagram routine
 * and posts the associated software interrupt, or passes the datagram to a raw
 * packet input routine.
 *
 * Routines exist for locating interfaces by their addresses
 * or for locating a interface on a certain network, as well as more general
 * routing and gateway routines maintaining information used to locate
 * interfaces.  These routines live in the files if.c and route.c
 */


/* Trusted IRIX */
#include <sys/mac_label.h>

/* The size of this structure should not exceed sizeof (struct sockaddr)
 * so as not to increase the size of struct ifreq, and thereby force
 * recompilation of numerous user programs.
 * Limits authority fields to 8 bits max.
 */
typedef struct ifsec {
	mac_label * ifs_label_max;	/* dominates all dgrams on if   */
	mac_label * ifs_label_min;	/* dominated by all if dgrams 	*/
	u_long      ifs_doi;		/* domain of interpretation	*/
	u_char	    ifs_authority_max;	/* maximum authority allowed	*/
	u_char	    ifs_authority_min;	/* minimum authority permitted	*/
	u_char	    ifs_reserved;	/* must be zero until defined	*/
	u_char	    ifs_idiom;		/* security idiom (see below)	*/
} ifsec_t;

/* values for ifs_idiom */
#define	IDIOM_MONO		0	/* Monolabel (unlabelled) 	*/
#define IDIOM_BSO_TX		1	/* BSO required for TX, not RX  */
#define IDIOM_BSO_RX		2	/* BSO required for RX, not TX  */
#define IDIOM_BSO_REQ		3	/* BSD required for TX and RX 	*/
#define IDIOM_CIPSO		4	/* CIPSO, Apr 91, tag types 1, 2*/
#define IDIOM_SGIPSO		5	/* SGI's Mint-flavored CIPSO, Apr 91*/
#define IDIOM_TT1		6	/* CIPSO, Apr 91, tag type 1 only */ 
#define IDIOM_CIPSO2		7	/* CIPSO2, Jan 92 		*/
#define IDIOM_SGIPSO2		8	/* SGIPSO2, Jan 92, with UID*/
#define IDIOM_SGIPSOD		9	/* SGIPSO, Apr 91, with UID */
#define IDIOM_SGIPSO2_NO_UID    10	/* SGIPSO2, Jan 92, no UID	*/
#define IDIOM_TT1_CIPSO2        11	/* CIPSO2, Jan 92, tag type 1 only */ 
#define IDIOM_MAX		IDIOM_TT1_CIPSO2

#define if_label_max		if_sec.ifs_label_max
#define if_label_min		if_sec.ifs_label_min
#define if_doi			if_sec.ifs_doi
#define if_authority_max	if_sec.ifs_authority_max
#define if_authority_min	if_sec.ifs_authority_min
#define if_idiom		if_sec.ifs_idiom


/*
 * Structure defining a queue for a network interface.
 *
 * (Would like to call this struct ``if'', but C isn't PL/1.)
 */
struct ifnet {
	char	*if_name;		/* name, e.g. ``en'' or ``lo'' */
	short	if_unit;		/* sub-unit for lower level driver */
	u_short	if_mtu;			/* maximum transmission unit */
	short	if_flags;		/* up/down, broadcast, etc. */
	short	if_timer;		/* time 'til if_watchdog called */
	int	if_metric;		/* routing metric (external only) */
	u_char	if_type;		/* ethernet, tokenring, etc */
	u_char	if_index;		/* numeric abbreviation for this if  */
	u_short if_xxxpad;		/* XXX undefined */
	struct	ifaddr *if_addrlist;	/* linked list of addresses per if */
	struct	ifqueue {
		struct	mbuf *ifq_head;
		struct	mbuf *ifq_tail;
		int	ifq_len;
		int	ifq_maxlen;
		u_long	ifq_drops;
		lock_t	ifq_lock;
	} if_snd;			/* output queue */
/* procedure handles */
	int	(*if_init)(int);	/* init routine */
	int	(*if_output)(struct ifnet *, struct mbuf *, struct sockaddr *);
					/* output routine */
	int	(*if_ioctl)(struct ifnet *, int, void *);
					/* ioctl routine */
	int	(*if_reset)(int, int);	/* bus reset routine */
	void	(*if_watchdog)(int);	/* timer routine */
	struct	ifnet *if_next;
/* generic interface statistics */
	u_long	if_ipackets;		/* packets received on interface */
	u_long	if_ierrors;		/* input errors on interface */
	u_long	if_opackets;		/* packets sent on interface */
	u_long	if_oerrors;		/* output errors on interface */
	u_long	if_collisions;		/* collisions on csma interfaces */
/* MIB-II statistics */
	time_t	if_lastchange;		/* last updated */
	u_long	if_ibytes;		/* total number of octets received */
	u_long	if_obytes;		/* total number of octets sent */
	u_long	if_imcasts;		/* pkts received via broad/multicast */
	u_long	if_omcasts;		/* packets sent via broad/multicast */
	u_long	if_iqdrops;		/* dropped on input, this interface */
	u_long	if_noproto;		/* destined for unsupported protocol */
	u_long	if_baudrate;		/* linespeed */
	u_long	if_odrops;		/* output pkts discarded w/o error */
/* 
 * Trusted IRIX: Any changes to the following three 
 * elements must be done at splnet or equivalent.
 */
	struct	mbuf *if_lbl_cache;	/* label mapping cache    */
	ifsec_t	if_sec;			/* interface security information */
	uid_t 	if_uid;			/* default uid for dacless if */	

	lock_t	if_lock;
};
#define	if_idrops	if_iqdrops	/* XXX compatibility */
#define	if_unknowns	if_noproto

#define	IFF_UP		0x1		/* interface is up */
#define	IFF_BROADCAST	0x2		/* broadcast address valid */
#define	IFF_DEBUG	0x4		/* turn on debugging */
#define	IFF_LOOPBACK	0x8		/* is a loopback net */
#define	IFF_POINTOPOINT	0x10		/* interface is point-to-point link */
#define	IFF_NOTRAILERS	0x20		/* avoid use of trailers */
#define	IFF_RUNNING	0x40		/* resources allocated */
#define	IFF_NOARP	0x80		/* no address resolution protocol */
#define	IFF_PROMISC	0x100		/* receive all packets */
#define	IFF_ALLMULTI	0x200		/* receive all multicast packets */
#define	IFF_FILTMULTI	0x400		/* need to filter multicast packets */
#define	IFF_INTELLIGENT	0x400		/* MIPS ABI - protocols on interface */
#define	IFF_MULTICAST	0x800		/* supports multicast */
#define IFF_CKSUM	0x1000		/* does checksumming */
#define IFF_ALLCAST	0x2000		/* does SRCROUTE broadcast */
#define IFF_PRIVATE     0x8000          /* MIPS ABI - do not advertise */

/*
 * The IFF_MULTICAST flag indicates that the network can support the
 * transmission and reception of higher-level (e.g., IP) multicast packets.
 * It is independent of hardware support for multicasting; for example,
 * point-to-point links or pure broadcast networks may well support
 * higher-level multicasts.
 *
 * The IFF_FILTMULTI flag indicates that the interface has imperfect
 * hardware multicast filtering. Additional filtering is required before
 * a multicast packet is accepted.
 *
 * The IFF_ALLCAST flag indicates that the interface supports
 * 802.5 source routing broadcast so that the broadcast msg can
 * cross bridges.
 */

/* flags set internally only: */
#define	IFF_CANTCHANGE	(IFF_BROADCAST | IFF_POINTOPOINT |   \
			    IFF_RUNNING | IFF_MULTICAST | IFF_FILTMULTI)

/*
 * Output queues (ifp->if_snd) and internetwork datagram level (pup level 1)
 * input routines have queues of messages stored on ifqueue structures
 * (defined above).  Entries are added to and deleted from these structures
 * by these macros, which should be called with ipl raised to splimp().
 */
#ifdef _MP_NETLOCKS
extern lock_t ifhead_lock;
#define IFHEAD_INITLOCK()	initnlock(&ifhead_lock, "ifheadlk")
#define IFHEAD_LOCK()	spsema(ifhead_lock);
#define IFHEAD_UNLOCK()	svsema(ifhead_lock);

#define IFNET_INITLOCKS(ifp) { \
			initnlock(&(ifp)->if_lock, "if_lock"); \
			IFQ_INITLOCK(&(ifp)->if_snd); \
}
#define IFQ_INITLOCK(ifq)	initnlock(&(ifq)->ifq_lock, "ifq_lock") 
#define IFNET_ISLOCKED(ifp)	(ownlock((ifp)->if_lock))
#define IFQ_LOCK(q)		spsema((q)->ifq_lock)
#define IFQ_UNLOCK(q)		svsema((q)->ifq_lock)
#define IFNET_UPPERLOCK(ifp,s)	{ 	\
	if (!(ifp->if_flags & IFF_LOOPBACK))		\
		IFNET_LOCK(ifp, s); 	\
}
#define IFNET_UPPERUNLOCK(ifp,s) {	\
	if (!(ifp->if_flags & IFF_LOOPBACK))		\
		IFNET_UNLOCK(ifp, s);	\
}
#define IFNET_LOCK(ifp,s)	s = io_splockspl((ifp)->if_lock, splimp)
#define IFNET_UNLOCK(ifp,s)	io_spunlockspl((ifp)->if_lock, s)
#define IFNET_LOCKNOSPL(ifp) 	spsema((ifp)->if_lock)
#define IFNET_UNLOCKNOSPL(ifp) 	svsema((ifp)->if_lock)
#define	IF_ENQUEUE_NOLOCK(ifq, m) { \
	(m)->m_act = 0; \
	if ((ifq)->ifq_tail == 0) \
		(ifq)->ifq_head = m; \
	else \
		(ifq)->ifq_tail->m_act = m; \
	(ifq)->ifq_tail = m; \
	(ifq)->ifq_len++; \
}
#define	IF_DEQUEUE_NOLOCK(ifq, m) { \
	(m) = (ifq)->ifq_head; \
	if (m) { \
		if (((ifq)->ifq_head = (m)->m_act) == 0) \
			(ifq)->ifq_tail = 0; \
		(m)->m_act = 0; \
		(ifq)->ifq_len--; \
	} \
}
#else
#define IFHEAD_INITLOCK()	
#define IFHEAD_LOCK()
#define IFHEAD_UNLOCK()	

#define IFNET_INITLOCKS(ifp) 
#define IFQ_INITLOCK(ifq)
#define IFNET_ISLOCKED(ifp)	(1)
#define IFQ_LOCK(q)	
#define IFQ_UNLOCK(q)
#define IFNET_UPPERLOCK(ifp,s)
#define IFNET_UPPERUNLOCK(ifp,s)
#define IFNET_LOCK(ifp,s)	s = splimp()
#define IFNET_UNLOCK(ifp,s)	splx(s)
#define IFNET_LOCKNOSPL(ifp) 
#define IFNET_UNLOCKNOSPL(ifp)
#define	IF_ENQUEUE_NOLOCK(ifq, m) 	IF_ENQUEUE(ifq, m)
#define	IF_DEQUEUE_NOLOCK(ifq, m) 	IF_DEQUEUE(ifq, m)
#endif /* _MP_NETLOCKS */

#define	IF_QFULL(ifq)		((ifq)->ifq_len >= (ifq)->ifq_maxlen)
#define	IF_DROP(ifq)		((ifq)->ifq_drops++)
#define	IF_ENQUEUE(ifq, m) { \
	IFQ_LOCK(ifq); \
	(m)->m_act = 0; \
	if ((ifq)->ifq_tail == 0) \
		(ifq)->ifq_head = m; \
	else \
		(ifq)->ifq_tail->m_act = m; \
	(ifq)->ifq_tail = m; \
	(ifq)->ifq_len++; \
	IFQ_UNLOCK(ifq); \
}
#define	IF_PREPEND(ifq, m) { \
	IFQ_LOCK(ifq); \
	(m)->m_act = (ifq)->ifq_head; \
	if ((ifq)->ifq_tail == 0) \
		(ifq)->ifq_tail = (m); \
	(ifq)->ifq_head = (m); \
	(ifq)->ifq_len++; \
	IFQ_UNLOCK(ifq); \
}
/*
 * Packets destined for level-1 protocol input routines have the following
 * structure prepended to the data.  IF_DEQUEUEHEADER extracts and returns
 * this structure's interface pointer, and skips over the space indicated
 * by it, when dequeueing the packet.  Otherwise M_SKIPIFHEADER should be
 * used to adjust for the ifheader.
 */
struct ifheader {
	struct ifnet	*ifh_ifp;	/* pointer to receiving interface */
	u_short		ifh_hdrlen;	/* byte size of this structure */
};

#define	M_ADJ(m, adj) { \
	(m)->m_off += (adj); \
	(m)->m_len -= (adj); \
	if ((m)->m_len == 0) \
		(m) = m_free(m); \
}
#define	M_SKIPIFHEADER(m) { \
	register int hdrlen; \
	hdrlen = mtod((m), struct ifheader *)->ifh_hdrlen; \
	M_ADJ(m, hdrlen); \
}
#define	IF_DEQUEUEHEADER(ifq, m, ifp) { \
	IFQ_LOCK(ifq); \
	(m) = (ifq)->ifq_head; \
	if (m) { \
		struct ifheader *ifh; \
		(ifq)->ifq_head = (m)->m_act; \
		if ((ifq)->ifq_head == 0) \
			(ifq)->ifq_tail = 0; \
		(m)->m_act = 0; \
		--(ifq)->ifq_len; \
		ifh = mtod((m), struct ifheader *); \
		(ifp) = ifh->ifh_ifp; \
		M_ADJ(m, ifh->ifh_hdrlen); \
	} \
	IFQ_UNLOCK(ifq); \
}


/*
 * Initialize a receive buffer's ifqueue header to point at ifp and to
 * have a data offset hdrlen bytes beyond buf.
 */
#define	IF_INITHEADER(buf, ifp, hdrlen) { \
	struct ifheader *ifh; \
	ifh = (struct ifheader *)(buf); \
	ifh->ifh_ifp = (ifp); \
	ifh->ifh_hdrlen = (hdrlen); \
}
#define	M_INITIFHEADER(m, ifp, hdrlen) { \
	struct ifheader *ifh; \
	ifh = mtod(m, struct ifheader *); \
	ifh->ifh_ifp = (ifp); \
	(m)->m_len = ifh->ifh_hdrlen = (hdrlen); \
}

#define	IF_DEQUEUE(ifq, m) { \
	IFQ_LOCK(ifq); \
	(m) = (ifq)->ifq_head; \
	if (m) { \
		if (((ifq)->ifq_head = (m)->m_act) == 0) \
			(ifq)->ifq_tail = 0; \
		(m)->m_act = 0; \
		(ifq)->ifq_len--; \
	} \
	IFQ_UNLOCK(ifq); \
}

#define	IFQ_MAXLEN	50
#define	IFNET_SLOWHZ	1		/* granularity is 1 second */
/*
 * The ifaddr structure contains information about one address
 * of an interface.  They are maintained by the different address families,
 * are allocated and attached when an address is set, and are linked
 * together so all addresses for an interface can be located.
 */
struct ifaddr {
	struct	sockaddr ifa_addr;	/* address of interface */
	union {
		struct	sockaddr ifu_broadaddr;
		struct	sockaddr ifu_dstaddr;
	} ifa_ifu;
#define	ifa_broadaddr	ifa_ifu.ifu_broadaddr	/* broadcast address */
#define	ifa_dstaddr	ifa_ifu.ifu_dstaddr	/* other end of p-to-p link */
	struct	ifnet *ifa_ifp;		/* back-pointer to interface */
	struct	ifaddr *ifa_next;	/* next address for interface */
};

struct ifstats {
	u_long	ifs_ipackets;		/* packets received on interface */
	u_long	ifs_opackets;		/* packets sent on interface */
	u_short	ifs_ierrors;		/* input errors on interface */
	u_short	ifs_oerrors;		/* output errors on interface */
	u_long	ifs_collisions;		/* collisions on csma interfaces */
};


/*
 * Interface request structure used for socket
 * ioctl's.  All interface ioctl's must have parameter
 * definitions which begin with ifr_name.  The
 * remainder may be interface specific.
 */
struct	ifreq {
#define	IFNAMSIZ	16
	char	ifr_name[IFNAMSIZ];		/* if name, e.g. "en0" */
	union {
		struct	sockaddr ifru_addr;
		struct	sockaddr ifru_dstaddr;
		struct	sockaddr ifru_broadaddr;
		short	ifru_flags;
		int	ifru_metric;
		char	ifru_data[1];		/* MIPS ABI - unused by BSD */
		char	ifru_enaddr[6];		/* MIPS ABI */
		char	ifru_oname[IFNAMSIZ];	/* MIPS ABI */
		struct	ifstats ifru_stats;
/* Trusted IRIX */
		ifsec_t	ifru_sec;	
		uid_t	ifru_uid;	
	} ifr_ifru;
#define	ifr_addr	ifr_ifru.ifru_addr	/* address */
#define	ifr_dstaddr	ifr_ifru.ifru_dstaddr	/* other end of p-to-p link */
#define	ifr_broadaddr	ifr_ifru.ifru_broadaddr	/* broadcast address */
#define	ifr_flags	ifr_ifru.ifru_flags	/* flags */
#define	ifr_metric	ifr_ifru.ifru_metric	/* metric */
#define	ifr_data	ifr_ifru.ifru_data	/* for use by interface */
#define ifr_enaddr      ifr_ifru.ifru_enaddr    /* ethernet address */
#define ifr_oname       ifr_ifru.ifru_oname     /* other if name */
#define	ifr_stats	ifr_ifru.ifru_stats	/* statistics */
/* Trusted IRIX */
#define	ifr_sec		ifr_ifru.ifru_sec	/* security configuration */
#define	ifr_uid		ifr_ifru.ifru_uid	/* security */
	
};
/*
 * Structure used in SIOCGIFCONF request.
 * Used to retrieve interface configuration
 * for machine (useful for programs which
 * must know all networks accessible).
 */
struct	ifconf {
	int	ifc_len;		/* size of associated buffer */
	union {
		caddr_t	ifcu_buf;
		struct	ifreq *ifcu_req;
	} ifc_ifcu;
#define	ifc_buf	ifc_ifcu.ifcu_buf	/* buffer address */
#define	ifc_req	ifc_ifcu.ifcu_req	/* array of structures returned */
};


#include <net/if_arp.h>
#ifdef _KERNEL
extern struct	ifqueue rawintrq;		/* raw packet input queue */
extern struct	ifnet *ifnet;

extern struct	ifaddr *ifa_ifwithaddr(struct sockaddr *);
extern struct	ifaddr *ifa_ifwithnet(struct sockaddr *);
extern struct	ifaddr *ifa_ifwithdstaddr(struct sockaddr *);

extern struct	ifnet *ifunit(char *);
extern int	if_attach(struct ifnet *);
extern int	if_down(struct ifnet *);

struct	socket;
extern int	ifioctl(struct socket *, int , caddr_t);

extern __uint32_t in_cksum_sub(ushort *, int, __uint32_t);

extern int	if_dropall(struct ifqueue*);
#endif /* _KERNEL */
#endif	/* !__net_if__ */
