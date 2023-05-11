#ifndef __netinet_if_ether__
#define __netinet_if_ether__
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
 *	@(#)if_ether.h	7.2 (Berkeley) 12/7/87 plus MULTICAST 1.0
 */

#ifdef __sgi
#include <net/if.h>
#include <netinet/in.h>
#endif

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	ether_header {
	u_char	ether_dhost[6];
	u_char	ether_shost[6];
	u_short	ether_type;
};

#define	ETHERTYPE_PUP	0x0200		/* PUP protocol */
#define	ETHERTYPE_IP	0x0800		/* IP protocol */
#define ETHERTYPE_ARP	0x0806		/* Addr. resolution protocol */
#ifdef __sgi
#define	ETHERTYPE_DECMOP	0x6001	/* DEC dump/load (MOP) */
#define	ETHERTYPE_DECCON	0x6002	/* DEC remote console */
#define	ETHERTYPE_DECnet	0x6003	/* DECnet 'routing' */
#define	ETHERTYPE_DECLAT	0x6004	/* DEC LAT */
#define	ETHERTYPE_SG_DIAG	0x8013	/* SGI diagnostic type */
#define	ETHERTYPE_SG_NETGAMES	0x8014	/* SGI network games */
#define	ETHERTYPE_SG_RESV	0x8015	/* SGI reserved type */
#define	ETHERTYPE_SG_BOUNCE	0x8016	/* SGI 'bounce server' */
#define	ETHERTYPE_XTP		0x817D	/* Protocol Engines XTP */

#define ETHERTYPE_8023		0x0004	/* IEEE 802.3 packet */

#define IEEE_8023_LEN		ETHERMTU /* IEEE 802.3 Length Field */
#endif /* __sgi */

/*
 * The ETHERTYPE_NTRAILER packet types starting at ETHERTYPE_TRAIL have
 * (type-ETHERTYPE_TRAIL)*512 bytes of data followed
 * by an ETHER type (as given above) and then the (variable-length) header.
 */
#define	ETHERTYPE_TRAIL		0x1000		/* Trailer packet */
#define	ETHERTYPE_NTRAILER	16

#define	ETHERMTU		1500
#define	ETHERMIN		(60-14)

#ifdef __sgi /* MULTICAST */
#ifdef _KERNEL
/*
 * Macro to map an IP multicast address to an Ethernet multicast address.
 * The high-order 25 bits of the Ethernet address are statically assigned,
 * and the low-order 23 bits are taken from the low end of the IP address.
 */
#define ETHER_MAP_IP_MULTICAST(ipaddr, enaddr)				\
	/* struct in_addr *ipaddr; */					\
	/* u_char enaddr[6];       */					\
{									\
	(enaddr)[0] = 0x01;						\
	(enaddr)[1] = 0x00;						\
	(enaddr)[2] = 0x5e;						\
	(enaddr)[3] = ((u_char *)ipaddr)[1] & 0x7f;			\
	(enaddr)[4] = ((u_char *)ipaddr)[2];				\
	(enaddr)[5] = ((u_char *)ipaddr)[3];				\
}
#endif
#endif /* __sgi */

/*
 * Ethernet Address Resolution Protocol.
 *
 * See RFC 826 for protocol description.  Structure below is adapted
 * to resolving internet addresses.  Field names used correspond to 
 * RFC 826.
 */
struct	ether_arp {
	struct	arphdr ea_hdr;	/* fixed-size header */
	u_char	arp_sha[6];	/* sender hardware address */
	u_char	arp_spa[4];	/* sender protocol address */
	u_char	arp_tha[6];	/* target hardware address */
	u_char	arp_tpa[4];	/* target protocol address */
};
#define	arp_hrd	ea_hdr.ar_hrd
#define	arp_pro	ea_hdr.ar_pro
#define	arp_hln	ea_hdr.ar_hln
#define	arp_pln	ea_hdr.ar_pln
#define	arp_op	ea_hdr.ar_op


/*
 * Structure shared between the ethernet driver modules and
 * the address resolution code.  For example, each ec_softc or il_softc
 * begins with this structure.
 */
struct	arpcom {
	struct 	ifnet ac_if;		/* network-visible interface */
	u_char	ac_enaddr[6];		/* ethernet hardware address */
	struct in_addr ac_ipaddr;	/* copy of ip address- XXX */
};

/*
 * Internet to ethernet address resolution table.
 */
struct	arptab {
	struct	in_addr at_iaddr;	/* internet address */
	u_char	at_enaddr[6];		/* ethernet address */
	u_char	at_timer;		/* minutes since last reference */
	u_char	at_flags;		/* flags */
	struct	mbuf *at_hold;		/* last packet until resolved/timeout */
};

#ifdef __sgi
struct	sritab {
	u_char	st_flags;		/* SRCROUTE flags */
	u_char	st_timer;		/* minutes since last reference */
#define	SRIF_VALID	0x1		/* this entry has valid SRC ROUTE */
#define	SRIF_ARP	0x2		/* this entry used by ARP */
#define	SRIF_NONARP	0x4		/* this entry used by none ARP */
#define	SRIF_PERM	0x8		/* Permanat staic entry */
	u_char	st_addr[6];		/* MAC address */
	u_short	st_ri[SRI_MAXLEN];	/* Route info */
#define SRI_LENMASK	0x1f00		/* Route info len including cnotrol */
};
#endif /* __sgi */

#ifdef	_KERNEL
extern void srarpinput(struct arpcom*, struct mbuf*, u_short*); /*SRCROUTE*/
extern u_short* srilook(char*, u_char);				/*SRCROUTE*/
extern u_char etherbroadcastaddr[6];
extern struct arptab *arptnew(struct in_addr *);
extern char *ether_sprintf(u_char *);
#ifdef __sgi
#define	ETHER_ISBROAD(addr) \
	(*(u_char*)(addr) == 0xff \
	 && !bcmp(addr, etherbroadcastaddr, sizeof etherbroadcastaddr))

#define	ETHER_ISGROUP(addr) (*(u_char*)(addr) & 0x01)

#define	ETHER_ISMULTI(addr) \
	(ETHER_ISGROUP(addr) \
	 && bcmp(addr, etherbroadcastaddr, sizeof etherbroadcastaddr))

/*
 * The address needs filtering before the packet is accepted when we're
 * in promiscuous mode and it's not for us and it's not a broadcast, or
 * we're filtering multicasts only and it's a multicast address.
 */
#define	ETHER_NEEDSFILTER(addr, ac) \
	(((ac)->ac_if.if_flags & IFF_PROMISC) \
	 && bcmp(addr, (ac)->ac_enaddr, sizeof (ac)->ac_enaddr) \
	 && !ETHER_ISBROAD(addr) \
	 || (((ac)->ac_if.if_flags & \
		(IFF_FILTMULTI|IFF_ALLMULTI)) == IFF_FILTMULTI) \
	 && ETHER_ISMULTI(addr))

extern	int arpresolve (struct arpcom *ac, struct mbuf *m,
			struct in_addr *destip, u_char *desten,
			int *usetrailers);
extern	void arpinput(struct arpcom *, struct mbuf *);
extern	void arpwhohas(struct arpcom *, struct in_addr *);

extern ether_cvtmulti(struct sockaddr *, int *);
#endif /* __sgi */

#endif
#endif /* !__netinet_if_ether__ */
