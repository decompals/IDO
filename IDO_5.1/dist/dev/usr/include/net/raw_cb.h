#ifndef __net_raw_cb__
#define __net_raw_cb__
/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)raw_cb.h	7.2 (Berkeley) 12/30/87 plus MULTICAST 1.0
 *	plus portions of 7.6 (Berkeley) 6/28/90
 */

#ifdef sgi
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <net/route.h>
#endif

struct mac_label;

/*
 * Raw protocol interface control block.  Used
 * to tie a socket to the generic raw interface.
 */
struct rawcb {
	struct	rawcb *rcb_next;	/* doubly linked list */
	struct	rawcb *rcb_prev;
	struct	socket *rcb_socket;	/* back pointer to socket */
	struct	sockaddr rcb_faddr;	/* destination address */
	struct	sockaddr rcb_laddr;	/* socket's address */
	struct	sockproto rcb_proto;	/* protocol family, protocol */
	caddr_t	rcb_pcb;		/* protocol specific stuff */
	struct	mbuf *rcb_options;	/* protocol specific options */
	struct	route rcb_route;	/* routing information */
	short	rcb_flags;
#ifdef sgi /* MULTICAST */
	struct	mbuf *rcb_moptions;	/* proto specific multicast options */
#endif
};

/*
 * Since we can't interpret canonical addresses,
 * we mark an address present in the flags field.
 */
#define	RAW_LADDR	01
#define	RAW_FADDR	02
#define	RAW_DONTROUTE	04		/* no routing, default */
#ifdef sgi  /* reno */
#define	RAW_HDRINCL	0x8000		/* XXX user supplies IP header. */
#endif

#define	sotorawcb(so)		((struct rawcb *)(so)->so_pcb)

/*
 * Nominal space allocated to a raw socket.
 */
#define	RAWSNDQ		8192
#define	RAWRCVQ		8192

/*
 * Format of raw interface header prepended by
 * raw_input after call from protocol specific
 * input routine.
 */
struct raw_header {
	struct	sockproto raw_proto;	/* format of packet */
	struct	sockaddr raw_dst;	/* dst address for rawintr */
	struct	sockaddr raw_src;	/* src address for sbappendaddr */
	struct	mac_label *raw_label;	/* security label for sbappendaddr */
	uid_t   raw_uid;	        /* uid for socket DAC */
};

#ifdef _KERNEL
#ifndef _IO_TPI_TPISOCKET_H_		/* only declare once */

struct rawcb rawcb;			/* head of list */

#endif 	/* _IO_TPI_TPISOCKET_H_ */
#endif	/* _KERNEL */
#endif	/* __net_raw_cb__ */
