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
 *	@(#)in_pcb.h	7.2 (Berkeley) 12/7/87 plus MULTICAST 1.0
 */

#ifdef __sgi
#include "sys/sema.h"
struct in_addr;
struct mbuf;
struct route;
struct socket;
#ifdef CIPSO	/* Trusted IRIX */
struct mac_label;
#endif
#endif

/*
 * Common structure pcb for internet protocol implementation.
 * Here are stored pointers to local and foreign host table
 * entries, local and foreign socket numbers, and pointers
 * up (to a socket structure) and down (to a protocol-specific)
 * control block.
 */
struct inpcb {
	struct	inpcb *inp_next,*inp_prev;
					/* pointers to other pcb's */
	struct	inpcb *inp_head;	/* pointer back to chain of inpcb's
					   for this protocol */
	struct	in_addr inp_faddr;	/* foreign host table entry */
	u_short	inp_fport;		/* foreign port */
	struct	in_addr inp_laddr;	/* local host table entry */
	u_short	inp_lport;		/* local port */
	struct	socket *inp_socket;	/* back pointer to socket */
	caddr_t	inp_ppcb;		/* pointer to per-protocol pcb */
	struct	route inp_route;	/* placeholder for routing entry */
	struct	mbuf *inp_options;	/* IP options */
#ifdef __sgi /* MULTICAST, 7.6 */
	struct	mbuf *inp_moptions;	/* IP multicast options */
	u_char	inp_ip_ttl;		/* XXX IP header ttl */
	u_char	inp_ip_tos;		/* XXX IP header tos */
	lock_t	inplock;	
	mrsplock_t *inp_mrlock;		/* MP locking for inpcb head list */
	int	inp_refcnt;		/* reference count */
#endif
};

#define	INPLOOKUP_WILDCARD	1
#define	INPLOOKUP_SETLOCAL	2

#define	sotoinpcb(so)	((struct inpcb *)(so)->so_pcb)

#ifdef _KERNEL
#ifdef __sgi
#ifdef _MP_NETLOCKS
#ifdef _ATOMIC_OPS
#define INPCB_MPSPIN(inp, s)		
#define INPCB_MPUNSPIN(inp, s)	
#define INPCB_HOLD(inp)			atomicAddInt(&(inp)->inp_refcnt, 1)
#else
#define INPCB_MPSPIN(inp, s)		s = splock((inp)->inplock);
#define INPCB_MPUNSPIN(inp, s)		spunlock((inp)->inplock, s);
#define INPCB_HOLD(inp) { \
		NETSPL_MPDECL(s); \
		INPCB_MPSPIN(inp, s); \
		(inp)->inp_refcnt++; \
		INPCB_MPUNSPIN(inp, s); \
}
#endif /* _ATOMIC_OPS */
#define INPCB_INITLOCKS(inp) { \
		initnlock(&(inp)->inplock, "inplk"); \
}
#define INPCB_FREELOCKS(inp) { \
		freesplock((inp)->inplock); \
		if ((inp)->inp_mrlock)  \
			mrspfree((inp)->inp_mrlock); \
}
#define INHEAD_SPINCOUNT	10
#define INHEAD_INITMRLOCK(inp) { \
	(inp)->inp_mrlock = (mrsplock_t *)kern_calloc(1, sizeof(mrsplock_t)); \
	mrinit_splock((inp)->inp_mrlock, "inpmrlk", INHEAD_SPINCOUNT); \
}
#define INHEAD_MRRLOCK(inp)		mrsplock((inp)->inp_mrlock, MR_ACCESS)
#define INHEAD_MRWLOCK(inp)		mrsplock((inp)->inp_mrlock, MR_UPDATE)
#define INHEAD_MRUNLOCK(inp)		mrspunlock((inp)->inp_mrlock)
#define INPCB_RELE(inp)			inpcb_rele(inp)
#define INPCB_ISMRLOCKED(inp)	ismrsplocked((inp)->inp_mrlock, MR_ACCESS|MR_UPDATE)
#define INPCB_ISHELD(inp)	((inp)->inp_refcnt > 1)

#else
#define INPCB_MPSPIN(inp, s)		
#define INPCB_MPUNSPIN(inp, s)	
#define INPCB_INITLOCKS(inp) 
#define INPCB_FREELOCKS(inp) 
#define INHEAD_INITMRLOCK(inp) 
#define INHEAD_MRRLOCK(inp)	
#define INHEAD_MRWLOCK(inp)
#define INHEAD_MRUNLOCK(inp)	
#define INPCB_HOLD(inp) { \
		(inp)->inp_refcnt++; \
}
#define INPCB_RELE(inp)			inpcb_rele(inp)
#define INPCB_ISMRLOCKED(inp)	(1)
#define INPCB_ISHELD(inp)	((inp)->inp_refcnt > 1)
#endif /* _MP_NETLOCKS */
extern void	in_losing(struct inpcb *);
extern int	in_pcballoc(struct socket *, struct inpcb *);
extern int	in_pcbbind(struct inpcb *, struct mbuf *);
extern int	in_pcbconnect(struct inpcb *, struct mbuf *);
extern void	in_pcbdetach(struct inpcb *);
extern void	in_pcbdisconnect(struct inpcb *);
extern void	in_pcbnotify(struct inpcb *, struct sockaddr *, u_short,
			     struct in_addr, u_short, int,
			     void (*)(struct inpcb *, int, void *),
			     void *);
extern int	in_pcbsetaddr(struct inpcb *, struct sockaddr_in *);

extern void	in_setpeeraddr(struct inpcb *, struct mbuf *);
extern void	in_setsockaddr(struct inpcb *, struct mbuf *);

#ifdef CIPSO
extern int	in_pcbsocklabel(struct inpcb *, struct mbuf *);
extern void	in_pcbchameleon(struct inpcb *, struct mac_label *, uid_t);
extern struct inpcb *in_pcblookup(struct inpcb *, struct in_addr, u_short,
				  struct in_addr, u_short, int, mac_label *);
#else
extern struct inpcb *in_pcblookup(struct inpcb *, struct in_addr, u_short,
				  struct in_addr, u_short, int);
#endif
extern int	in_pcbfree(struct inpcb *);
#endif /* __sgi */
#endif /* _KERNEL */
