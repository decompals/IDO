/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)af.h	7.1 (Berkeley) 6/4/86
 */

struct sockaddr;
struct afhash;

/*
 * Address family routines,
 * used in handling generic sockaddr structures.
 *
 * Hash routine is called
 *	af_hash(addr, h);
 *	struct sockaddr *addr; struct afhash *h;
 * producing an afhash structure for addr.
 *
 * Netmatch routine is called
 *	af_netmatch(addr1, addr2);
 * where addr1 and addr2 are sockaddr *.  Returns 1 if network
 * values match, 0 otherwise.
 */
#define	_HASHOP		struct sockaddr *, struct afhash *
#define	_NETMATCHOP	struct sockaddr *, struct sockaddr *
struct afswitch {
	int	(*af_hash)(_HASHOP);
	int	(*af_netmatch)(_NETMATCHOP);
};

struct afhash {
	u_int	afh_hosthash;
	u_int	afh_nethash;
};

#ifdef _KERNEL
extern struct	afswitch afswitch[];

/*
 * Routines to add and delete afswitch table entries.
 */
int af_add(int, int (*)(_HASHOP), int (*)(_NETMATCHOP));
int af_del(int);
#endif

#undef _HASHOP
#undef _NETMATCHOP
