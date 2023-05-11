#ifndef __ARPA_INET_H__
#define __ARPA_INET_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)inet.h	5.4 (Berkeley) 6/1/90
 */

/* External definitions for functions in inet(3N) */

#include <sys/types.h>
#include <netinet/in.h>

extern unsigned long	inet_addr(const char *);
extern char *		inet_ntoa(struct in_addr);
extern struct in_addr	inet_makeaddr(int, int);
extern unsigned long	inet_network(const char *);
extern unsigned long	inet_lnaof(struct in_addr);
extern unsigned long	inet_netof(struct in_addr);

#ifdef __cplusplus
}
#endif

#endif /* __ARPA_INET_H__ */
