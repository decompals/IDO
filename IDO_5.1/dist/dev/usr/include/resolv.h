#ifndef __RESOLV_H__
#define __RESOLV_H__
/*
 * -
 * Copyright (c) 1983, 1987, 1989, 1993
 *    The Regents of the University of California.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 * 	This product includes software developed by the University of
 * 	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * -
 * Portions Copyright (c) 1993 by Digital Equipment Corporation.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Digital Equipment Corporation not be used in advertising or
 * publicity pertaining to distribution of the document or software without
 * specific, written prior permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * -
 * --Copyright--
 */

/*
 *	@(#)resolv.h	8.1 (Berkeley) 6/2/93
 *	Id: resolv.h,v 4.9.1.2 1993/05/17 09:59:01 vixie Exp vixie $
 */

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>

/*
 * Resolver configuration file.
 * Normally not present, but may contain the address of the
 * inital name server(s) to query and the domain search list.
 */

#ifndef _PATH_RESCONF
#define _PATH_RESCONF        "/etc/resolv.conf"
#endif

/*
 * Global defines and variables for resolver stub.
 */
#define	MAXNS		3		/* max # name servers we'll track */
#define	MAXDFLSRCH	3		/* # default domain levels to try */
#define	MAXDNSRCH	6		/* max # domains in search path */
#define	LOCALDOMAINPARTS 2		/* min levels in name that is "local" */

#define	RES_TIMEOUT	5		/* min. seconds between retries */

/* Formerly "struct state" */
struct __res_state {
	int	retrans;	 	/* retransmition time interval */
	int	retry;			/* number of times to retransmit */
	long	options;		/* option flags - see below. */
	int	nscount;		/* number of name servers */
	struct	sockaddr_in nsaddr_list[MAXNS];	/* address of name server */
#define	nsaddr	nsaddr_list[0]		/* for backward compatibility */
	u_short	id;			/* current packet id */
	char	*dnsrch[MAXDNSRCH+1];	/* components of domain to search */
	char	defdname[MAXDNAME];	/* default domain */
	long	pfcode;			/* RES_PRF_ flags - see below. */
};

/*
 * Resolver options
 */
#define RES_INIT	0x0001		/* address initialized */
#define RES_DEBUG	0x0002		/* print debug messages */
#define RES_AAONLY	0x0004		/* authoritative answers only */
#define RES_USEVC	0x0008		/* use virtual circuit */
#define RES_PRIMARY	0x0010		/* query primary server only */
#define RES_IGNTC	0x0020		/* ignore trucation errors */
#define RES_RECURSE	0x0040		/* recursion desired */
#define RES_DEFNAMES	0x0080		/* use default domain name */
#define RES_STAYOPEN	0x0100		/* Keep TCP socket open */
#define RES_DNSRCH	0x0200		/* search up local domain tree */

#define RES_DEFAULT	(RES_RECURSE | RES_DEFNAMES | RES_DNSRCH)

/*
 * Resolver "pfcode" values.  Used by dig.
 */
#define RES_PRF_STATS	0x0001
/*			0x0002	*/
#define RES_PRF_CLASS   0x0004
#define RES_PRF_CMD	0x0008
#define RES_PRF_QUES	0x0010
#define RES_PRF_ANS	0x0020
#define RES_PRF_AUTH	0x0040
#define RES_PRF_ADD	0x0080
#define RES_PRF_HEAD1	0x0100
#define RES_PRF_HEAD2	0x0200
#define RES_PRF_TTLID	0x0400
#define RES_PRF_HEADX	0x0800
#define RES_PRF_QUERY	0x1000
#define RES_PRF_REPLY	0x2000
#define RES_PRF_INIT    0x4000
/*			0x8000	*/

/* Routines documented in resolver(3N) */ 
extern int dn_comp (const u_char *, u_char *, int, u_char **, u_char **);
extern int dn_expand (const u_char *, const u_char *, const u_char *,
			u_char *, int);
extern int res_init (void);
extern int res_mkquery (int, const char *, int, int, const char *,
			int, const char *, char *, int);
extern int res_query (const char *, int, int, u_char *, int);
extern int res_search (const char *, int, int, u_char *, int);
extern int res_send (const char *, int, char *, int);
extern struct __res_state _res;

/* BIND internal undocumented routines */
extern char *__hostalias (const char *);
extern char *p_cdname (const char *, const char *, FILE *),
	    *p_rr (const char *, const char *, FILE *),
	    *__p_type (int),
	    *__p_class (int),
	    *__p_time (u_int32_t);
extern void __p_query (const char *);
extern void __fp_query (const char *, FILE *);
extern void __fp_resstat (struct __res_state *, FILE *);
extern void __putlong (u_int32_t, u_char *);
extern void __putshort (u_short, u_char *);
extern int __dn_skipname (const u_char *, const u_char *);


#ifdef __cplusplus
}
#endif

#endif /* !__RESOLV_H__ */
