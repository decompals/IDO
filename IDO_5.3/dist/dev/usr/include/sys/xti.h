/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Avenue                               |
 * |         Sunnyvale, California 94088-3650, USA             |
 * |-----------------------------------------------------------|
 */
/* $Header: /proj/irix5.3/isms/irix/kern/sys/RCS/xti.h,v 1.1 1992/06/26 21:09:33 nelson Exp $ */

#ifndef _SYS_XTI_H
#define _SYS_XTI_H

#include <sys/tiuser.h>

/*
 * The following are the error codes needed by both the kernel
 * level transport providers and the user level library.
 */

#define TNOSTRUCTYPE	20	/* unsupported struct-type requested */
#define TBADNAME	21	/* invalid transport provider name */
#define TBADQLEN	22	/* qlen is zero */
#define TADDRBUSY	23	/* address in use */

/* 
 * The following are the events returned by t_look
 */

#define T_GODATA	0x0100	/* sending normal data is again possible */
#define T_GOEXDATA	0x0200	/* sending expedited data is again possible */
#undef T_EVENTS
#define T_EVENTS	0x03ff	/* event mask	                  */

/*
 * XTI error return
 */

#ifndef _KERNEL
#ifdef _BUILDING_SVR4_LIBC
extern int *_t_errnop;
#define T_ERRNO	(*_t_errnop)
#else
extern int t_errno;
#endif /* _BUILDING_SVR4_LIBC */
#endif

/*
 * The following are structure types used when dynamically
 * allocating the above structures via t_alloc().
 */
#define T_BIND_STR	1		/* struct t_bind	*/
#define T_OPTMGMT_STR	2		/* struct t_optmgmt	*/
#define T_CALL_STR	3		/* struct t_call	*/
#define T_DIS_STR	4		/* struct t_discon	*/
#define T_UNITDATA_STR	5		/* struct t_unitdata	*/
#define T_UDERROR_STR	6		/* struct t_uderr	*/
#define T_INFO_STR	7		/* struct t_info	*/

/*
 * general purpose defines
 */

#define	T_YES		1
#define T_NO		0
#define T_UNUSED	-1
#define	T_NULL		0
#define T_ABSREQ	0x8000

/*
 *	ISO-specific option and management primitives 
 */

/*
 *	definition of the ISO transport classes
 */

#define	T_CLASS0	0
#define	T_CLASS1	1
#define	T_CLASS2	2
#define	T_CLASS3	3
#define	T_CLASS4	4

/*
 *	definition of the  priorities 
 */

#define	T_PRITOP	0
#define	T_PRIHIGH	1
#define	T_PRIMID	2
#define	T_PRILOW	3
#define	T_PRIDFLT	4

/*
 *	definition of the protection levels
 */

#define	T_NOPROTECT		1
#define	T_PASSIVEPROTECT	2
#define T_ACTIVEPROTECT		4

/*
 *	default values for the length of the TPDU's
 */

#define	T_LTPDUDFLT	128

/*
 *	rate structure
 */

struct rate {
	long	targetvalue;		/* target value */
	long	minacceptvalue;		/* minimum acceptable value */
};

/*
 *	reqvalue structure
 */

struct reqvalue {
	struct	rate	called;		/* called rate */
	struct	rate	calling;	/* calling rate */
};

/*
 *	thrpt structure
 */

struct	thrpt {
	struct	reqvalue maxthrpt;	/* maximum throughput */
	struct	reqvalue avgthrpt;	/* average throughput */
};

/*
 *	management structure 
 */

struct	management {
	short	dflt;		/* T_YES: the following parameter values */
				/*	  are ignored, default values	*/
				/*	  are used;			*/
				/* T_NO:  the following parameter values */
				/*	  are used			*/
	int	ltpdu;		/* maximum length of TPDU (in octets)	*/
	short	reastime;	/* reassignment time (in seconds)	*/
	char	class;		/* preferred class; value: T_CLASS0-	*/
				/*	T_CLASS4			*/
	char	altclass;	/* alternative class			*/
	char	extform;	/* extended format: T_YES or T_NO	*/
	char	flowctrl;	/* flow control: T_YES or T_NO		*/
	char	checksum;	/* checksum (cl. 4): T_YES or T_NO	*/
	char	netexp;		/* network expeditted data: T_YES or 	*/
				/* 	T_NO				*/
	char	netrecptcf;	/* receipt confirmation: T_YES or T_NO	*/
};


/*
 *	ISO connection-oriented optoins
 */

struct	isoco_options {
	struct	thrpt	throughput;	/* throughput			*/
	struct	reqvalue transdel;	/* transit delay		*/
	struct	rate	reserrorrate;	/* residual error rate		*/
	struct	rate	transffailprob;	/* transfer failure probability */
	struct	rate	estfailprob;	/* connection establishment 	*/
					/* 	failure probability	*/
	struct	rate	relfailprob;	/* connection release failure	*/
					/*	probability		*/
	struct	rate	estdelay;	/* connection establishment	*/
					/*	delay			*/
	struct	rate	reldelay;	/* connection release delay	*/
	struct	netbuf	connresil;	/* connection resilience	*/
	unsigned short	protection;	/* protection			*/
	short		priority;	/* priority 			*/
	struct management mngmt;	/* management parameters	*/
	char		expd;		/* expedited data: T_YES or	*/
					/*	T_NO			*/
};

/*
 *	ISO connectionless options
 */

struct isocl_options {
	struct	rate	transdel;	/* transit delay		*/
	struct	rate	reserrorrate;	/* residual error rate		*/
	unsigned short	protection;	/* protection			*/
	short		priority;	/* priority			*/
};

/*
 *	TCP-specific environment 
 */

/*
 *	TCP precedence levels
 */

#define	T_ROUTINE	0
#define	T_PRIORITY	1
#define	T_IMMEDIATE	2
#define	T_FLASH		3
#define	T_OVERRIDEFLASH	4
#define	T_CRITIC_ECP	5
#define	T_INETCONTROL	6
#define	T_NETCONTROL	7

/*
 *	TCP security options structure
 */

struct	secoptions {
	short	security;		/* security field */
	short	compartment;		/* compartment */
	short	handling;		/* handling restrictions */
	long	tcc;			/* transmission control code */
};

/*
 *	TCP options
 */

struct	tcp_options {
	short		precedence;	/* precedence */
	long		timeout;	/* abort timeout */
	long		max_seg_size;	/* maximum segment size */
	struct secoptions secopt;	/* TCP security options */
};


#endif	/* _SYS_XTI_H */
