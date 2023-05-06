/**************************************************************************
 *									  *
 * 		 Copyright (C) 1989-1993 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
#ifndef __SYS_PAR_H__
#define __SYS_PAR_H__

#ident "$Revision: 1.32 $"

#ifdef __cplusplus
extern "C" {
#endif

#include "sys/syscall.h"

/* par operation types values given to /dev/par(ioctl) */

#define PAR_START	100	/* par start */
#define PAR_SYSCALLINH	101	/* trace syscalls for family */
#define PAR_READ	102	/* read par buffer */
#define PAR_SYSCALL	103	/* trace system calls  */
#define PAR_RESCHED	104	/* trace scheduler  */
#define PAR_NETSCHED	105	/* trace network scheduling */
#define PAR_NETFLOW	106	/* trace network data flow */
#define PAR_VMFAULT	107	/* trace VM faults */
#define PAR_VMFAULT_SET 108	/* select VM fault tracing set */
#define PAR_SETMAXIND	109	/* setr maximum # of indirect bytes */

/* storage format tokens for fawlty */
#define OLDPROCTKN	3	/* resched -- old process token */
#define NEWPROCTKN	4	/* resched -- new process token */
#define IDLEPROCTKN	5	/* resched -- idle process token */
#define USTRINGTKN	6	/* user string token */
#define SYSCALLTKN	8	/* system call token */
#define SYSRETTKN	9	/* system call return code token */
#define RUNQTKN		11	/* resched -- runq  token */
#define OFLOWTKN	13	/* overflow count */
#define NAMETKN		14	/* process name token */
#define RMPROCTKN	15	/* remove died process token */
#define NETEVENTKN	16	/* net new event inited */
#define NETSLEEPTKN	17	/* net sleeps */
#define NETWAKEUPTKN	18	/* net waken up */
#define NETWAKINGTKN	19	/* net waking up */
#define NETFLOWTKN	20	/* net data flow */
#define NETDROPTKN	21	/* net data drop */
#define NETEVENTDONETKN	22	/* net event done */
#define SENDSIGTKN	23	/* process is sending (delivering) signal */
#define RCVSIGTKN	24	/* process is receiving (processing) signal */
#define VMFAULTTKN	25	/* VM fault token */

/* reasons for idle */
#define NO_STACK	0x1	/* kernel stack is used on another processor */
#define NO_SLOAD	0x2	/* not in core */
#define MUSTRUN		0x4	/* only allowed to run on certain processor */
#define NO_FP		0x8	/* FP on another processor */
#define NO_GFX		0x10	/* graphics lock held by another process */
#define EMPTY		0x20	/* run queue empty */
#define NO_DISC		0x40	/* cpu doesn't run right sched disciplines */
#define IN_SINGLE	0x80	/* wrong proc when in single proc mode */
#define NO_GANG		0x100	/* proc member of gang that can't schedule */
#define GANG_PREEMPTED	0x200	/* gang is queued but better ndpri guy */

/* reasons for resheduled */
#define MUSTRUNCPU	0x01	/* must run on other cpu */
#define SEMAWAIT	0x02	/* wait for semaphore */
#define RESCHED_P	0x04	/* rescheduled due to preempt */
#define RESCHED_Y	0x08	/* rescheduled due to yield */
#define RESCHED_S	0x10	/* rescheduled due to stopped by signal */
#define RESCHED_D	0x20	/* rescheduled due to process died */

/* reasons for netevents */
#define NETRES_NULL	0x00	/* null reason */
#define NETRES_MBUF	0x01	/* mbuf */
#define NETRES_QFULL	0x02	/* queue full */
#define NETRES_QEMPTY	0x03	/* queue empty */
#define NETRES_SBFULL	0x04	/* socket buffer full */
#define NETRES_SBEMPTY	0x05	/* socket buffer empty */
#define NETRES_CKSUM	0x06	/* checksum */
#define NETRES_SIZE	0x07	/* bad size packet */
#define NETRES_HEADER	0x08	/* bad packet header */
#define NETRES_FRAG	0x09	/* duplicated fagment */
#define NETRES_SYSCALL	0x0a	/* system call */
#define NETRES_INT	0x0b	/* interrupt */
#define NETRES_ERROR	0x0c	/* error */
#define NETRES_PROTCALL	0x0d	/* protocol call */
#define NETRES_PROTRET	0x0e	/* protocol return */
#define NETRES_UNREACH	0x0f	/* unreachable */
#define NETRES_CONN	0x10	/* connect call */
#define NETRES_CONNDONE	0x11	/* connect done */
#define NETRES_OFFSET	0x12	/* bad offset */
#define NETRES_NOTOURS	0x13	/* forwarding datagram */

#define	NETRES_RFSNULL	0x14
#define	NETRES_GETATTR	0x15
#define	NETRES_SETATTR	0x16
#define	NETRES_ROOT	0x17
#define	NETRES_LOOKUP	0x18
#define	NETRES_READLINK	0x19
#define	NETRES_READ	0x1a
#define	NETRES_WRTCACHE	0x1b
#define	NETRES_WRITE	0x1c
#define	NETRES_CREATE	0x1d
#define	NETRES_REMOVE	0x1e
#define	NETRES_RENAME	0x1f
#define	NETRES_LINK	0x20
#define	NETRES_SYMLINK	0x21
#define	NETRES_MKDIR	0x22
#define	NETRES_RMDIR	0x23
#define	NETRES_READDIR	0x24
#define	NETRES_STATFS	0x25
#define	NETRES_NPROC	0x26

#define NETRES_SBLOCK	0x27	/* sb lock */
#define NETRES_SBUNLOCK	0x28	/* sb unlock */
#define NETRES_SBFLUSH	0x29	/* sb flush */
#define NETRES_TIMO	0x29	/* sb flush */
#define NETRES_BUSY	0x30	/* busy */

/* netevent subtoken */
#define NETEVENT_LINKUP		0x00
#define NETEVENT_LINKDOWN	0x01
#define NETEVENT_IPUP		0x02
#define NETEVENT_IPDOWN		0x03
#define NETEVENT_UDPUP		0x04
#define NETEVENT_UDPDOWN	0x05
#define NETEVENT_TCPUP		0x06
#define NETEVENT_TCPDOWN	0x07
#define NETEVENT_SOUP		0x08
#define NETEVENT_SODOWN		0x09
#define NETEVENT_NFSUP		0x0a
#define NETEVENT_NFSDOWN	0x0b
#define NETEVENT_RAWUP		0x0c
#define NETEVENT_RAWDOWN	0x0d
#define NETEVENT_ARPUP		0x0e
#define NETEVENT_ARPDOWN	0x0f
#define NETEVENT_RPCUP		0x10
#define NETEVENT_RPCDOWN	0x11
#define NETEVENT_DDNUP		0x12
#define NETEVENT_DDNDOWN	0x13

/* misc for netevent */
#define NETCNT_NULL	((char)-99)
#define NETPID_NULL	((char)-98)

/* VM fault subtoken */
#define VMEVENT_VFAULT_FILE 0
#define VMEVENT_VFAULT_SWAP 1
#define VMEVENT_VFAULT_CACHE 2
#define VMEVENT_VFAULT_ZERO 3
#define VMEVENT_VFAULT_FAIL 4
#define VMEVENT_PFAULT_MOD 5
#define VMEVENT_PFAULT_COPY 6
#define VMEVENT_PFAULT_STEAL 7
#define VMEVENT_PFAULT_FAIL 8
#define VMEVENT_SWAPCHUNK_WAIT 9
#define VMEVENT_SWAPCHUNK_START 10
#define VMEVENT_SWAP_PAGE_START 11
#define VMEVENT_SWAP_PAGE_DONE 12
#define VMEVENT_SWAPCHUNK_GROUP 13
#define VMEVENT_VFAULT_FILE_START 14
#define VMEVENT_VFAULT_SWAP_START 15
#define VMEVENT_DISK_QUEUED 16
#define VMEVENT_DISK_START 17
#define VMEVENT_DISK_DONE 18
#define VMEVENT_WS_SWAPOUT_START 19
#define VMEVENT_WS_SWAPOUT_DONE 20
#define VMEVENT_WS_SWAPIN_START 21
#define VMEVENT_WS_SWAPIN_DONE 22
#define VMEVENT_WS_SWAPOUT_SCHED 23
#define VMEVENT_WS_SWAPIN_SCHED 24

#define VMEVENT_FLAG(vc) (1 << (vc))

#define VMEVENT_DEFAULT_SET (VMEVENT_FLAG(VMEVENT_VFAULT_FILE) | \
			     VMEVENT_FLAG(VMEVENT_VFAULT_SWAP) | \
			     VMEVENT_FLAG(VMEVENT_VFAULT_CACHE) | \
			     VMEVENT_FLAG(VMEVENT_VFAULT_ZERO) | \
			     VMEVENT_FLAG(VMEVENT_VFAULT_FAIL) | \
			     VMEVENT_FLAG(VMEVENT_PFAULT_MOD) | \
			     VMEVENT_FLAG(VMEVENT_PFAULT_COPY) | \
			     VMEVENT_FLAG(VMEVENT_PFAULT_STEAL) | \
			     VMEVENT_FLAG(VMEVENT_PFAULT_FAIL) | \
			     VMEVENT_FLAG(VMEVENT_SWAPCHUNK_WAIT) | \
			     VMEVENT_FLAG(VMEVENT_SWAPCHUNK_START) | \
			     VMEVENT_FLAG(VMEVENT_SWAP_PAGE_START) | \
			     VMEVENT_FLAG(VMEVENT_SWAP_PAGE_DONE) | \
			     VMEVENT_FLAG(VMEVENT_SWAPCHUNK_GROUP) | \
			     VMEVENT_FLAG(VMEVENT_VFAULT_FILE_START) | \
			     VMEVENT_FLAG(VMEVENT_VFAULT_SWAP_START) | \
			     VMEVENT_FLAG(VMEVENT_DISK_QUEUED) | \
			     VMEVENT_FLAG(VMEVENT_DISK_START) | \
			     VMEVENT_FLAG(VMEVENT_DISK_DONE) | \
			     VMEVENT_FLAG(VMEVENT_WS_SWAPOUT_START) | \
			     VMEVENT_FLAG(VMEVENT_WS_SWAPOUT_DONE) | \
			     VMEVENT_FLAG(VMEVENT_WS_SWAPIN_START) | \
			     VMEVENT_FLAG(VMEVENT_WS_SWAPIN_DONE) | \
			     VMEVENT_FLAG(VMEVENT_WS_SWAPOUT_SCHED) | \
			     VMEVENT_FLAG(VMEVENT_WS_SWAPIN_SCHED))

#define VMEVENT_WS_SCHED_NORUN		0x0001
#define VMEVENT_WS_SCHED_LOTSOFMEM	0x0002
#define VMEVENT_WS_SCHED_OLD		0x0004
#define VMEVENT_WS_SCHED_SLOAD		0x0008
#define VMEVENT_WS_SCHED_SXBRK		0x0010

#ifdef _KERNEL
struct proc;
struct sysent;
extern int trflags;
extern void fawlty_exit(void);
extern void fawlty_fork(void);
extern void fawlty_rcvsig(int);
extern void fawlty_rename(void);
extern void fawlty_sendsig(int, pid_t);
extern void fawltyidle(char, int);
extern void fawltynetevent(char, pid_t, char, int, char);
extern void fawltysched(char, struct proc *, char, char, int);
extern void fawltysys(short, struct sysent *, sysarg_t *);
extern void fawltysysend(short, struct sysent *, sysarg_t *, long, long, int);

#ifdef NET_DEBUG
#define NETPAR(f,t,p,s,l,r) {if (trflags&(f)) fawltynetevent(t, p, s, l, r);}
#else
#define NETPAR(f,t,p,s,l,r)
#endif /* NET_DEBUG */

#ifdef _VMFAULT_TRACING
#ifdef _KERNEL
#include <sys/vnode.h>
#include <sys/buf.h>
extern void fawltyvmfault(pid_t,char,caddr_t,caddr_t,struct vnode *,pgno_t);
extern void fawltyvmdisk(pid_t,char,struct buf *);
extern int vmfaultflags;
#endif /* _KERNEL */
#define VMFAULTTRACE(sc,va,epc,vp,offset) \
			     {if ((trflags & VMFAULTHIST) && \
				  (vmfaultflags & VMEVENT_FLAG(sc))) \
					  fawltyvmfault(u.u_procp->p_pid,\
							(sc),(va),(caddr_t) (epc),\
							(vp),(offset));}
#define VMSWAPTRACE(pid,sc,va,epc,vp,offset) \
			     {if ((trflags & VMFAULTHIST) && \
				  (vmfaultflags & VMEVENT_FLAG(sc))) \
					  fawltyvmfault((pid), \
							(sc),(va),(caddr_t) (epc),\
							(vp),(offset));}
#define VMSWAPSTATE(sc) \
		     {if ((trflags & VMFAULTHIST) && \
			  (vmfaultflags & VMEVENT_FLAG(sc))) \
				  fawltyvmfault(0, \
						(sc),NULL,NULL,NULL,0);}
#define VMDISKSTATE(sc,bp) \
		     {if ((trflags & VMFAULTHIST) && \
			  (vmfaultflags & VMEVENT_FLAG(sc))) \
				  fawltyvmdisk(0, (sc),(bp));}

#ifdef _VM_WS_SWAPPING
#define VM_WS_SWAP_TRACE(sc,pp,count) \
	     {if ((trflags & VMFAULTHIST) && \
		  (vmfaultflags & VMEVENT_FLAG(sc))) \
			  fawltyvmfault((pp)->p_pid,\
				(sc), \
				(caddr_t) ((pp)->p_size + \
					    ((pp)->p_shaddr != NULL \
					     ? (pp)->p_shaddr->s_totpregsize \
					     : 0)), \
				(caddr_t) ((pp)->p_rss),\
				NULL,(count));}
#endif /* _VM_WS_SWAPPING */
#else
#define VMFAULTTRACE(sc,va,epc,vp,offset) { 0; }
#define VMSWAPTRACE(pid,sc,va,epc,vp,offset) { 0; }
#define VMSWAPSTATE(sc) { 0; }
#define VMDISKSTATE(sc,bp) { 0; }
#ifdef _VM_WS_SWAPPING
#define VM_WS_SWAP_TRACE(sc,pp,count) { 0; }
#endif /* _VM_WS_SWAPPING */
#endif

/* fault trace flags - kept in trflags */
#define SYSCALLINH	0x02	/* inherit syscall trace on fork */
#define SYSCALLHIST	0x04	/* trace system calls */
#define RESCHEDHIST	0x08	/* trace rescheds */
#define ALLPROCSHIST	0x20	/* track all processes */
#define NETSCHED	0x40	/* trace network scheduling */
#define NETFLOW		0x80	/* trace network data flow */
#define VMFAULTHIST	0x100	/* trace vfault and pfault activity */

#endif /* _KERNEL */

/*
 * Fawlty record definitions kernel -> padc
 */

/*
 * Fawlty trace buffer structure
 * We work in units of fitem_t's for simplicity ...
 */
typedef uint fitem_t;

/* # of units in a 16K buffer */
#define FAWLTMAX	(((16*1024) / sizeof(fitem_t)) - 2)
struct fawltybuf {
	fitem_t	stat;		/* current status of this buffer */
	fitem_t	len;		/* length of current contents in ints */
	fitem_t	buf[FAWLTMAX];	/* buffer contents */
};

#define TOKENSIZ	8	/* each fawlty rec contains 8 bit token field */

/*
 * Net event record: used by NETEVENTKN, NETSLEEPTKN, NETWAKEUPTKN, 
 * NETWAKINGTKN, NETFLOWTKN, NETDROPTKN, and NETEVENTDONETKN tokens.
 */
struct neteventrec {
	short	pid;
	unchar	subtkn;
	unchar	token;
	uint	counts;
	ushort 	pad;
	unchar	reasons;
	unchar 	cpu;
	uint	tick;
};
typedef struct neteventrec neteventrec_t;

/*
 * VM fault event record: used by VMFAULTTKN
 */
struct vmfaultrec {
	short	pid;
	unchar	subtkn;
	unchar	token;
	caddr_t	vaddr;
	caddr_t	epc;
	uint	tick;
	unchar	cpu;
	unchar	pad1;
	ushort	pad2;
	dev_t	dev;
	ino_t	ino;
	pgno_t	offset;
};
typedef struct vmfaultrec vmfaultrec_t;

/*
 * Schedule record: used by OLDPROCTKN, NEWPROCTKN, and RUNQTKN tokens.
 */
struct schedrec {
	short	pid;
	unchar	pri;
	unchar	token;
	uint	reasons;
	ushort	pad;
	unchar	rtpri;
	unchar	cpu;
	void	*event;
	void	*event2;
	uint	tick;
};
typedef struct schedrec schedrec_t;

/*
 * Idle record: used by IDLEPROCTKN token.
 */
struct idlerec {
	short	cpu;
	unchar	reasons;
	unchar	token;
	uint	tick;
};
typedef struct idlerec idlerec_t;

/*
 * System call record: used by SYSCALLTKN/SYSRETTKN token.
 */

#define FAWLTY_MAX_PARAMS	8
/*
 * NOTE: kernel and padc code depends upon the fact that the size of the
 * callrec struct preceeding the params array may be copied by integer
 * (that is, 4,8,12, etc. bytes long).  All hell will break loose if a
 * field is added before params that makes it byte-aligned (if the compiler
 * permits it without rounding).
 *
 * Following the parameters are the indirect parameters:
 *	descriptor (short)
 *	length (short)
 *	value ..
 *
 * For variable length indirect parameters (like read/write) we don't
 * want infinite amounts of data - we start by limiting it to PAR_DEFINDBYTES
 * This can be changed up to PAR_MAXINDBYTES via an ioctl.
 * Note that this doesn't apply to strings.
 * The 4096 is somewhat arbitrary but expanding it much more will probably
 * require some changes to the buffering in prf.c
 */
#define PAR_DEFINDBYTES	32
#define PAR_MAXINDBYTES	4096
struct callrec {
	short	pid;
	unchar	numparams;
	unchar	token;
	ushort	callno;
	unchar	cpu;
	unchar	abi;
	uint	tick;
	unchar	numiparams;		/* number of indirect params */
	usysarg_t	params[1];	/* more of these ... */
};
typedef struct callrec callrec_t;

/*
 * Process-name record: used by NAMETKN.
 */
struct namerec {
	short pid;
	unchar pad;
	unchar token;
	unchar name[16];
};
typedef struct namerec namerec_t;

/*
 * Remove-process record: used by RMPROCTKN.
 */
struct rmprocrec {
	short	pid;
	ushort	token;
};
typedef struct rmprocrec rmprocrec_t;

/*
 * Signal record: used by SENDSIGTKN and RCVSIGTKN.
 */
struct sigrec {
	short	rcvpid;
	unchar	cpu;
	unchar	token;
	ushort	signo;
	uint	tick;
};
typedef struct sigrec sigrec_t;

/*
 * User supplied string record
 */
typedef struct ustringrec {
	short	pid;
	unchar	pad;
	unchar	token;
	char	string[64];
	uint	tick;
} ustringrec_t;

/*
 * an overflow record
 */
typedef struct {
	short	pad;
	unchar	pad2;
	unchar	token;
} overflowrec_t;

#ifdef __cplusplus
}
#endif
#endif /* __SYS_PAR_H__ */
