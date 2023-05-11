/**************************************************************************
 *									  *
 * 		 Copyright (C) 1986-1993 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

#ifndef __SYSSGI_H__
#define __SYSSGI_H__

#ifdef __cplusplus
extern "C" {
#endif

#ident "$Revision: 3.128 $"

#include <sys/types.h>

/*
** syssgi() system call commands.
*/

#define SGI_SYSID		1	/* get the system ID */
#define	SGI_TUNE		3	/* change tune values --
						see sys/tuneable.h */
#define	SGI_IDBG		4	/* access to internal debugger */
#define SGI_INVENT		5	/* hardware inventory */
#define SGI_RDNAME		6	/* get name of process given pid */
#define SGI_SETLED		7	/* set cpu board led state(s) */
#define SGI_SETNVRAM		8	/* set nvram */
#define SGI_GETNVRAM		9	/* get nvram */
#define SGI_QUERY_FTIMER        12
#define SGI_QUERY_CYCLECNTR	13
#define SGI_PROCSZ		14	/* recalc p_size, p_rsssize */

/* POSIX signal system calls */
#define SGI_SIGACTION		15
#define SGI_SIGPENDING		16
#define SGI_SIGPROCMASK	17
#define SGI_SIGSUSPEND		18
/* Other POSIX system calls */
#define SGI_SETSID	20
#define SGI_SETPGID	21
#define SGI_SYSCONF	22
#define SGI_WAIT4	23
#define SGI_PATHCONF	24


/* Block read & Block write system calls: to allow access to > 2 gig
 * of space on a disk or logical volume for large filesystems.
 */
#define SGI_READB	30
#define SGI_WRITEB	31

/* setgroups() and getgroups() system calls */
#define SGI_SETGROUPS	40
#define SGI_GETGROUPS	41

#define SGI_SETTIMEOFDAY	52	/* set time including microseconds */
#define SGI_SETTIMETRIM		53	/* change crystal trimmer */
#define SGI_GETTIMETRIM		54
#define SGI_SPROFIL		55	/* sprofil(2) entry point */
#define SGI_RUSAGE		56	/* Berkeley's getrusage(2) */
#define SGI_SIGSTACK		57	/* Berkeley's sigstack(2) */
#define SGI_SIGSTATUS		58	/* sgi call to support BSD longjmp */
#define SGI_NETPROC		59	/* start network process */
#define SGI_SIGALTSTACK		60	/* SVR4 sigaltstack(2) */
#define SGI_BDFLUSHCNT		61	/* change bdflushcnt */
#define SGI_SSYNC		62	/* synchronous sync */
#define SGI_NFSCNVT		63	/* convert file handle to descriptor */
#define SGI_GETPGID             64      /* SVR4 getpgid */
#define SGI_GETSID              65      /* SVR4 getsid */
#define SGI_IOPROBE		66	/* I/O probing */
#define SGI_CONFIG		67	/* get configuration data */
#define SGI_ELFMAP		68	/* atomically mmap in an elf dso */
#define SGI_MCONFIG		69	/* loadable module configuration */

/*
 * Trusted IRIX system calls
 */
/* mandatory access (MAC) */
/*
 * XXX: the next four interfaces should be obsoleted by the more
 *	general SGI_EAG_[SG]ET... interfaces.
 */
#define SGI_GETPLABEL		70	/* Get process label */
#define SGI_SETPLABEL		71	/* Set process label */
#define SGI_GETLABEL		72	/* Get file label */
#define SGI_SETLABEL		73	/* Set file label */
/* audit (SAT) */
#define SGI_SATREAD		74	/* Read audit rec from the kernel */
#define SGI_SATWRITE		75	/* Write audit rec to the kernel */
#define SGI_SATCTL		76	/* Control/query the audit stream */
/* extended attribute handling */
#define SGI_LOADATTR		77	/* Load attributes database */
#define SGI_UNLOADATTR		78	/* Unload attributes database */
/* trusted sockets */
#define SGI_RECVLUMSG		79	/* recvmsg() with label and uid */
/* more extended attribute handling */
#define SGI_PLANGMOUNT		80	/* mount() with PlanG info */
/* trusted sockets with DAC */
#define SGI_GETPSOACL		81	/* Get process socket ACL. */
#define SGI_SETPSOACL		82	/* Set process socket ACL.  */
/* still more extended attribute handling */
#define SGI_EAG_GETATTR		83	/* Get the named file attribute */
#define SGI_EAG_SETATTR		84	/* Set the named file attributes */
#define SGI_EAG_GETPROCATTR	85	/* Get the named process attribute */
#define SGI_EAG_SETPROCATTR	86	/* Set the named process attributes */
#define SGI_REVOKE		87	/* Revoke access to a device */
#define SGI_FREVOKE		SGI_REVOKE /* Poor name choice from 4.0.5TRIX */
/*reserved			88*/
/*reserved			89*/
#define SGI_SBE_GET_INFO        98      /* get SBE count on a mc3 board*/
#define SGI_SBE_CLR_INFO        99      /* clear SBE count on a mc3 board */
#define SGI_RMI_FIXECC		100	/* RMI read ECC-correct on/off (1/0) */
#define SGI_R4K_CERRS		101	/* return set of ECC cache-err cnts */
#define SGI_GET_EVCONF		102	/* Get the "evconfig" structure
					 * built by the IP19 prom. */

#define SGI_MPCWAROFF		103	/* turn off libmpc WAR handling */
#define SGI_SET_AUTOPWRON	104	/* set auto power on time */
#define SGI_SPIPE		105	/* set stream pipe */
#define SGI_SYMTAB		106	/* get runtime symtab info */
#define	SGI_SET_FP_PRECISE	107	/* set/clear precise FP exception mode*/
#define SGI_TOSSTSAVE		108	/* toss any saved pregions */
#define SGI_FDHI		109	/* return highest valid fd */
#define	SGI_SET_CONFIG_SMM	110	/* set/clear sequential memory mode */
#define	SGI_SET_FP_PRESERVE	111	/* preserve p_fpflags across exec */
#define SGI_MINRSS		112	/* minrss */
#define	SGI_GRIO		113	/* guaranteed rate I/O */
#define	SGI_XLV_SET_TAB		114	/* set incore logical volume config */
#define	SGI_XLV_GET_TAB		115	/* get incore logical volume config */
#define	SGI_GET_FP_PRECISE	116	/* get precise FP exception mode */
#define	SGI_GET_CONFIG_SMM	117	/* get sequential memory mode */
#define	SGI_FP_IMPRECISE_SUPP	118	/* does hw support imprecise mode? */
#define	SGI_CONFIG_NSMM_SUPP	119	/* does hw support non-seq mem mode? */
					/* 120 and 121 are available */

/* Frame Scheduler Timestamping Control */
						
#define SGI_RT_TSTAMP_CREATE    122 /* create timestamping buffer for specific cpu */
#define SGI_RT_TSTAMP_DELETE    123 /* delete timestamping buffer */
#define SGI_RT_TSTAMP_START     124 /* start logging timestamps */
#define SGI_RT_TSTAMP_STOP      125 /* stop logging timestamps */
#define SGI_RT_TSTAMP_ADDR      126 /* get physical addr for timestamp buffer */
#define SGI_RT_TSTAMP_MASK      127 /* set tstamp mask */
#define SGI_RT_TSTAMP_EOB_MODE  128 /* set end-of-buffer action */
					
#define	SGI_USE_FP_BCOPY	129	/* should bcopy/bzero use fp? */

#define SGI_GET_UST		130	/* get unadjusted system time value */

#define SGI_SPECULATIVE_EXEC	131	/* turn speculative execution on/off */

#define SGI_XLV_NEXT_RQST	132	/* wait for next xlv configuration
					   request. */
#define SGI_XLV_ATTR_CURSOR	133	/* get cursor for xlv attributes */
#define SGI_XLV_ATTR_GET	134	/* get xlv attribute value */
#define SGI_XLV_ATTR_SET	135	/* set xlv attribute */

/* btool - code coverage - only used when compiled with -DBTOOL */
#define SGI_BTOOLSIZE		136
#define SGI_BTOOLGET		137
#define SGI_BTOOLREINIT		138

#define	SGI_CREATE_UUID		139	/* create a DCE-defined UUID */

/* disable CSR_EXCEPT while in GL address space */
#define SGI_NOFPE		140	/* disable CSR_EXCEPT */

#define SGI_OLD_SOFTFP		141	/* use old (asm) softfp code */
#define	SGI_FS_INUMBERS		142	/* xfs get inode number table */
#define	SGI_FS_BULKSTAT		143	/* xfs get stat64 info in bulk */

/* more Frame Scheduler calls */
#define SGI_RT_TSTAMP_WAIT	144	/* wait for tstamp buffer to reach 2/3 watermark */
#define SGI_RT_TSTAMP_UPDATE    145     /* update fifo buffer head index */

/* things needed by xFS dump and xFS-based DMIG interfaces */
#define	SGI_PATH_TO_HANDLE	146	/* get a file's file handle	   */
#define	SGI_PATH_TO_FSHANDLE	147	/* get a file's file system handle */
#define	SGI_FD_TO_HANDLE	148	/* get an open file's file handle  */
#define	SGI_OPEN_BY_HANDLE	149	/* open a file given a file handle */
#define	SGI_READLINK_BY_HANDLE	150	/* read a link using a file handle */

#define SGI_READ_DANGID		151	/* Probe for Dang existance */

/* Sizing constants used by kmem readers */
#define SGI_CONST		152	/* System sizing constants */
#define SGI_XFS_FSOPERATIONS	153	/* entry of xfs extended operations */


/* Extended accounting functions */
#define SGI_SETASH		154	/* set array session handle */
#define SGI_GETASH		155	/* get array session handle */
#define SGI_SETPRID		156	/* set project ID */
#define SGI_GETPRID		157	/* get project ID */
#define SGI_SETSPINFO		158	/* set service provider info */
#define SGI_GETSPINFO		159	/* get service provider info */
#define	SGI_SHAREII		160	/* ShareII product syscall */
#define SGI_NEWARRAYSESS	161	/* start new array session */
#define SGI_GETDFLTPRID		162	/* get system default project ID */
#define	SGI_SET_DISMISSED_EXC_CNT 163	/* set dismissed exception count */
#define	SGI_GET_DISMISSED_EXC_CNT 164	/* get dismissed exception count */
/* More cycle counter support */
#define SGI_CYCLECNTR_SIZE	165	/* Size user needs to use to read CC */
#define SGI_QUERY_FASTTIMER	166	/* period of fast itimers in ns */
#define SGI_PIDSINASH		167	/* List PIDs in given array session */
#define SGI_ULI			168

/* support for large pages */
#define SGI_LPG_SHMGET          169     /* DBA: use large pages for shared mem */
#define SGI_LPG_MAP             170     /* DBA: use large pages for prog segment */

#define SGI_CACHEFS_SYS		171		/* CacheFS system call */
#define SGI_NFSNOTIFY		172		/* lockd client/server failure */
#define SGI_LOCKDSYS		173		/* set lockd options & client name */

/*
 * Performance monitoring calls
 */
#define SGI_EVENTCTR            174
#define SGI_GETPRUSAGE          175

#define	SGI_PROCMASK_LOCATION	176     /* Used to move p_hold into prda */

#define	SGI_UNUSED		177

#define	SGI_CKPT_SYS		178     /* checkpoint/restart system call */
#define	SGI_GETGRPPID		179     /* return a list of pid's for a given group */
#define	SGI_GETSESPID		180     /* return a list of pid's for a given session */

#define SGI_ENUMASHS		181	/* return a list of all active ASHs */
#define SGI_SETASMACHID		182	/* set array machine ID */
#define SGI_GETASMACHID		183	/* get array machine ID */
#define SGI_GETARSESS		184	/* retrieve arsess info */
#define SGI_JOINARRAYSESS	185	/* join existing array session */

#define SGI_SPROC_KILL		186	/* Send a sync. signal to a sproc */

#define SGI_DBA_CONFIG		187	/* DBA: get/set database accelerator features */


/* similar to uname() system call, but returns "official name" for hardware
 * specific releases from the base release.  Takes a count and pointer
 * to a buffer in which to place the name.  name will be no more than 256
 * characters.
*/
#define SGI_RELEASE_NAME	188
#define SGI_SYNCH_CACHE_HANDLER 189
#define SGI_SWASH_INIT		190	/* SoftWindows Address Space Helper */

/*
 * Numa Migration
 */

#define SGI_NUMA_MIGR_PAGE	200	/* migrate a page */
#define SGI_NUMA_MIGR_PAGE_ALT	201	/* migrate a page of other processes */

#define SGI_KAIO_USERINIT	202	/* DBA: kernel asyncio process initialization */
#define SGI_KAIO_READ		203	/* DBA: kernel asyncio read request */
#define SGI_KAIO_WRITE		204	/* DBA: kernel asyncio write request */
#define SGI_KAIO_SUSPEND	205	/* DBA: kernel asyncio nap till N I/Os complete */
#define SGI_KAIO_STATS		206	/* DBA: kernel asyncio stats collection */


#define SGI_INITIAL_PT_SPROC	213	/* make initial proc a pthread sproc */

/*
 * The following are needed for
 * implementing syssgi() calls
 * involving very large pages.
 */
/* values for arg4 of SGI_LPG_SHMGET */
#define SGI_LPG_256K            1
#define SGI_LPG_1M              2
#define SGI_LPG_4M              3
#define SGI_LPG_16M             4

/* flag (arg5 of SGI_LPG_SHMGET) indicating tlbwire or not */
#define SGI_LPG_WIRE            1


/* the syssgi() SGI_RMI_FIXECC entrypoint toggles the RMI ecc
 * read-correction. */
#define RMI_ECC_OFF		0
#define RMI_ECC_ON		1

#if (defined(R4000) && defined(_FORCE_ECC))
/* Debug stuff uses numbers up around 1000 */
#define SGI_FORCE_ECC		1010	/* debug - force ecc errors */

enum force_ecc_where { IN_PD, IN_PI, IN_SD, IN_SI, IN_MEM, IN_MEM_IO3 };

/* each double-word in memory has an 8-bit ECC checkbit value that
 * is computed and stored with it. */
typedef struct ecc_data_word {
    uint hi_word;
    uint lo_word;
    u_char ecc_val;
} ecc_data_word_t;
#endif /* R4K and _FORCE_ECC */

#define SGI_PHYSP		1011	/* get phys pgno for vaddr */

/*
 * kernel threads temporary
 */

#define	SGI_KTHREAD		1012

/*reserved			1020*/

/*
 * kernel pathconf() must know whether called by fpathconf or 
 * pathconf--i.e how to interpret the 1st parameter.  (It is not
 * currently used at all, but may be in the future.)
 */
#define PATHCONF	1
#define FPATHCONF	2


#define	GET_SYSID	SGI_SYSID	/* compatability */
#define	MAXSYSIDSIZE	64	/* maximum size in bytes of the system id   */

/* hardware inventory options */
#define SGI_INV_SIZEOF	1	/* get sizeof inventory struct */
#define SGI_INV_READ	2	/* read inventory table */

/* configuration data options */
#define ADAP_READ 	1
#define ADD_NODE  	2
#define DELETE_NODE  	3
#define GET_NODE  	4

/* IO probe directions */
#define IOPROBE_READ	0
#define IOPROBE_WRITE	1

/* nvram options */
#define SGI_NVSTRSIZE	128	/* Not perfect, but reasonable size. */

/* SGI_MINRSS options */
#define MINRSS_ADDPNAME		1
#define MINRSS_DELPNAME		2
#define MINRSS_LISTPNAME	3
#define MINRSS_ADDVNODE		4
#define MINRSS_DELVNODE		5
#define MINRSS_LISTVNODE	6
/* struct for LISTPNAME */
#define MINRSS_PNAMESZ		20
struct getpname {
	char g_name[MINRSS_PNAMESZ];
	pgno_t g_minrss;
};
/* struct for LISTVNODE */
struct getvnode {
	dev_t g_fsid;
	ino_t g_nodeid;
	pgno_t g_minrss;
};

#ifdef _KERNEL
struct irix5_getvnode {
	__int32_t g_fsid;
	__uint32_t g_nodeid;
	__int32_t g_minrss;
};
#endif

/* EXTENDED MEMORY operations (though sgifastpath syscall)
 * Intended to be used by the HIPPI controller to provide a fastpath for
 * low latency memory operations.  Device driver will specify a range of
 * addresses which the user can read/write using the following syssgi calls.
 */
#define SGIFAST_PIOMEM_BREAD32	0	/* extended memory block read */
#define	SGIFAST_PIOMEM_BWRITE32	1	/* extended memory block write */
#define	SGIFAST_PIOMEM_NULL	2	/* extended memory NOP */

/*
 * SGI_CONST parameters
 */
#define SGICONST_MBUF		1	/* Mbuf */
#define SGICONST_PTE		2	/* Pte */
#define SGICONST_PAGESZ		3	/* _PAGESZ */
#define SGICONST_PARAM		4	/* Param */

/* SGI_ULI options */
enum {
    /* These options are used from the top half of a process, i.e. they
     * are ordinary system calls
     */

    ULI_SLEEP,
    ULI_DEBUG,
    ULI_SET_DEBUG_SIG,

    /* These options are used from the bottom half of the process, i.e.
     * they are interrupt mode calls
     */

    ULI_RETURN,
    ULI_CPUID,
    ULI_WAKEUP,
    ULI_CONWRITE,

    ULI_MAXCALL
};

/*
 * SGI_PROCMASK_LOCATION options
 */
#define USER_LEVEL	1
#define KERNEL_LEVEL	2

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
#ifndef _KERNEL
#include <stddef.h>
extern ptrdiff_t syssgi(int, ...);
#endif /* !_KERNEL */
#endif

#if _KERNEL
#include <sys/systm.h>

struct syssgia {
	sysarg_t cmd;
	sysarg_t arg1, arg2, arg3, arg4, arg5, arg6, arg7;
};

extern int syssgi(struct syssgia *, rval_t *);

/* procscan parameters for findash */
struct findashinfo {
	ash_t	ash;
	pid_t	*useraddr;
	int	usermax;
	int	count;
	int	current;
	int	errno;
};
#endif	/* _KERNEL */

#ifdef __cplusplus
}
#endif
#endif /* __SYSSGI_H__ */
