/*
 *
 * Copyright 1988,1992, Silicon Graphics, Inc.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
 * the contents of this file may not be disclosed to third parties, copied or
 * duplicated in any form, in whole or in part, without the prior written
 * permission of Silicon Graphics, Inc.
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
 * and Computer Software clause at DFARS 252.227-7013, and/or in similar or
 * successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
 * rights reserved under the Copyright Laws of the United States.
 */


#ifndef	__SYS_SAT_H
#define	__SYS_SAT_H
#ifdef __cplusplus
extern "C" {
#endif

#ident "$Revision: 1.42 $"

#include <sys/types.h>
#ifdef _KERNEL
#include <sys/systm.h>	/* for rval_t */
#include <sys/vnode.h>	/* for enum symfollow */
#endif /* _KERNEL */
#include <sys/mac_label.h>


	/*********************/
	/*	Typdefs      */
	/*********************/

typedef struct mbuf sat_rec_t;		/* for audit records */
typedef struct mbuf sat_pn_t;		/* for sat_pathname records */


	/************************/
	/*	Prototypes      */
	/************************/

#ifdef _KERNEL
/* Prototypes for sat kernel interfaces */

struct pathname;
extern void	sat_init( void );
extern sat_rec_t *sat_alloc( int );
extern sat_pn_t *sat_pnalloc( int );
extern sat_rec_t *sat_kern_alloc( int );
extern void	sat_enqueue( sat_rec_t *, int );
extern void	sat_kern_enqueue( sat_rec_t * );
extern void	sat_update_rwdir( sat_rec_t *, int );
extern void	sat_pn_init( sat_pn_t *, struct pathname *, int );
extern void	sat_pn_finalize( struct vnode * );
extern int	sat_lookup( char *, enum symfollow );
extern void	sat_mb_align( struct mbuf * );
extern void	sat_mcat( struct mbuf *, struct mbuf * );
extern int	sat_mcat_data( int (*)(void *, void *, int),
			       struct mbuf *, char *, int );
extern void	sat_mcatstr( struct mbuf *, char * );
extern int	sat_read( char *, unsigned, rval_t * );
extern int	sat_write( int, int, char *, unsigned );
extern int	sat_ctl( int, int, pid_t, rval_t * );

/* Prototypes for "audit points" */

struct ifnet;
struct ifreq;
struct inpcb;
struct ip;
struct pollfd;
extern void sat_access( int, sat_pn_t *, int );
extern void sat_access2( int, sat_pn_t *, sat_pn_t *, int );
extern void sat_acct( char *, sat_pn_t *, int );
extern void sat_bsdipc_addr( int, void *, struct mbuf *, int );
extern void sat_bsdipc_create( short, void *, short, short, int );
extern void sat_bsdipc_create_pair( short, void *, short, short, short, void *,
				    int );
extern void sat_bsdipc_if_config( int, void *, int, struct ifreq *, int );
extern void sat_bsdipc_if_setlabel( int, void *, struct ifreq *, int, int );
extern void sat_bsdipc_mac_change( short, void *, mac_label *, int );
extern void sat_bsdipc_match( struct ip *, mac_label *, struct inpcb *, int );
extern void sat_bsdipc_missing( struct ifnet *, struct ip *, int );
extern void sat_bsdipc_range( struct ifnet *, struct ip *, mac_label *, int,
			      int );
extern void sat_bsdipc_resvport( int, void *, int, int );
extern void sat_bsdipc_shutdown( short, void *, short, int );
extern void sat_bsdipc_snoop( void *, mac_label *, int, int );
extern void sat_check_priv( int, int );
extern void sat_chmod( sat_pn_t *, int, int );
extern void sat_chown( sat_pn_t *, int, int, int );
extern void sat_chrwdir( sat_pn_t *, int );
extern void sat_clock( int, int );
extern void sat_close( int, int );
extern void sat_domainname_set( char *, int, int );
extern void sat_dup( int, int, int );
extern void sat_exec( sat_rec_t *, sat_pn_t *, sat_pn_t *, int, sat_pn_t **,
		      int );
extern void sat_exit( int, int );
extern void sat_fchdir( int, int );
extern void sat_fchmod( mode_t, int );
extern void sat_fchown( uid_t, gid_t, int );
extern void sat_fd_read( int, int );
extern void sat_fd_read2( fd_set *, int );
extern void sat_pfd_read2( struct pollfd *, int, int );
extern void sat_tty_setlabel( int, mac_label *, int );
extern void sat_fd_rdwr( int, int, int );
extern void sat_fork( pid_t, int );
extern void sat_hostid_set( long, int );
extern void sat_hostname_set( char *, int, int );
extern void sat_mount( sat_pn_t *, sat_pn_t *, dev_t, int );
extern void sat_open( int, int, int, sat_pn_t *, int );
extern void sat_pipe( int, int, int );
extern void sat_kill( int, pid_t, uid_t, uid_t, mac_label *, int );
extern void sat_proc_access( int, pid_t, uid_t, uid_t, mac_label *, int );
extern void sat_setgroups( int, gid_t *, int );
extern void sat_setlabel( sat_pn_t *, struct mac_label *, int );
extern void sat_setplabel( sat_rec_t *, mac_label *, int );
extern void sat_setregid( sat_rec_t *, gid_t, gid_t, int );
extern void sat_setreuid( sat_rec_t *, uid_t, uid_t, int );
extern void sat_svipc_access( mac_label *, int, int, int );
extern void sat_svipc_change( int, int, int, int, int, int, int, int );
extern void sat_svipc_create( key_t, int, int, int );
extern void sat_svipc_remove( int, int );
extern void sat_utime( sat_pn_t *, time_t *, time_t, time_t, int );
extern void sat_control( int, int, int, int );
#endif /* _KERNEL */

	/*********************/
	/*	Defines      */
	/*********************/

/*
 * Major and minor version numbers.  These identify the
 * "version" of the records in a file.
 */
#define	SAT_VERSION_MAJOR	2
#define	SAT_VERSION_MINOR	1
#define	SAT_FILE_MAGIC		"SGIAUDIT"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* handy macro */

#define	sat_skip_hdr(m,t)	mtod((m)->m_act,t)

/* maximum values */

#define SAT_MAX_RECORD		65535	/* maximum kernel record size */
#define SAT_MAX_USER_REC	 4000	/* maximum buffer to sat_write */
					/* (user-level auditing) */
/* SAT outcome bits */

#define SAT_UNDEFINED		0xff	/* should never show up in audit file*/
#define SAT_FAILURE		0x00	/* handy, redundant, says '~SUCCESS' */
#define SAT_SUCCESS		0x01	/* 1 = success, 0 = failure          */
#define SAT_DAC			0x02	/* 1 = dac affected outcome, 0 = not */
#define SAT_MAC			0x04	/* 1 = mac affected outcome, 0 = not */
#define SAT_PRIVILEGE		0x08	/* 1 = failed/succeded due to priv.  */

/* sat_ctl() commands */

#define SATCTL_AUDIT_ON		1
#define SATCTL_AUDIT_OFF	2
#define SATCTL_AUDIT_QUERY	3
#define SATCTL_SET_SAT_ID	4
#define SATCTL_GET_SAT_ID	5
#define SATCTL_LOCALAUDIT_ON	6
#define SATCTL_LOCALAUDIT_OFF	7
#define SATCTL_LOCALAUDIT_QUERY	8
#define SATCTL_REGISTER_SATD	9

/* u.u_suser values */
#define SAT_SUSER_CHECKED	0x01	/* we checked u.u_uid == 0 */
#define SAT_SUSER_POSSESSED	0x02	/* our u.u_uid was 0 when checked */

/* bitmask for audit events */

#define NSATBITS	(sizeof(unsigned long) * NBBY)	/* bits per mask */
#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif

#define	SAT_SET(n, p)	((p)->ev_bits[(n)/NSATBITS] |= (1 << ((n) % NSATBITS)))
#define	SAT_CLR(n, p)	((p)->ev_bits[(n)/NSATBITS] &= ~(1 << ((n) % NSATBITS)))
#define	SAT_ISSET(n, p) \
	(((p)->ev_bits[(n)/NSATBITS] & (1 << ((n) % NSATBITS))) != 0)
#define SAT_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

	/********************************/
	/*	Function macros		*/
	/********************************/

#ifdef _KERNEL
extern	int	sat_enabled;

/* syscalls */

#define _SAT_READ(a,b,c)	((sat_enabled)? sat_read(a,b,c): ENOPKG)
#define _SAT_WRITE(a,b,c,d)	((sat_enabled)? sat_write(a,b,c,d): ENOPKG)
#define _SAT_CTL(a,b,c,d)	((sat_enabled)? sat_ctl(a,b,c,d): ENOPKG)

/* misc kernel routines */
#define _SAT_ALLOC(a)		((sat_enabled)? sat_alloc(a): NULL)
#define _SAT_PNALLOC(a)		((sat_enabled)? sat_pnalloc(a): NULL)
#define _SAT_PN_INIT(a,b,c)	((sat_enabled)? sat_pn_init(a,b,c): (void)0)
#define _SAT_PN_FINALIZE(a)	((sat_enabled)? sat_pn_finalize(a): (void)0)

/* "audit points" */

#define	_SAT_ACCESS(a,b,c)	((sat_enabled)? sat_access(a,b,c): (void)0)
#define	_SAT_ACCESS2(a,b,c,d)	((sat_enabled)? sat_access2(a,b,c,d): (void)0)
#define	_SAT_ACCT(a,b,c)	((sat_enabled)? sat_acct(a,b,c): (void)0)
#define	_SAT_BSDIPC_ADDR(a,b,c,d)	\
			((sat_enabled)? sat_bsdipc_addr(a,b,c,d): (void)0)
#define	_SAT_BSDIPC_CREATE(a,b,c,d,e)	\
			((sat_enabled)? sat_bsdipc_create(a,b,c,d,e): (void)0)
#define	_SAT_BSDIPC_CREATE_PAIR(a,b,c,d,e,f,g)	\
			((sat_enabled)?	\
				sat_bsdipc_create_pair(a,b,c,d,e,f,g): (void)0)
#define	_SAT_BSDIPC_IF_CONFIG(a,b,c,d,e)	\
			((sat_enabled)? \
				sat_bsdipc_if_config(a,b,c,d,e): (void)0)
#define	_SAT_BSDIPC_IF_SETLABEL(a,b,c,d,e)	\
			((sat_enabled)?	\
				sat_bsdipc_if_setlabel(a,b,c,d,e): (void)0)
#define	_SAT_BSDIPC_MAC_CHANGE(a,b,c,d)	\
			((sat_enabled)? sat_bsdipc_mac_change(a,b,c,d): (void)0)
#define	_SAT_BSDIPC_MATCH(a,b,c,d)	\
			((sat_enabled)? sat_bsdipc_match(a,b,c,d): (void)0)
#define	_SAT_BSDIPC_MISSING(a,b,c)	\
			((sat_enabled)? sat_bsdipc_missing(a,b,c): (void)0)
#define	_SAT_BSDIPC_RANGE(a,b,c,d,e)	\
			((sat_enabled)? sat_bsdipc_range(a,b,c,d,e): (void)0)
#define	_SAT_BSDIPC_RESVPORT(a,b,c,d)	\
			((sat_enabled)? sat_bsdipc_resvport(a,b,c,d): (void)0)
#define	_SAT_BSDIPC_SHUTDOWN(a,b,c,d)	\
			((sat_enabled)? sat_bsdipc_shutdown(a,b,c,d): (void)0)
#define	_SAT_BSDIPC_SNOOP(a,b,c,d)	\
			((sat_enabled)? sat_bsdipc_snoop(a,b,c,d): (void)0)
#define	_SAT_CHECK_PRIV(a,b)	((sat_enabled)? sat_check_priv(a,b): (void)0)
#define	_SAT_CHMOD(a,b,c)	((sat_enabled)? sat_chmod(a,b,c): (void)0)
#define	_SAT_CHOWN(a,b,c,d)	((sat_enabled)? sat_chown(a,b,c,d): (void)0)
#define	_SAT_CHRWDIR(a,b)	((sat_enabled)? sat_chrwdir(a,b): (void)0)
#define	_SAT_CLOCK(a,b)		((sat_enabled)? sat_clock(a,b): (void)0)
#define	_SAT_CLOSE(a,b)		((sat_enabled)? sat_close(a,b): (void)0)
#define	_SAT_DOMAINNAME_SET(a,b,c)	\
			((sat_enabled)? sat_domainname_set(a,b,c): (void)0)
#define	_SAT_DUP(a,b,c)		((sat_enabled)? sat_dup(a,b,c): (void)0)
#define	_SAT_EXEC(a,b,c,d,e,f)	((sat_enabled)? sat_exec(a,b,c,d,e,f): (void)0)
#define	_SAT_EXIT(a,b)		((sat_enabled)? sat_exit(a,b): (void)0)
#define	_SAT_FCHDIR(a,b)	((sat_enabled)? sat_fchdir(a,b): (void)0)
#define	_SAT_FCHMOD(a,b)	((sat_enabled)? sat_fchmod(a,b): (void)0)
#define	_SAT_FCHOWN(a,b,c)	((sat_enabled)? sat_fchown(a,b,c): (void)0)
#define	_SAT_FD_READ(a,b)	((sat_enabled)? sat_fd_read(a,b): (void)0)
#define	_SAT_FD_READ2(a,b)	((sat_enabled)? sat_fd_read2(a,b): (void)0)
#define	_SAT_PFD_READ2(a,b,c)	((sat_enabled)? sat_pfd_read2(a,b,c): (void)0)
#define	_SAT_TTY_SETLABEL(a,b,c)	\
			((sat_enabled)? sat_tty_setlabel(a,b,c): (void)0)
#define	_SAT_FD_RDWR(a,b,c)	((sat_enabled)? sat_fd_rdwr(a,b,c): (void)0)
#define	_SAT_FORK(a,b)		((sat_enabled)? sat_fork(a,b): (void)0)
#define	_SAT_HOSTID_SET(a,b)	((sat_enabled)? sat_hostid_set(a,b): (void)0)
#define	_SAT_HOSTNAME_SET(a,b,c)	\
			((sat_enabled)? sat_hostname_set(a,b,c): (void)0)
#define	_SAT_MOUNT(a,b,c,d)	((sat_enabled)? sat_mount(a,b,c,d): (void)0)
#define	_SAT_OPEN(a,b,c,d,e)	((sat_enabled)? sat_open(a,b,c,d,e): (void)0)
#define	_SAT_PIPE(a,b,c)	((sat_enabled)? sat_pipe(a,b,c): (void)0)
#define	_SAT_KILL(a,b,c,d,e,f)	((sat_enabled)? sat_kill(a,b,c,d,e,f): (void)0)
#define	_SAT_PROC_ACCESS(a,b,c,d,e,f)	\
			((sat_enabled)? sat_proc_access(a,b,c,d,e,f): (void)0)
#define	_SAT_SETGROUPS(a,b,c)	((sat_enabled)? sat_setgroups(a,b,c): (void)0)
#define	_SAT_SETLABEL(a,b,c)	((sat_enabled)? sat_setlabel(a,b,c): (void)0)
#define	_SAT_SETPLABEL(a,b,c)	((sat_enabled)? sat_setplabel(a,b,c): (void)0)
#define	_SAT_SETREGID(a,b,c,d)	((sat_enabled)? sat_setregid(a,b,c,d): (void)0)
#define	_SAT_SETREUID(a,b,c,d)	((sat_enabled)? sat_setreuid(a,b,c,d): (void)0)
#define	_SAT_SVIPC_ACCESS(a,b,c,d)	\
			((sat_enabled)? sat_svipc_access(a,b,c,d): (void)0)
#define	_SAT_SVIPC_CHANGE(a,b,c,d,e,f,g,h)	\
				((sat_enabled)?	\
				    sat_svipc_change(a,b,c,d,e,f,g,h): (void)0)
#define	_SAT_SVIPC_CREATE(a,b,c,d)	\
			((sat_enabled)? sat_svipc_create(a,b,c,d): (void)0)
#define	_SAT_SVIPC_REMOVE(a,b)	((sat_enabled)? sat_svipc_remove(a,b): (void)0)
#define	_SAT_UTIME(a,b,c,d,e)	((sat_enabled)? sat_utime(a,b,c,d,e): (void)0)
#define	_SAT_CONTROL(a,b,c,d)	((sat_enabled)? sat_control(a,b,c,d): (void)0)
#endif /* _KERNEL */

	/***************************************/
	/*	Common record definitions      */
	/***************************************/

struct	sat_hdr {
    /* fixed-length portion of header (sizeof struct sat_hdr) */
	int	sat_magic;	/* sat header "magic number" */
	u_char	sat_rectype;	/* what type of record follows */
	u_char	sat_outcome;	/* failure/success, because of dac/mac check */
	u_char	sat_sequence;	/* sequence number for this record (by type) */
	u_char	sat_errno;	/* system call error number */
	time_t	sat_time;	/* seconds since 1970 */
	u_char	sat_ticks;	/* sub-second clock ticks (0-99) */
	u_char	sat_syscall;	/* system call number */
	u_short	sat_subsyscall;	/* system call "command" number */
	long	sat_host_id;	/* host id (new for format 2.0) */
	uid_t	sat_id;		/* SAT user-id */
	dev_t	sat_tty;	/* controlling tty, if present */
	pid_t	sat_ppid;	/* parent process id */
	pid_t	sat_pid;	/* process id of record's generator */
	uid_t	sat_euid;	/* Effective user id */
	uid_t	sat_ruid;	/* Real user id */
	gid_t	sat_egid;	/* Effective group id */
	gid_t	sat_rgid;	/* Real group id */
	u_short	sat_hdrsize;	/* total bytes in the header */
	u_short	sat_recsize;	/* bytes in the following record */
	u_short	sat_label_size;	/* how many bytes of label follow? */
	u_short	sat_cwd_len;	/* bytes of current working dir following */
	u_short	sat_root_len;	/* bytes of current root dir following */
	u_char	sat_glist_len;	/* number of multi-group entries */
	u_char	sat_pname_len;	/* process name length */
    /* variable-length portion: size = hdr->sat_hdrsize - sizeof(sat_hdr_t) */
	/* group list */
	/* process label */
	/* current working directory */
	/* current root directory */
	/* process name, from u.u_comm */
};

typedef struct	sat_hdr sat_hdr_t;

/*
 * Common pathname record.
 * Note: the data for the filenames immediately follows the record.
 */
struct	sat_pathname {
	ino_t sat_inode;
	dev_t sat_device;
	uid_t sat_fileown;
	gid_t sat_filegrp;
	mode_t sat_filemode;
	u_short sat_reqname_len;
	u_short sat_actname_len;
	/* char data[];	*/
	/* file label */
};

/*
 * The audit file header
 */
struct sat_filehdr {
	char	sat_magic[8];		/* == "SGIAUDIT" */
	u_char	sat_major;		/* version of audit data */
	u_char	sat_minor;
	u_char	sat_pad1[2];		/* alignment filler */
	time_t	sat_start_time;		/* time header written */
	time_t	sat_stop_time;		/* time file closed (added later) */
	long	sat_host_id;		/* host id */
	int	sat_mac_enabled: 1;	/* boolean: ignore mac fields or not */
	int	sat_total_bytes: 31;	/* number of bytes to skip past hdr */
	u_short	sat_user_entries;	/* number of sat_list_ent structs */
	u_short	sat_group_entries;	/*   in the user and group lists */
	u_short	sat_host_entries;	/*   and the hostid <-> name list */
	u_char	sat_timezone_len;	/* bytes of timezone string */
	u_char	sat_hostname_len;	/* bytes of hostname */
	u_char	sat_domainname_len;	/* bytes of domainname */
	u_char	sat_pad2[3];		/* alignment filler */
	/* TZ (timezone) (including trailing null) */
	/* hostname (including trailing null) */
	/* domainname (including trailing null) */
	/* user entries, each word aligned */
	/* group entries, each word aligned */
	/* hostid entries, each word aligned */
};

struct sat_list_ent {
	long	sat_id;			/* user/group/host id */
	union {				/* user/group/host name */
		u_short	len;		/* --file format */
		char	data[1];	/* --memory format */
	} sat_name;
	/* name (including trailing null) */
};

/*
 * used in the u-area to record current working and root
 * directories
 */
struct	sat_wd {
	u_short cwd_len;
	u_short root_len;
	char data[1];
	/* cwd */
	/* root */
};

	/**************************************/
	/*	Audit record definitions      */
	/**************************************/

/*
 * These records consist of nothing more than a pathname
 * record and thus have no struct definition:
 *
 *	sat_access_denied
 *	sat_access_failed
 *	sat_chrwdir
 *	sat_rd_symlink
 *	sat_file_crt_del
 *	sat_file_write
 *	sat_file_attr_read
 */

struct	sat_open {
	short sat_filedes;
	short sat_file_created;
	int sat_open_flags;
	/* sat_pathname */
};

struct	sat_mount {
	dev_t sat_fs_dev;
	short sat_npaths;
	/* sat_pathname device */
	/* sat_pathname mount_point */
};

struct	sat_file_attr_write {
	union {
		mode_t sat_filemode;		/* for chmod */
		struct {
			uid_t sat_fileown;
			gid_t sat_filegrp;
		} chown;			/* for chown */
		int sat_label_size;		/* for setlabel */
		struct {
			time_t sat_atime;
			time_t sat_mtime;
		} utime;			/* for utime */
	} newattr;
	/* label, if setlabel */
	/* sat_pathname */
};

struct	sat_exec {
	uid_t sat_euid;		/* effective could be set by setuid progs */
	gid_t sat_egid;		/* effective could be set by setgid progs */
	u_char sat_npaths;	/* number of pathname modules following */
	u_char sat_interpreter;	/* is there a shell interpreter? */
	/* sat_pathname */
	/* ... */
};

struct	sat_sysacct {
	/* was accounting turned on or off? TRUE=on, FALSE=off */
	int sat_acct_state;
};

struct	sat_fchdir {
	int sat_filedes;
};

struct	sat_fd_read {
	int sat_filedes;
};

struct	sat_fd_read2 {
	int sat_nfds;
	/* "short *" list of descriptors */
};

struct sat_tty_setlabel {
	u_short sat_filedes;
	u_short sat_label_size;
	/* tty label */
};

struct	sat_fd_write {
	int sat_filedes;
};

struct	sat_fd_attr_write {
	int sat_filedes;
	union {
		mode_t sat_filemode;		/* for fchmod */
		struct {
			uid_t sat_fileown;
			gid_t sat_filegrp;
		} fchown;			/* for fchown */
	} newattr;
};

struct	sat_pipe {
	short sat_read_filedes;
	short sat_write_filedes;
};

struct	sat_dup {
	short sat_old_filedes;
	short sat_new_filedes;
};

struct	sat_close {
	int sat_filedes;
};

struct	sat_fork {
	pid_t sat_newpid;
};

struct	sat_exit {
	int sat_exit_status;
};

struct	sat_proc_access {
	pid_t sat_pid;
	uid_t sat_ruid;
	uid_t sat_euid;
	int sat_signal;		/* kill only */
	/* process label */
};

struct	sat_proc_own_attr_write {
	union {
		struct {
			uid_t sat_euid;
			uid_t sat_ruid;
		} uid;				/* for setuid, setreuid */
		struct {
			gid_t sat_egid;
			gid_t sat_rgid;
		} gid;				/* for setgid, setregid */
		int sat_label_size;		/* for setplabel */
		int sat_glist_len;		/* for setgroups */
	} newattr;
	/* process label, if setplabel */
	/* group list, if setgroups */
};

struct	sat_svipc_access {
	int sat_svipc_id;
	u_short sat_label_size;
	u_short sat_svipc_perm;
	/* label */
};

struct	sat_svipc_create {
	int sat_svipc_id;
	key_t sat_svipc_key;
	u_short sat_svipc_perm;
};

struct	sat_svipc_remove {
	int sat_svipc_id;
};

struct	sat_svipc_change {
	int sat_svipc_id;
	uid_t sat_svipc_oldown;
	gid_t sat_svipc_oldgrp;
	u_short sat_svipc_oldperm;
	uid_t sat_svipc_newown;
	gid_t sat_svipc_newgrp;
	u_short sat_svipc_newperm;
};

struct	sat_bsdipc_create {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_comm_domain;
	short	sat_protocol;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_create_pair {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_comm_domain;
	short	sat_protocol;
	short	sat_second_dscr;
	void *	sat_second_socket;	/* second socket identifier	*/
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
	uid_t   sat_second_so_uid;
	uid_t   sat_second_so_rcvuid;
	short   sat_second_so_uidcount;
/*	int  *	sat_so_uidlist;		- allocated separately		*/
/*	int  *	sat_second_so_uidlist;	- allocated separately		*/
};

struct	sat_bsdipc_shutdown {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_how;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_mac_change {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	u_short sat_label_size;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	mac_label   sat_label;		-  New label on socket 		*/
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_dac_change {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_address {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_addr_len;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	char *	sat_socket_addr;	- allocated separately		*/
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_resvport {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_port;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

#define SATIFNAMSIZ 16
struct	sat_bsdipc_if_setlabel {
	void *	sat_socket;		/* socket identifier 		*/
	int	sat_socket_dscr;
	u_long	sat_doi;		/* domain of interpretation	*/
	u_char	sat_authority_max;	/* maximum authority allowed	*/
	u_char	sat_authority_min;	/* minimum authority permitted	*/
	u_char	sat_reserved;		/* must be zero until defined	*/
	u_char	sat_idiom;		/* security idiom		*/
	short	sat_maxlabel_len;
	short	sat_minlabel_len;
	char    sat_ifname[SATIFNAMSIZ];/* name of intended interface	*/
/*	mac_label   sat_label_max;	-  dominates all dgrams on if	*/
/*	mac_label   sat_label_min;	-  dominated by all if dgrams 	*/
};

struct	sat_bsdipc_if_setuid {
	void *	sat_socket;		/* socket identifier 		*/
	int	sat_socket_dscr;
	uid_t   sat_newuid;
	char    sat_ifname[SATIFNAMSIZ];/* name of intended interface	*/
};

struct	sat_bsdipc_if_config {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_ifreq_len;
	int	sat_ioctl_cmd;
/*	struct ifreq sat_ifreq;		- allocated separately		*/
};

struct	sat_bsdipc_match {
	void *	sat_socket;		/* socket identifier 		*/
	u_short sat_ip_len;
	u_short sat_label_len;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
	uid_t   sat_uid;		/* Uid on datagram 		*/
/*	struct ip sat_ip;		-  ip header & port numbers	*/
/*	mac_label   sat_label;		-  Label on datagram 		*/
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_dac_denied {
	void *	sat_socket;		/* socket identifier 		*/
	u_short sat_ip_len;
	u_short sat_label_len;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	struct ip sat_ip;		-  ip header & port numbers	*/
/*	mac_label   sat_label;		-  Label on datagram 		*/
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

struct	sat_bsdipc_snoop {
	void *	sat_socket;		/* socket identifier 		*/
					/* <<< NEED MORE HERE ??? >>>	*/
	u_short sat_label_len;
	uid_t   sat_so_uid;
	uid_t   sat_so_rcvuid;
	short   sat_so_uidcount;
/*	mac_label   sat_label;		-  Label on datagram 		*/
/*	int  *	sat_so_uidlist;		- allocated separately		*/
};

/* used for rx and tx out of range errors, and tx and rx OK */
struct	sat_bsdipc_range {  
	char    sat_ifname[SATIFNAMSIZ];/* name of interface		*/
	u_short sat_ip_len;
	u_short sat_label_len;
	uid_t   sat_uid;		/* Uid on datagram 		*/
/*	struct ip sat_ip;		-  ip datagram header (only!)	*/
/*	mac_label   sat_label;		-  Label on datagram 		*/
};

struct	sat_bsdipc_missing {  /* IP security option missing or malformed */
	char    sat_ifname[SATIFNAMSIZ];/* name of interface		*/
	u_short sat_ip_len;		/* length of following ip hdr	*/
/*	struct ip sat_ip;		-  ip datagram header & options	*/
};

struct	sat_clock_set {
	time_t sat_newtime;
};

/*
 * sat_hostname_set and sat_domainname_set consist of
 * nothing more than a null-terminated string and thus
 * have no struct definition.
 */

struct	sat_hostid_set {
	long sat_newhostid;
};

struct	sat_check_priv {
	int sat_priv_state;	/* did they possess superuser privilege? */
};

struct sat_control {
	int sat_cmd;		/* satctl command (e.g. SATCTL_AUDIT_ON) */
	int sat_arg;		/* argument (depends on command) */
	pid_t sat_pid;		/* pid (depends on command) */
};

	/*********************************/
	/*	System call numbers      */
	/*********************************/

/*
 * Selected system call numbers.  This is not intended to be
 * a complete list.  Add new entries on an as-needed basis.
 * (these must stay up to date with uts/mips/os/sysent.c)
 *
 * SAT_SYSCALL_KERNEL is for kernel-direct messages that are not
 * the result of some system call (timetrim, for example).
 */
#define SAT_SYSCALL_KERNEL	255

#define SAT_SYSCALL_ACCESS	 33
#define SAT_SYSCALL_CHDIR	 12
#define SAT_SYSCALL_CHMOD	 15
#define SAT_SYSCALL_CHOWN	 16
#define SAT_SYSCALL_CHROOT	 61
#define SAT_SYSCALL_CLOSE	  6
#define SAT_SYSCALL_CREAT	  8
#define SAT_SYSCALL_DUP		 41
#define SAT_SYSCALL_EXEC	 11
#define SAT_SYSCALL_EXECE	 59
#define SAT_SYSCALL_EXIT	  1
#define SAT_SYSCALL_FORK	  2
#define SAT_SYSCALL_FCHDIR	147
#define SAT_SYSCALL_FCHMOD	153
#define SAT_SYSCALL_FCHOWN	152
#define SAT_SYSCALL_KILL	 37
#define SAT_SYSCALL_LINK	  9
#define SAT_SYSCALL_MKDIR	 80
#define SAT_SYSCALL_MKNOD	 14
#define SAT_SYSCALL_MOUNT	 21
#define SAT_SYSCALL_OPEN	  5
#define SAT_SYSCALL_PIPE	 42
#define SAT_SYSCALL_PROCBLK	131
#define SAT_SYSCALL_RENAME	114
#define SAT_SYSCALL_RMDIR	 79
#define SAT_SYSCALL_SETGID	 46
#define SAT_SYSCALL_SETREGID	123
#define SAT_SYSCALL_SETREUID	124
#define SAT_SYSCALL_SETUID	 23
#define SAT_SYSCALL_SYSSGI	 40
#define SAT_SYSCALL_STAT	 18
#define SAT_SYSCALL_TRUNCATE	112
#define SAT_SYSCALL_UMOUNT	 22
#define SAT_SYSCALL_UNLINK	 10
#define SAT_SYSCALL_UTIME	 30

	/******************************/
	/*	SAT record types      */
	/******************************/
/*
 * If you add or remove a record type, update sat_init in sat.c as well
 * as the sat_eventtostr library function (event-to-string mapping).
 */

/* Path name record types */
#define SAT_FILE_HEADER		0	/* special type for SAT file headers */
#define SAT_ACCESS_DENIED	1	/* file access denied */
#define SAT_ACCESS_FAILED	2	/* file access failed (e.g. no file) */
#define SAT_CHDIR		3	/* change working directory */
#define SAT_CHROOT		4	/* change root directory */
#define SAT_OPEN		5	/* file open */
#define SAT_OPEN_RO		6	/* file open, read only */
#define SAT_READ_SYMLINK	7	/* read symbolic link */
#define SAT_FILE_CRT_DEL	8	/* file creation/deletion */
#define SAT_FILE_CRT_DEL2	9	/* as above with two pathnames */
#define SAT_FILE_WRITE		10	/* file data write */
#define SAT_MOUNT		11	/* mount/unmount */
#define SAT_FILE_ATTR_READ	12	/* file attribute read */
#define SAT_FILE_ATTR_WRITE	13	/* file attribute write */
#define SAT_EXEC		14	/* exec */
#define SAT_SYSACCT		15	/* system accounting */
/* File descriptor record types */
#define SAT_FCHDIR		20	/* change working directory via fd */
#define SAT_FD_READ		21	/* read file data or attrs via fd */
#define SAT_FD_READ2		22	/* as above with a set of fd's */
#define SAT_TTY_SETLABEL	23	/* tty reclassify (ioctl) */
#define SAT_FD_WRITE		24	/* write file data via fd */
#define SAT_FD_ATTR_WRITE	25	/* write file attributes via fd */
#define SAT_PIPE		26	/* create a pipe */
#define SAT_DUP			27	/* duplicate a descriptor */
#define SAT_CLOSE		28	/* close a descriptor */
/* Process record types */
#define SAT_FORK		40	/* create a new process */
#define SAT_EXIT		41	/* destroy a (this) process */
#define SAT_PROC_READ		42	/* read a process's addr space */
#define SAT_PROC_WRITE		43	/* write a process's addr space */
#define SAT_PROC_ATTR_READ	44	/* read a process's attributes */
#define SAT_PROC_ATTR_WRITE	45	/* change a process's attributes */
#define SAT_PROC_OWN_ATTR_WRITE	46	/* change this process's attributes */
/* System V IPC record types */
#define SAT_SVIPC_ACCESS	50	/* System V IPC access */
#define SAT_SVIPC_CREATE	51	/* System V IPC create */
#define SAT_SVIPC_REMOVE	52	/* System V IPC remove */
#define SAT_SVIPC_CHANGE	53	/* System V IPC change */
/* BSD IPC record types */
#define	SAT_BSDIPC_CREATE	60	/* socket, accept */
#define	SAT_BSDIPC_CREATE_PAIR	61	/* socketpair */
#define	SAT_BSDIPC_SHUTDOWN	62	/* shutdown */
#define	SAT_BSDIPC_MAC_CHANGE	63	/* setsockopt */
#define	SAT_BSDIPC_ADDRESS	64	/* bind, connect, accept syscalls */
#define SAT_BSDIPC_RESVPORT	65	/* bind to reserved port */
#define SAT_BSDIPC_DELIVER	66	/* rx pkt delivered to socket	*/
#define SAT_BSDIPC_CANTFIND	67	/* rx pkt no match on port/label */
#define SAT_BSDIPC_SNOOP_OK	68	/* raw socket delivery permitted */
#define SAT_BSDIPC_SNOOP_FAIL	69	/* raw socket delivery denied	*/

/* Public object record types */
#define SAT_CLOCK_SET		70	/* set the system clock */
#define SAT_HOSTNAME_SET	71	/* set the host name */
#define SAT_DOMAINNAME_SET	72	/* set the domain name */
#define SAT_HOSTID_SET		73	/* set the host id */
/* other record types */
#define SAT_CHECK_PRIV		80	/* make-or-break privilege checks */
#define SAT_CONTROL		81	/* audit controls */

/* more BSD IPC types */
#define SAT_BSDIPC_DAC_CHANGE	87	/* change socket uid or acl	*/
#define SAT_BSDIPC_DAC_DENIED	88	/* rx pkt not delivered	due to DAC  */
#define SAT_BSDIPC_IF_SETUID	89	/* ioctl SIOCSIFUID succeed/fail */
#define SAT_BSDIPC_RX_OK	90	/* rx pkt label in range	*/
#define SAT_BSDIPC_RX_RANGE	91	/* rx pkt label out of range	*/
#define SAT_BSDIPC_RX_MISSING	92	/* rx pkt label missing/malformed */
#define SAT_BSDIPC_TX_OK	93	/* tx pkt label in range	*/
#define SAT_BSDIPC_TX_RANGE	94	/* tx pkt label out of range	*/
#define SAT_BSDIPC_TX_TOOBIG	95	/* tx pkt label doesn't fit	*/
#define SAT_BSDIPC_IF_CONFIG	96	/* configure interface address	*/
#define	SAT_BSDIPC_IF_INVALID	97	/* ioctl SIOCSIFLABEL disallowed */
#define SAT_BSDIPC_IF_SETLABEL	98	/* ioctl SIOCSIFLABEL succeeded */

/* record types for user-level records generated with satwrite(2) */
#define SAT_USER_RECORDS	100	/* beginning of non-kernel auditing */
#define SAT_AE_AUDIT		100	/* audit subsys reporting on itself */
#define SAT_AE_IDENTITY		101	/* identification & authentication */
#define SAT_AE_DBEDIT		102	/* admin database editor */
#define SAT_AE_MOUNT		103	/* mount / unmount */
#define SAT_AE_CUSTOM		104	/* user-defined */
#define SAT_AE_LP		105	/* lp subsystem */

#define SAT_NTYPES		110	/* max record type + 1 (or greater) */

typedef	struct sat_ev_mask {
	unsigned long	ev_bits[howmany(SAT_NTYPES, NSATBITS)];
} sat_event_mask;

#ifdef __cplusplus
}
#endif
#endif	/* !__SYS_SAT_H */
