/**************************************************************************
 *									  *
 * 		 Copyright (C) 1988,1992 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

#ifndef	__SAT_COMPAT_HDR_
#define	__SAT_COMPAT_HDR_

#ident "$Revision: 1.2 $"

#include <sys/types.h>
#include <sys/mac_label.h>

/*
 *  The following new types allow us to account for changes in fundamental
 *  data types between 4.0.5 and 5.1.  Sat uses the external representation
 *  of pid under 4.0.5, so the pid field is not affected by the change.
 */

#ifndef _SYSTYPE_SVR4
typedef ushort	o_mode_t
typedef short	o_dev_t;
typedef ushort	o_uid_t;
typedef ushort	o_gid_t;
typedef short	o_nlink_t;

typedef unsigned long	n_mode_t;
typedef unsigend long	n_dev_t;
typedef long		n_uid_t;
typedef long		n_gid_t;
typedef unsigned long	n_nlink_t;
#else 
typedef mode_t	n_mode_t;
typedef dev_t	n_dev_t;
typedef uid_t	n_uid_t;
typedef gid_t	n_gid_t;
typedef nlink_t	n_nlink_t;
#endif

	/********************************/
	/*	Version 1.0 headers	*/
	/********************************/

	/* Common Record Header (pre-fix) */

struct	sat_hdr_1_0 {
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
	o_uid_t	sat_id;		/* SAT user-id */
	o_dev_t	sat_tty;	/* controlling tty, if present */
	pid_t	sat_ppid;	/* parent process id */
	pid_t	sat_pid;	/* process id of record's generator */
	o_uid_t	sat_euid;	/* Effective user id */
	o_uid_t	sat_ruid;	/* Real user id */
	o_gid_t	sat_egid;	/* Effective group id */
	o_gid_t	sat_rgid;	/* Real group id */
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

struct	sat_pathname_1_0 {
	ino_t sat_inode;
	o_dev_t sat_device;
	o_uid_t sat_fileown;
	o_gid_t sat_filegrp;
	o_mode_t sat_filemode;
	u_short sat_reqname_len;
	u_short sat_actname_len;
	/* char data[];	*/
	/* file label */
};

	/* Sat File Header */

struct sat_filehdr_1_0 {
	char	sat_magic[8];		/* == "SGIAUDIT" */
	u_char	sat_major;		/* version of audit data */
	u_char	sat_minor;
	time_t	sat_start_time;		/* time header written */
	time_t	sat_stop_time;		/* time file closed (added later) */
	long	sat_host_id;		/* host id */
	int	sat_total_bytes;	/* number of bytes to skip past hdr */
	u_short	sat_user_entries;	/* number of sat_user_ent structs */
	u_short	sat_group_entries;	/* number of sat_group_ent structs */
	u_char	sat_timezone_len;	/* bytes of timezone string */
	u_char	sat_hostname_len;	/* bytes of hostname */
	u_char	sat_domainname_len;	/* bytes of domainname */
	/* TZ (timezone) (including trailing null) */
	/* hostname (including trailing null) */
	/* domainname (including trailing null) */
	/* user entries, each word aligned */
	/* group entries, each word aligned */
};

struct sat_user_ent_1_0 {
	o_uid_t	sat_uid;
	u_short	sat_namelen;
	/* user name (including trailing null) */
};

struct sat_group_ent_1_0 {
	o_gid_t	sat_gid;
	u_short	sat_namelen;
	/* group name (including trailing null) */
};

struct	sat_mount_1_0 {
	o_dev_t sat_fs_dev;
	short sat_npaths;
	/* sat_pathname device */
	/* sat_pathname mount_point */
};

struct	sat_file_attr_write_1_0 {
	union {
		o_mode_t sat_filemode;		/* for chmod */
		struct {
			o_uid_t sat_fileown;
			o_gid_t sat_filegrp;
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

struct	sat_exec_1_0 {
	o_uid_t sat_euid;	/* effective could be set by setuid progs */
	o_gid_t sat_egid;	/* effective could be set by setgid progs */
	u_char sat_npaths;	/* number of pathname modules following */
	u_char sat_interpreter;	/* is there a shell interpreter? */
	/* sat_pathname */
	/* ... */
};

struct	sat_fd_read2_1_0 {
	struct sat_fdset {
		long fds_bits[8];
	} sat_fdset;

};

struct	sat_fd_attr_write_1_0 {
	int sat_filedes;
	union {
		o_mode_t sat_filemode;		/* for fchmod */
		struct {
			o_uid_t sat_fileown;
			o_gid_t sat_filegrp;
		} fchown;			/* for fchown */
	} newattr;
};

struct	sat_proc_access_1_0 {
	pid_t sat_pid;
	o_uid_t sat_ruid;
	o_uid_t sat_euid;
	int sat_signal;		/* kill only */
	/* process label */
};

struct	sat_proc_own_attr_write_1_0 {
	union {
		struct {
			o_uid_t sat_euid;
			o_uid_t sat_ruid;
		} uid;				/* for setuid, setreuid */
		struct {
			o_gid_t sat_egid;
			o_gid_t sat_rgid;
		} gid;				/* for setgid, setregid */
		int sat_label_size;		/* for setplabel */
		int sat_glist_len;		/* for setgroups */
	} newattr;
	/* process label, if setplabel */
	/* group list, if setgroups */
};

struct	sat_svipc_change_1_0 {
	int sat_svipc_id;
	o_uid_t sat_svipc_oldown;
	o_gid_t sat_svipc_oldgrp;
	u_short sat_svipc_oldperm;
	o_uid_t sat_svipc_newown;
	o_gid_t sat_svipc_newgrp;
	u_short sat_svipc_newperm;
};

	/********************************/
	/*	Version 1.1 headers	*/
	/********************************/

struct	sat_hdr_1_1 {
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
        long    sat_host_id;    /* host id (new for format 1.1) */
	o_uid_t	sat_id;		/* SAT user-id */
	o_dev_t	sat_tty;	/* controlling tty, if present */
	pid_t	sat_ppid;	/* parent process id */
	pid_t	sat_pid;	/* process id of record's generator */
	o_uid_t	sat_euid;	/* Effective user id */
	o_uid_t	sat_ruid;	/* Real user id */
	o_gid_t	sat_egid;	/* Effective group id */
	o_gid_t	sat_rgid;	/* Real group id */
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

struct sat_filehdr_1_1 {
	char	sat_magic[8];		/* == "SGIAUDIT" */
	u_char	sat_major;		/* version of audit data */
	u_char	sat_minor;
        u_char  sat_pad1[2];            /* alignment filler */
	time_t	sat_start_time;		/* time header written */
	time_t	sat_stop_time;		/* time file closed (added later) */
	long	sat_host_id;		/* host id */
        int     sat_mac_enabled: 1;     /* boolean: ignore mac fields or not */
	int	sat_total_bytes: 31;	/* number of bytes to skip past hdr */
	u_short	sat_user_entries;	/* number of sat_user_ent structs */
	u_short	sat_group_entries;	/* number of sat_group_ent structs */
        u_short sat_host_entries;       /* number of hostid <-> name list */
	u_char	sat_timezone_len;	/* bytes of timezone string */
	u_char	sat_hostname_len;	/* bytes of hostname */
	u_char	sat_domainname_len;	/* bytes of domainname */
        u_char  sat_pad2[3];            /* alignment filler */
	/* TZ (timezone) (including trailing null) */
	/* hostname (including trailing null) */
	/* domainname (including trailing null) */
	/* user entries, each word aligned */
	/* group entries, each word aligned */
};

struct	sat_bsdipc_create_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_comm_domain;
	short	sat_protocol;
};

struct	sat_bsdipc_create_pair_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_comm_domain;
	short	sat_protocol;
	short	sat_second_dscr;
	void *	sat_second_socket;	/* second socket identifier	*/
};

struct	sat_bsdipc_shutdown_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_how;
};

struct	sat_bsdipc_mac_change_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	u_short sat_label_size;
/*	mac_label   sat_label;		-  New label on socket 		*/
};

struct	sat_bsdipc_address_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_addr_len;
/*	char *	sat_socket_addr;	- allocated separately		*/
};

struct	sat_bsdipc_resvport_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	short	sat_socket_dscr;
	short	sat_port;
};

struct	sat_bsdipc_match_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
	u_short sat_ip_len;
	u_short sat_label_len;
/*	struct ip sat_ip;		-  ip header & port numbers	*/
/*	mac_label   sat_label;		-  Label on datagram 		*/
};

struct	sat_bsdipc_snoop_1_1 {
	void *	sat_socket;		/* socket identifier 		*/
					/* <<< NEED MORE HERE ??? >>>	*/
	u_short sat_label_len;
/*	mac_label   sat_label;		-  Label on datagram 		*/
};

struct	sat_bsdipc_range_1_1 {  
	char    sat_ifname[SATIFNAMSIZ];/* name of interface		*/
	u_short sat_ip_len;
	u_short sat_label_len;
/*	struct ip sat_ip;		-  ip datagram header (only!)	*/
/*	mac_label   sat_label;		-  Label on datagram 		*/
};

	/********************************/
	/*	Version 1.2 headers	*/
	/********************************/

struct	sat_hdr_1_2 {
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
        long    sat_host_id;    /* host id (new for format 1.1) */
	o_uid_t	sat_id;		/* SAT user-id */
	o_dev_t	sat_tty;	/* controlling tty, if present */
	pid_t	sat_ppid;	/* parent process id */
	pid_t	sat_pid;	/* process id of record's generator */
	o_uid_t	sat_euid;	/* Effective user id */
	o_uid_t	sat_ruid;	/* Real user id */
	o_gid_t	sat_egid;	/* Effective group id */
	o_gid_t	sat_rgid;	/* Real group id */
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

struct sat_filehdr_1_2 {
	char	sat_magic[8];		/* == "SGIAUDIT" */
	u_char	sat_major;		/* version of audit data */
	u_char	sat_minor;
        u_char  sat_pad1[2];		/* alignment filler */
	time_t	sat_start_time;		/* time header written */
	time_t	sat_stop_time;		/* time file closed (added later) */
	long	sat_host_id;		/* host id */
        int	sat_mac_enabled: 1;     /* boolean: ignore mac fields or not */
	int	sat_total_bytes: 31;	/* number of bytes to skip past hdr */
	u_short	sat_user_entries;	/* number of sat_user_ent structs */
	u_short	sat_group_entries;	/* number of sat_group_ent structs */
	u_char	sat_timezone_len;	/* bytes of timezone string */
	u_char	sat_hostname_len;	/* bytes of hostname */
	u_char	sat_domainname_len;	/* bytes of domainname */
        u_char  sat_pad2[3];		/* alignment filler */
	/* TZ (timezone) (including trailing null) */
	/* hostname (including trailing null) */
	/* domainname (including trailing null) */
	/* user entries, each word aligned */
	/* group entries, each word aligned */
};
	/********************************/
	/*	Version 2.1 headers	*/
	/********************************/

struct	sat_hdr_2_0 {
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
        long    sat_host_id;    /* System host id */
	n_uid_t	sat_id;		/* SAT user-id */
	n_dev_t	sat_tty;	/* controlling tty, if present */
	pid_t	sat_ppid;	/* parent process id */
	pid_t	sat_pid;	/* process id of record's generator */
	n_uid_t	sat_euid;	/* Effective user id */
	n_uid_t	sat_ruid;	/* Real user id */
	n_gid_t	sat_egid;	/* Effective group id */
	n_gid_t	sat_rgid;	/* Real group id */
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

struct sat_filehdr_2_0 {
	char	sat_magic[8];		/* == "SGIAUDIT" */
	u_char	sat_major;		/* version of audit data */
	u_char	sat_minor;
        u_char  sat_pad1[2];            /* alignment filler */
	time_t	sat_start_time;		/* time header written */
	time_t	sat_stop_time;		/* time file closed (added later) */
	long	sat_host_id;		/* host id */
	int	sat_mac_enabled: 1;	/* boolean: ignore mac fields or not */
	int	sat_total_bytes: 31;	/* number of bytes to skip past hdr */
	u_short	sat_user_entries;	/* number of sat_list_ent structs */
	u_short	sat_group_entries;	/*   in the user and group lists  */
        u_short sat_host_entries;	/*   and the hostid <-> name list */
	u_char	sat_timezone_len;	/* bytes of timezone string */
	u_char	sat_hostname_len;	/* bytes of hostname */
	u_char	sat_domainname_len;	/* bytes of domainname */
        u_char  sat_pad2[3];            /* alignment filler */
	/* TZ (timezone) (including trailing null) */
	/* hostname (including trailing null) */
	/* domainname (including trailing null) */
	/* user entries, each word aligned */
	/* group entries, each word aligned */
};

#endif	/* __SAT_COMPAT_HDR_ */
