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

/*
 * This file contains audit struct definitions suitable for
 * reading and interpreting old versions of an audit file.
 */

#ifndef	__SAT_COMPAT_HDR_
#define	__SAT_COMPAT_HDR_

#ident "$Revision: 1.1 $"

#include <sys/types.h>
#include <sys/mac_label.h>

	/********************************/
	/*	Version 1.0 headers	*/
	/********************************/

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

#endif	/* __SAT_COMPAT_HDR_ */
