/**************************************************************************
 *									  *
 * 		 Copyright (C) 1989, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
/*
 * imon.h - inode monitor definitions
 *
 * $Revision: 1.7 $
 * $Date: 1993/08/25 23:14:00 $
 */

#ifndef _SYS_IMON_H
#define _SYS_IMON_H

#ifdef __cplusplus
extern "C" {
#endif


typedef ushort intmask_t;

/* Interest mask bits */
#define IMON_CONTENT	(1 << 0)	/* contents or size have changed */
#define IMON_ATTRIBUTE	(1 << 1)	/* mode or ownership have changed */
#define IMON_DELETE	(1 << 2)	/* last link has gone away */
#define IMON_EXEC	(1 << 3)	/* process executing */
#define IMON_EXIT	(1 << 4)	/* last process exited */
#define IMON_RENAME	(1 << 5)	/* inode has been renamed */
#ifdef _KERNEL
#define IMON_UNMOUNT	(1 << 13)	/* filesystem has been unmounted */
#define IMON_EXECUTING	(1 << 14)	/* internal file-in-execution flag */
#define IMON_COLLISION	(1 << 15)	/* internal hash collision flag */
#endif
#define IMON_CONTAINED	(1 << 15)	/* monitor all contained files in */
					/* a filesystem device */

#define IMON_OVER	0xff		/* queue has overflowed */
					/* XXX should be 0xffff */
#define IMON_EVENTMASK	0x3fff		/* interest bits for all events */
#define IMON_ALL	IMON_EVENTMASK	/* XXX archaic name */

/* User-specifiable mask bits */
#define	IMON_USERMASK	(IMON_CONTAINED|IMON_EVENTMASK)

/* Event queue element */
typedef struct {
	ino_t		qe_inode;	/* inode number of file */
	dev_t		qe_dev;		/* device of file */
	intmask_t	qe_what;	/* what events occurred */
} qelem_t;

#define IMON_ANYINODE	((ino_t) 0)	/* inumber wildcard */
#define	IMON_PATHNAME	((ino_t) 1)	/* qelem_t is really a qpath_t */

/*
 * If a qelem_t's qe_inode is IMON_PATHNAME, it's really a qpath_t and
 * qp_psize bytes of 0-terminated pathname follow it.
 */
typedef struct {
	ino_t		qp_magic;	/* must be IMON_PATHNAME */
	u_int		qp_psize;	/* gross pathname byte size */
} qpath_t;

/* Imon ioctls */
#define oIMONIOC_EXPRESS (('i' << 8) | 1)	/* old stat struct */
#define oIMONIOC_REVOKE	(('i' << 8) | 2)	/* old stat struct */
#define IMONIOC_QTEST	(('i' << 8) | 3)
#define IMONIOC_EXPRESS	(('i' << 8) | 4)
#define IMONIOC_REVOKE	(('i' << 8) | 5)

/* Interest structure for express/delete */
typedef struct {
	char		*in_fname;	/* pathname */
	struct stat	*in_sb;		/* optional status return buffer */
	intmask_t	in_what;	/* what types of events to send */
} interest_t;

#ifdef __cplusplus
}
#endif

#endif /* _SYS_IMON_H */
