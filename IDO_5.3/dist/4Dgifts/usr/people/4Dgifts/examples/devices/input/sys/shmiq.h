#ifndef __SYS_SHMIQ_H__
#define __SYS_SHMIQ_H__

/**************************************************************************
 *									  *
 *		Copyright ( C ) 1990, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
/*
 * Internals for the shared memory input queue driver
 */
#ident "$Revision: 1.4 $"

#include "sys/poll.h"
/*
 * The data is ALMOST never interpreted.  The exception is for cursor
 * tracking.    If the type field is '0' (IDEV_PTR_EVENT), then
 * u.ptraxis[0] and u.ptraxis[1] contain pointer coordinates which
 * are sent to the "setscr" stuff designated stream.
 */
struct shmqdata {
	unsigned char device ;		/* device major */
	unsigned char which ;		/* device minor */
	unsigned char type ;		/* event type */
	unsigned char flags ;		/* little event data */
	union {
	    long int pos ;		/* big event data */
	    short ptraxis[2] ;		/* event data for PTR events */
	} un;
} ;

/* Data structure for intra-kernel verification ( addressability ) */
struct shmiqlinkid {
	short int devminor ;	/* Identifies the shmiq */
	short int index ;	/* Identifies the device */
} ;

struct shmqevent {
	union {
		long int time ;		/* 10 millisecond units from lbolt */
		struct shmiqlinkid id ;
	} un ;
	struct shmqdata data ;
} ;

struct sharedMemoryInputQueue {
	volatile long int head ;		/* user's index into events */
	volatile long int tail ;		/* kernel's index into events */
	volatile unsigned long int flags ;	/* place for out-of-band data */
	struct shmqevent events[1] ;		/* input event buffer */
} ;
/* flags' bits */
#define SHMIQ_OVERFLOW	0x00000001
#define SHMIQ_CORRUPTED 0x00000002

#ifdef _KERNEL

/* At "I_LINK" time, a M_PCPROTO message with this is sent to the lower
 * stream to see if he plays by the "shmiq" rules.  The correct response
 * is to "qreply" with a M_PCPROTO with "mtype" changed from "SHMIQ_NOTICE"
 * to "SHMIQ_PLAY".
 */
#define SHMIQ_NOTICE ( ( 'S' << 24 ) | ( 'H' << 16 ) | ( 'M' << 8 ) | 'Q' )
#define SHMIQ_PLAY   ( 'S' | ( 'H' << 8 ) | ( 'M' << 16 ) | ( 'Q' << 24 ) )
struct shmqntc {
	unsigned long int mtype ;
	struct shmiqlinkid id ;
} ;

struct cursorTrack { /* Used for kernel cursor tracking */
	int status ;
	long int x ;		/* cursor X */
	long int y ;		/* cursor Y */
	/* We can't filter at the source or the locator won't move */
	long int filterULX ;	/* don't care box minimum cursor X */
	long int filterULY ;	/* don't care box minimum cursor Y */
	long int filterLRX ;	/* don't care box maximum cursor X */
	long int filterLRY ;	/* don't care box maximum cursor Y */
} ;

struct shmq {
	struct sharedMemoryInputQueue *q ; /* kernel space pointer */
	long int qtail ;		/* private copy the user can't break */
	long int qsize ;		/* record at attach time */
	struct proc *owner ;		/* proccess attached to */
	caddr_t user_vaddr ;		/* location in owner's proccess space */
} ;

/* Chain for "I_LINK"'ed lower streams */
struct shmiqlink {
	struct shmiq *up ;
	struct shmiqlinkid id ;
	queue_t *lwq ;		/* top write queue of lower stream */
	queue_t *lrq ;		/* top read queue of lower stream */
	struct shmiq *mux ;
	int l_index ;		/* system ID from struct linkblk */
	int l_flags ;		/* flags specific to this device */
} ;

struct shmiq {
	/* Event Queue stuff */
	struct shmq qd ;		/* Connection data for user's queue */
	struct cursorTrack locator ;	/* Used for kernel cursor tracking */
	unsigned int currentScreen ;	/* Where to send cursor motion */

	/* Streams stuff */
	queue_t *urq ;			/* top read queue of upper stream */
#define MAX_MUX_LINKS 16 /* Should be a dynamic parameter, but ... */
	struct shmiqlink *linkrecord[MAX_MUX_LINKS] ;

	/* spin lock for access */
	lock_t alck ;			/* always acquired by "trading" */
					/* for the "shmiq_lock" */

	/* Misc. Std. Driver stuff */
	int status ;
/* possible status values -- OR'ed together */
#define SHMIQ_STR_INIT 1
#define SHMIQ_CHAR_INIT	2

	int stroflag ;
	int deviceminor ;		/* To find our way back */
	struct pollhead selproc ;		/* For selwakeup */
} ;

/* Stuff from master.d/shmiq */
extern int shmiq_cnt ; /* Actual number */
extern lock_t shmiq_lock ; /* Controls all access to the array */
extern struct shmiq *dev_shmiq[] ;

#define SHMIQ_POSCURSOR ( ( 'S' << 24 ) | ( 'H' << 16 ) | ( 'I' << 8 ) | 'Q' )

#endif /* _KERNEL */

/* USER VISIBLE */
#define SHMIQ_MINSIZE 100 /* Minimum size of queue in number of events */

struct muxioctl {	/* for QIOCMUXIOC */
	int index ;	/* index of lower stream */
	int realcmd ;	/* subdevice IOCTL command */
} ;

struct shmiqreq {		/* for QIOCATTACH */
	char *user_vaddr ;	/* starting address to lock down for a shmiq */
	int arg ;		/* size of queue event array */
} ;

#define	SHMIQ_LINK_CURSX	0x1
#define	SHMIQ_LINK_CURSY	0x2
#define	SHMIQ_LINK_CURSOR	0x3
struct shmiqsetcurs {	/* for QIOCSETCURS */
	short index ;	/* index of device */
	short axes ;	/* axes we care about (SHMIQ_LINK_CURS[XY]) */
} ;

struct shmiqsetcpos {	/* for QIOCSETCPOS */
	short	x ;
	short	y ;
} ;

/* ioctl cmds */
#define QIOCATTACH	_IOW('Q',1,struct shmiqreq)	/* map memory */
#define QIOCDETACH	_IO('Q',2)			/* unmap memory */
#define QIOCSERVICED	_IO('Q',3)		/* acknowledge overflow */
#define QIOCURSTRK	_IOW('Q',4,int)		/* set cursor tracking mode */
#define QIOCURSIGN	_IOW('Q',5,long int[4])	/* set cursor filter box */
#define QIOCSETSCRN	_IOW('Q',6,int)		/* set curent screen */
#define QIOCIISTR	_IOW('Q',7, struct muxioctl)/* double indirect I_STR */
#define QIOCGETINDX	_IOWR('Q',8,int)	/* get index from l_index */
#define	QIOCSETCURS	_IOWR('Q',9,struct shmiqsetcurs ) /* set cursor axes */
#define	QIOCSETCPOS	_IOWR('Q',10,struct shmiqsetcpos ) /* set cursor position */

/* QIOCURSTRK mode values -- or'ed together */
#define SHMIQ_CURSOR_TRACKED 1
#define SHMIQ_CURSOR_ASYNC 2	/* Tracks physical device */
#define SHMIQ_CURSOR_SYNC  0	/* Tracks logical device */
#define SHMIQ_CURSOR_FILTER 4	/* use ignore box ? (arm trigger) */

#endif /* __SYS_SHMIQ_H__ */
