/*************************************************************************
*                                                                        *
*               Copyright (C) 1992,1994 Silicon Graphics, Inc.       	 *
*                                                                        *
*  These coded instructions, statements, and computer programs  contain  *
*  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
*  are protected by Federal copyright law.  They  may  not be disclosed  *
*  to  third  parties  or copied or duplicated in any form, in whole or  *
*  in part, without the prior written consent of Silicon Graphics, Inc.  *
*                                                                        *
**************************************************************************/
#ident  "$Revision: 1.5 $ $Author: jeffreyh $"

#ifndef __AIO_H__
#define __AIO_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/timers.h>
#include <sys/fcntl.h>

#ifdef  _ABI_SOURCE
/*
 * aio - POSIX 1003.1b-1993
 */

typedef struct aiocb {
    int	aio_fildes;		/* file descriptor to perform aio on */
    volatile void *aio_buf; 	/* Data buffer */
    size_t	aio_nbytes;	/* number of bytes of data */
    off_t	aio_offset;	/* file offset position */
    int		aio_reqprio;	/* aio priority, (Currently must be 0) */
    sigevent_t	aio_sigevent;	/* notification information */
    int		aio_lio_opcode;	/* opcode for lio_listio() call */
    ulong	aio_reserved[7];/* reserved for internal use */
    ulong	aio_pad[6];
} aiocb_t;

/* for aio_cancel() return values */
#define AIO_CANCELED	1	/* cancelled operation */
#define AIO_NOTCANCELED 2	/* some ops not cancelled */
#define AIO_ALLDONE	3	/* all aio has completed */

/* for aiocb.aio_lio_opcode */
#define LIO_NOP		0	/* listio request with no data */
#define LIO_READ	1	/* listio read request */
#define LIO_WRITE	2	/* listio write request */

/* for lio_listio mode flag */
#define LIO_WAIT	4	/* blocks until lio_listio complete */
#define LIO_NOWAIT	3	/* asynchronous lio_listio call, doesn't block */

/* for lio_hold routine */
#define AIO_HOLD_CALLBACK	1
#define AIO_RELEASE_CALLBACK	2
#define AIO_ISHELD_CALLBACK	3

#if defined(_SGI_SOURCE)
/* These defines are not for use by applications. */
#define _AIO_SGI_LISTIO_MAX	2048
#define _AIO_SGI_MAX		2048
#define _AIO_SGI_PRIO_DELTA_MAX	0
#endif
#if !defined(_POSIX_SOURCE)
/*
 * This stucture is the optional argument to aio_sgi_init. The defaults
 * that are used if NULL is given as the argument are in parentheses at the
 * end of each comment.
 */
typedef struct aioinit {
    int aio_threads;	/* The number of threads started (5) */
    int aio_locks;	/* Initial number of preallocated locks (3) */
    int aio_num;	/* estimated total simultanious aiobc structs (1000) */
    int aio_usedba;	/* Try to use DBA for raw I/O in lio_listio (0) */
    int aio_debug;	/* turn on debugging (0) */
    int aio_pad[4];		
} aioinit_t;
#endif /* !_POSIX_SOURCE */
#ifndef aio_init
#define aio_init() aio_sgi_init(NULL);
#endif /* aio_init */
extern void aio_sgi_init(aioinit_t *);
extern int aio_read(aiocb_t *);
extern int aio_write(aiocb_t *);
extern int lio_listio(int, aiocb_t * const [], int, sigevent_t *);
extern int aio_cancel(int, aiocb_t *);
extern int aio_error(const aiocb_t *);
extern ssize_t aio_return(aiocb_t *);
extern int aio_suspend(const aiocb_t * const [], int, const timespec_t *);
extern int aio_fsync(int, aiocb_t *);
extern int aio_hold(int);
#else /* _ABI_SOURCE */
/*
 * aio - POSIX 1003.4 Draft 12
 */

typedef struct aiocb {
	/* defined by Posix.4 D12 */
	int	aio_fildes;	/* file descriptor to perform aio on */
	off_t	aio_offset;	/* file offset position */
	volatile void *aio_buf;
	size_t	aio_nbytes;
	int	aio_reqprio;	/* aio priority, larger values lowers pri */
	sigevent_t aio_sigevent;/* signal to be generated on completion */
	int	aio_lio_opcode;	/* opcode for lio_listio() call */
	/* SGI defined */
	size_t	aio_nobytes;	/* return bytes */
	int	aio_whence;	/* for seeking */
	int	aio_errno;	/* return error from this aio op */
	int	aio_ret;	/* returned status */
} aiocb_t;

/* for aio_cancel() return values */
#define AIO_CANCELED	1	/* cancelled operation */
#define AIO_NOTCANCELED 2	/* some ops not cancelled */
#define AIO_ALLDONE	3	/* all aio has completed */

/* for aiocb.aio_lio_opcode */
#define LIO_READ	1
#define LIO_WRITE	2
#define LIO_NOP		3

/* for lio_listio mode flag */
#define LIO_WAIT	1
#define LIO_NOWAIT	2

#if defined(_SGI_SOURCE)
#define _AIO_LISTIO_MAX		255
#define _AIO_MAX		4
#define _AIO_PRIO_DELTA_MAX	100
#endif

extern void aio_init(void);
extern int aio_read(struct aiocb *);
extern int aio_write(struct aiocb *);
extern int lio_listio(int, struct aiocb *[], int, sigevent_t *);
extern int aio_cancel(int, struct aiocb *);
extern int aio_error(struct aiocb *);
extern ssize_t aio_return(struct aiocb *);
extern int aio_suspend(const struct aiocb *[], int, timespec_t *);
extern int aio_fsync(int, struct aiocb *);

#ifdef __cplusplus
}
#endif
#endif /*  _ABI_SOURCE */
#endif /* __AIO_H__ */
