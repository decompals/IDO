/*************************************************************************
*                                                                        *
*               Copyright (C) 1992, Silicon Graphics, Inc.       	 *
*                                                                        *
*  These coded instructions, statements, and computer programs  contain  *
*  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
*  are protected by Federal copyright law.  They  may  not be disclosed  *
*  to  third  parties  or copied or duplicated in any form, in whole or  *
*  in part, without the prior written consent of Silicon Graphics, Inc.  *
*                                                                        *
**************************************************************************/
#ident  "$Revision: 1.3 $ $Author: roberts $"

#ifndef __AIO_H__
#define __AIO_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/timers.h>
#include <sys/fcntl.h>


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

#endif /* __AIO_H__ */
