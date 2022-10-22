#ifndef _XSHMTRANS_H_
#define _XSHMTRANS_H_

#include <ulocks.h>

#define SGI_SHM_FILENAME "/usr/tmp/.Xshmtrans"

typedef struct _connhdr {
    ulock_t clientlock;			/* spinlock to single thread startup */
    usema_t *syncsema;			/* semaphore for synchronization */
    usema_t *syncsema2;			/* semaphore for synchronization */
    unsigned char *arenahdr;		/* used for passing per/client arena */
} connhdr_t;

typedef struct _arenahdr {
    unsigned char *shmbuf;		/* start of buffer */
    unsigned char *shmbufend;		/* end of buffer */

    volatile int fullflag;		/* buffer is full flag */
    volatile int emptyflag;		/* buffer is empty flag */

    usema_t *fullsema;			/* semaphore for full flag */
    usema_t *emptysema;			/* semaphore for empty flag */

    ulock_t headlock;			/* spinlock to protect head ptr */
    ulock_t taillock;			/* spinlock to protect tail ptr */

    volatile unsigned char *headptr;	/* head of valid data */
    volatile unsigned char *tailptr;	/* tail of valid data */
} arenahdr_t;

typedef struct _shmdata {
    usptr_t *arena;			/* shared arena */
    arenahdr_t *arenahdr;		/* shared data structures */
    connhdr_t *connhdr;			/* server connection header */
    unsigned char *curreqptr;		/* pointer to start of current req. */
    unsigned char *reqendptr;		/* pointer to end of current request */
    unsigned char *halfptr;		/* pointer to end of current request */
    int pid;
} shmdata_t;


/* do n bytes fit in buffer?? */
#define SHM_BYTES_FIT( head, tail, endofbuf, n) \
	( ( (tail) <= (head) ) ? \
		( ( (head) + (n) ) < (endofbuf) ) : \
		( ( (head) + (n) ) < (tail) ) )

#define _XShmAlloc( ret, type, my_shmptr, my_dpy, nbytes ) \
{ \
	arenahdr_t *my_arenahdr = (my_shmptr)->arenahdr; \
	unsigned char *my_tailptr = (unsigned char *)my_arenahdr->tailptr; \
	if ( (my_dpy)->bufptr != (my_dpy)->buffer ) \
		_XShmCompatBufCopy(my_dpy); \
	if ( SHM_BYTES_FIT( (my_shmptr)->reqendptr, my_tailptr, \
				my_arenahdr->shmbufend, (nbytes) ) ) \
	{ \
/* printf("_XShmAlloc:n = %d, ret = 0x%x, tail = 0x%x\n", nbytes, (my_shmptr)->reqendptr, my_tailptr); */ \
		(ret) = (type)((my_shmptr)->reqendptr); \
		(my_shmptr)->reqendptr += nbytes; \
	} else { \
		(ret) = (type) _XShmRealAlloc( (my_dpy), (nbytes) ); \
	} \
}

#ifndef TRANS_SERVER
extern void _XShmFastBcopy(volatile int *, volatile int *, int);
extern unsigned char *_XShmRealAlloc(struct _XDisplay *, unsigned long);
extern void _XShmFlush(struct _XDisplay *, int);
extern void _XShmWaitForNotFull(struct _XDisplay *, unsigned char *);
extern void _XShmCompatBufCopy(struct _XDisplay *);
extern void _XShmEndBuf(struct _XDisplay *, unsigned char *);
extern void _XShmSendData(struct _XDisplay *, unsigned char *, unsigned long);
#endif

#if defined(TRANS_SERVER) && defined(XSERV_t)

#define TRANS_ACCEPT_POLLSEMA_FAILED	-4
#define TRANS_ACCEPT_NEWSEMA_FAILED	-5
#define TRANS_ACCEPT_NEWLOCK_FAILED	-6
#define TRANS_ACCEPT_VSEMA_FAILED	-7
#define TRANS_ACCEPT_PSEMA_FAILED	-8

typedef struct _connectionShm {
    /*
     * unixfd: file descriptor to a Unix domain socket used for writing
     * back replies and errors to shm clients (also used to determine
     * client death, see unixfd_closed).
     */
    int unixfd;
    /*
     * unixfd_closed: if TRUE, means select triggered on the unixfd, but
     * since nothing should ever be written to this fd, it probably
     * means the client has closed the unixfd.  In which case break,
     * of the connection.
     */
    Bool unixfd_closed;
    /*
     * Handle to pointers into the shared memory data arena data
     * structure (the header).  See lib/X/XShmTrans.h
     */
    struct _arenahdr *arenahdr;
    /*
     * Pointer to next request.
     */
    unsigned char *nextreq;
    /*
     * Pointer to current request.
     */
    unsigned char *curreq;
} ConnectionShm, *ConnectionShmPtr;

typedef struct _Shmpriv {
    XtransConnInfo  unixciptr; /* unix ciptr */
    connhdr_t *connhdr;
    usptr_t *arena;
    ConnectionShm	ocpriv; /* */
} ShmPriv;
#endif

#endif /* _XSHMTRANS_H_ */
