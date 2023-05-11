#ifndef _XSHMTRANS_H_
#define _XSHMTRANS_H_
#define SHMBUFSIZE (64*2*1024)
#define SGI_SHM_FILENAME "/usr/tmp/shmtrans"

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
    int oldbin;				/* binary linked with an old sharedlib*/

    /* used to save fields of prda that implement atomic check-and-set */
    unsigned int t_scassrch;
    unsigned int t_ecassrch;
    unsigned int t_slckcas1;
    unsigned int t_elckcas1;
    unsigned int t_slckcas2;
    unsigned int t_elckcas2;
    unsigned int t_sunlckcas1;
    unsigned int t_eunlckcas1;
    unsigned int t_sunlckcas2;
    unsigned int t_eunlckcas2;
    unsigned int t_scas;
    unsigned int t_ecas;
    unsigned int t_casfail;
    unsigned int t_sclckcas1;
    unsigned int t_eclckcas1;
    unsigned int t_hscas;
    unsigned int t_hecas;
    unsigned int t_hcasfail;
    unsigned int t_hcasmayhave;
    unsigned int t_hcashavelock;

} shmdata_t;
#endif /* _XSHMTRANS_H_ */
