#include <stdio.h>
#include <bstring.h>

#define _KERNEL
#include <sys/types.h>
#undef _KERNEL
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/termio.h>
#include <sys/cmn_err.h>
#include <sys/fcntl.h>

/* Streams stuff */
#include <sys/stream.h>
#include <sys/strids.h>
#include <sys/stropts.h>

#include <sys/shmiq.h>
#define _KERNEL
#include <sys/idev.h>

#include "inputtest.h"

static queue_t	queues[2];
static queue_t	*read_q= &queues[0];
static queue_t	*write_q= &queues[1];

int lbolt;
int doass= 1;
unsigned char kbdtype = 0;
static	int	dev_fd;
static	int	pointerMode = 0;

	/*
	 * This file helps to debug device streams modules from the user
	 * level.  
	 */

/***====================================================================***/

void
cmn_err(int level,char *str,void *a1,void *a2,void *a3,void *a4,void *a5)
{
    switch (level) {
	case CE_CONT: 	fprintf(stderr,"         ");
			break;
	case CE_WARN: 	fprintf(stderr,"Warning! ");
			break;
	case CE_DEBUG:
	default:	break;
    }
    fprintf(stderr,str,a1,a2,a3,a4,a5);
    return;
}

void
assfail(char *expr,char *file,int line)
{
    fprintf(stderr,"%s, Line %d: Assertion failed (%s)|n",file,line,expr);
    exit(1);
}

mblk_t	*
allocb(int size, uint pri)
{
mblk_t	*blk;

    blk= (mblk_t *)malloc(sizeof(mblk_t));
    if (blk!=NULL) {
	blk->b_next= NULL;
	blk->b_prev= NULL;
	blk->b_cont= NULL;
	blk->b_rptr= blk->b_wptr= (unsigned char *)malloc(size);
	if (blk->b_rptr) {
	    blk->b_datap= (struct datab *)malloc(sizeof(struct datab));
	    if (blk->b_datap) {
		blk->b_datap->db_type= M_DATA;
		return blk;
	    }
	    free(blk->b_rptr);
	}
	free(blk);
    }
    return NULL;
}

/* ARGSUSED */
void
putnext(queue_t *q,mblk_t *blk)
{
struct shmqevent *ev= (struct shmqevent *)blk->b_rptr;

    if ((q==read_q)&&(blk->b_datap->db_type==M_DATA)) {
	while ( ((unsigned char *)ev)< blk->b_wptr ) {
		switch (ev->data.type) {
		    case QE_PTR_EVENT:
			fprintf(stderr,"Pointer to (%d,%d) ",
						   ev->data.un.ptraxis[0],
						   ev->data.un.ptraxis[1]);
			if (ev->data.flags) {
			    int	some= 0;
			    fprintf(stderr,"[");
			    if (ev->data.flags&QE_RESPONSE)
				fprintf(stderr,"%sresponse",some++?" ":"");
			    if (ev->data.flags&IDEV_GEN_PTR_X)
				fprintf(stderr,"%sx",some++?" ":"");
			    if (ev->data.flags&IDEV_GEN_PTR_Y)
				fprintf(stderr,"%sy",some++?" ":"");
			    if (ev->data.flags&QE_X_CLAMPED)
				fprintf(stderr,"%sxclamped",some++?" ":"");
			    if (ev->data.flags&QE_Y_CLAMPED)
				fprintf(stderr,"%syclamped",some++?" ":"");
			    fprintf(stderr,"]\n");
			}
			else fprintf(stderr,"\n");
			break;
		    case QE_VAL_EVENT:
			fprintf(stderr,"Valuator %d to %d",ev->data.which,
							   ev->data.un.pos);
			if (ev->data.flags) {
			    int	some= 0;
			    fprintf(stderr,"[");
			    if (ev->data.flags&QE_MORE_EVENTS)
				fprintf(stderr,"%smore",some++?" ":"");
			    if (ev->data.flags&QE_RESPONSE)
				fprintf(stderr,"%sresponse",some++?" ":"");
			    if (ev->data.flags&QE_CLAMPED)
				fprintf(stderr,"%sclamped",some++?" ":"");
			    fprintf(stderr,"]\n");
			}
			else fprintf(stderr,"\n");
			break;
		    case QE_BTN_EVENT:
			fprintf(stderr,"Button %d %s\n",ev->data.which,
				(ev->data.flags&QE_BTN_DOWN)?"down":"up");
			break;
		    default:
			fprintf(stderr,"Unknown event type %d\n",ev->data.type);
			break;
		}
		ev++;
	}
    }
    else if (q==write_q) {
	if (blk->b_datap->db_type==M_DATA) {
	    fprintf(stderr,"writing %d bytes to device\n",blk->b_wptr-blk->b_rptr);
	    write(dev_fd,blk->b_rptr,blk->b_wptr-blk->b_rptr);
	}
	else if (blk->b_datap->db_type==M_IOCTL) {
	    struct iocblk *iocb= (struct iocblk *)blk->b_rptr;
	    struct strioctl sio;

	    fprintf(stderr,"ioctl 0x%x",iocb->ioc_count);
	    sio.ic_cmd = iocb->ioc_cmd;
	    sio.ic_timout = 0;
	    if (blk->b_cont) {
		sio.ic_len=	iocb->ioc_count;
		sio.ic_dp=	blk->b_cont->b_rptr;
		fprintf(stderr,"(%d bytes)\n",iocb->ioc_count);
	    }
	    else {
		sio.ic_len=	0;
		sio.ic_dp=	NULL;
		fprintf(stderr,"(0 (%d) bytes)\n",iocb->ioc_count);
	    }
	    ioctl(dev_fd,I_STR,&sio);
	}
    }
    else {
	fprintf(stderr,"unknown queue in putnext\n");
    }
    freemsg(blk);
}

void
freemsg(mblk_t *blk)
{
mblk_t *next;
   if (blk!=NULL) {
	do {
	    next = blk->b_cont;
	    if (blk->b_rptr!=NULL)
		free(blk->b_rptr);
	    if (blk->b_datap!=NULL)
	    free(blk->b_datap);
	    free(blk);
	    blk= next;
	} while (blk);
   }
   return;
}


/***====================================================================***/

void
flushq(register queue_t *q, int flag)
{
}

int
canput(queue_t *q)
{
    return 1;
}

void
qreply(queue_t *q,mblk_t *blk)
{
}

int
itimeout(int (*func)(),void *priv,int ticks,pl_t id)
{
    return 1;
}

void
untimeout( int id )
{
}

toid_t
bufcall(uint size, int pri, void (*func)(), long arg)
{
    return 0;
}

splstr()
{
    return 0;
}

void *
kmem_alloc(size_t nbytes,int flag)
{
    return malloc(nbytes);
}

void
kmem_free(void *vptr, size_t size)
{
    free(vptr);
}

#undef WR
queue_t *
WR(queue_t *q)
{
    return(q+1);
}


	/*
	 *  Add stubs for any functions modules used by your streams module
	 *  here.
	 */

/***====================================================================***/

	/*
	 * To add a device:
	 *     Add an extern for the streamtab which describes the device
	 *         and add an entry to devices[].  The name of the device
	 *	   is the name of the streams module *not* the name returned
	 *	   by your IDEVGETDEVICEDESC ioct.   For example, the spaceball
	 *	   device is named "spaceball," but the object file for it
	 *         is "sball.o" and the streams module name is "sball" --
	 *	   the spaceball entry in the table lists "sball."
	 *     To simplify debugging, you might want to change the 
	 *	   DEFAULT_DEVICE to be the name of your device.
	 */

#define	DEFAULT_DEVICE	"sball"

extern	struct streamtab keyboardinfo;
extern	struct streamtab mouseinfo;
extern	struct streamtab dialinfo;
extern	struct streamtab tabletinfo;
extern	struct streamtab sballinfo;
extern	struct streamtab hitachiinfo;
extern	struct streamtab impinfo;

static struct _devinfo {
	char			*name;
	struct streamtab	*st;
} devices[] = {
	{	"keyboard",	&keyboardinfo	},
	{	"mouse",	&mouseinfo	},
	{	"dial",		&dialinfo	},
	{	"tablet",	&tabletinfo	},
	{	"sball",	&sballinfo	},
	{	"hitachi",	&hitachiinfo	},
	{	"imp",		&impinfo	},
	{	NULL,		NULL		}
};

/***====================================================================***/

int
openDevice(int	argc,char *argv[])
{
int	i;
char	buf[128];
char	*name= DEFAULT_DEVICE;
int	(*openfunc)();

    if (argc>1)
	name= argv[1];
    for (i=0;i<sizeof(devices)/sizeof(struct _devinfo);i++) {
	if ((devices[i].name!=NULL)&&
	    (strcasecmp(devices[i].name,name)==0)) {
		openfunc= devices[i].st->st_rdinit->qi_qopen;
		if ((*openfunc)(read_q,0,0,MODOPEN)==0) {
		    if (argc>2) {
			dev_fd = open(argv[2],O_RDWR|O_NONBLOCK);
			if (dev_fd<0) {
			    fprintf(stderr,"Couldn't open %s\n",argv[2]);
			    return -1;
			}
		    } else {
			sprintf(buf,"/dev/input/%s",name);
			dev_fd = open(buf,O_RDWR|O_NONBLOCK);
			if (dev_fd<0) {
			    sprintf(buf,"/dev/input/%s-pointer",name);
			    dev_fd = open(buf,O_RDWR|O_NONBLOCK);
			    if (dev_fd<0) {
				fprintf(stderr,"Couldn't open %s\n",buf);
				return -1;
			    }
			    pointerMode = 1;
			}
		    }
		    return dev_fd;
		}
		else fprintf(stderr,"open function for %s failed\n",name);
	}
    }
    fprintf(stderr,"device %s not found\n",name);
    return -1;
}

/***====================================================================***/

int
main(int argc,char *argv[])
{
char	buf[128];
int	n,found;
void	(*readfunc)( idevInfo *, char *, int );
int	(*wioctl)( idevInfo *, int, int, char *, int *);
idevInfo	*pInfo;

    if ((dev_fd=openDevice(argc,argv))<0) {
	return 1;
    }
    pInfo= (idevInfo *)read_q->q_ptr;
    readfunc= pInfo->sInfo.readData;
    wioctl= pInfo->sInfo.writeIoctl;
    (*wioctl)(pInfo,IDEVINITDEVICE,0,NULL,&found);
    if (pointerMode) {
	idevPtrMode mode;
	mode.mode=	IDEV_GEN_ALL_EVENTS|IDEV_EXCLUSIVE;
	mode.xAxis=	0;
	mode.yAxis=	1;
	(*wioctl)((idevInfo *)read_q->q_ptr,IDEVSETPTRMODE,
				sizeof(idevPtrMode),(unsigned char *)&mode,
				&found);
    }
    while (1) {
	n= read(dev_fd,buf,128);
	if (n>0) {
	    (*readfunc)( (idevInfo *)read_q->q_ptr, buf, n );
	}
    }
}

