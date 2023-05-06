/**************************************************************************
 *									  *
 *		Copyright ( C ) 1993, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
#ident "$Revision: 1.1 $"

#ifdef INPUT_TEST
#define _KERNEL
#include "sys/types.h"
#undef _KERNEL
#endif
#include "sys/ioctl.h"
#include "sys/errno.h"
#include "sys/termio.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/param.h"
#include "sys/kmem.h"
#include "sys/mload.h"

/* Streams stuff */
#include "sys/stream.h"
#include "sys/strmp.h"
#include "sys/strids.h"
#include "sys/stropts.h"

#include <bstring.h>
#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h"
#define	_KERNEL
extern int sginap(long);
#define STREAMS_DELAY(t) sginap(t)
#else
#include "sys/systm.h"
#endif /* INPUT_TEST */

/* my stuff */
#include "sys/shmiq.h"
#include "sys/idev.h"
#include "sys/imp.h"

#ifndef NULL
#define NULL 0
#endif

#ifndef isdigit
#define	isdigit(c)	(((c)>='0')&&((c)<='9'))
#endif

/**************************************************************************

Since there is not programmer documentation for the IMP mouse,  I'm including
this information in the source. The IMP mouse is a remote (IR) mouse designed
for use in making presentations.

From: carroll@cs.uiuc.edu
To: msc@sgi.sgi.com
Subject: Imp Protocol

Overview:

Imp communicates with the host over a serial interface. All communication
is in the form of 3-byte packets. The DTR line is used for power and must
therefore be on at all times. The RTS line is used for signaling and
operation control. It must be on except when signalling in order to receive
packets.

Communication Modes:

When intially started, Imp sends data at 1200 baud, 7 bits, no parity. These
settings and the format of the data packets conform exactly to standard
Microsoft mouse data formats. Imp can also run at a higher speed with more
data.

Startup:

An Imp reset or start sequence starts with DTR on, and RTS off for 10..1000 ms.
After this reset, Imp will be at 1200 7N1 format and will immediately send a
logon sequence. This consists of the character 'M' followed by the character
'I'. These should arrive within 2 or 3 character times. After this, Imp
can be switched to its normal mode by holding RTS off for at least 1600 ms.
After that, Imp will switch to 4800 baud, 8 bits, no parity. When RTS is
re-asserted, Imp will send an ID packet, after which normal pointer data
will arrive.

Packet Format:

The initial packet format is identical to Microsoft / Logitech format.

The standard packet format (after switching to 4800 8N1) is a 3-byte format
with the bits as follows:

Byte	Bits	Meaning
0	7	Center button state (1 == down, 0 == up)
	6	Sync bit, always 1.
	5	Left Front button state
	4	Right Front button state
	3..2	Y7..Y6
	1..0	X7..X6

1	7	Left Rear button state
	6	Sync bit, always 0
	5..0	X5..X0

2	7	Right Rear button state
	6	Sync bit, always 0
	5..0	Y5..Y0

Note: This is identical to the 1200 baud format except for the presence of
bit 7 in each byte, which is not present at 1200 baud.

The notation Yn and Xn indicate the bits of the mickey deltas in the Y and X
directions. Y and X are 8-bit signed values. The value -128 (0x80) is reserved
for Y and X. Such a value indicates a special packet, in which the button
state bits are valid, but the X and Y values should not be used as mickey
counts. If Y is 0x80, then that indicates low battery in the transmitter. If
X is 0x80, then the transmitter ID has changed.

Motion Adjustments:

In order to provide well tempered motion from the phsyical device, the driver
should maintain three independent motion adjustment values, the Bias, X
sensitivity and Y sensitivity. The sensitivies are simply multipliers, which
scale the mickey counts from physical to logical mickeys. As the these will
be small values (frequently less than 1:1), the driver should maintain spill
counters to track partial mickeys and smooth out quantum effects. The Bias
should be applied before the sensitivities, and is used to damp the motion
indicated by the disk. Bias should be in a range from about 15 to 75. The
physical mickey counts should be adjusted downward by the Bias, and any
resulting counts less than 1 set to 1, after which the sensitivities are
applied. This provides an annulus of fixed speed around the center of the
disc, with smooth acceleration as the user presses further out.

**************************************************************************/

	/*
	 * Macros and variables specific to IMP 
	 * go here
	 */

static idevDesc _imp_desc = {
	IMP_NAME,		/* devName */
	IMP_TYPE,		/* devType */
	IMP_NUM_BUTTONS,	/* nButtons */
	IMP_NUM_VALUATORS,	/* nValuators */
	IMP_NUM_LEDS,		/* nLEDs */
	IMP_NUM_STR_DPYS,	/* nStrDpys */
	IMP_NUM_INT_DPYS,	/* nIntDpys */
	IMP_NUM_BELLS,		/* nBells */
	IMP_FLAGS		/* flags */
};

static struct termio _imp_initial_termio = {
	IGNBRK|IGNPAR,	/* c_iflag */
	0,		/* c_oflag */
	B1200|CS7|CREAD|HUPCL|CNEW_RTSCTS, /* c_cflag */
	0,		/* c_lflag */
	0,		/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   5,    1,     0  }
};

static struct termio _imp_normal_termio = {
	IGNBRK|IGNPAR,	/* c_iflag */
	0,		/* c_oflag */
	B4800|CS8|CREAD|HUPCL|CNEW_RTSCTS, /* c_cflag */
	0,		/* c_lflag */
	0,		/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   5,    1,     0  }
};

static idevValuatorDesc _imp_dflt_val_desc = {
	IMP_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	IMP_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	IMP_VALUATOR_MIN,		/* hwMinVal */
	IMP_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_EITHER,			/* possibleModes */
	IDEV_EITHER,			/* mode */
	IMP_VALUATOR_RESOLUTION,	/* resolution */
	IMP_VALUATOR_MIN,		/* minVal */
	IMP_VALUATOR_MAX		/* maxVal */
};

static idevTransform impDfltAccel = {
    IDEV_ACCEL,
    IDEV_ACCEL,
    0,
    8,		    /* acceleratorn */
    10,		    /* acceleratord */
    0, 
    0,
    0,
    0
};

static char imp_dflt_btnmap[IMP_NUM_BUTTONS] = {0, 1, 3, 2, 4};
   
static int imp_osync = 0;		/* track out of sync events */

static int impref = 0;                /* reference count for unload */
char *impmversion = M_VERSION;
int impdevflag = 0;

const char* cnb_name = "cn";
const char* lfb_name = "lf";
const char* rfb_name = "rf";
const char* lrb_name = "lr";
const char* rrb_name = "rr";

/*
 * XXX Need to eight figure out how to do this with IDEV_CURVE or
 * provice ioctl's to change the bias
 */
#define DFLT_BIAS   50
int bias(signed char bias, signed char delta)
{
    register int sign = 0;
    
    if (delta != 0) {
	if (delta < 0) {
	    sign = 1;
	    delta = -delta;
	}
	delta -= bias;
	if (delta < 0) delta = 1;
    }
    return sign ? -delta : delta;    
}

/* ARGSUSED */
static void
imp_intr( idevInfo *pInfo, unsigned char *str, int len )
{
	imp_state_t *imp= (imp_state_t *)pInfo;
	unsigned char newButtons;
	unsigned char buttons;
	signed char deltaX, deltaY;

	while (len--) {
		register c = *str++;
		if (imp->wantsignon) {
		    if (imp->wantsignon == 2 && c == 'M')
			imp->wantsignon--;
		    else if (imp->wantsignon == 1 && c == 'I')
			imp->wantsignon--;
		    else
			imp->wantsignon = 2;
		}
		/*
		 * Restart the state machine whenever
		 * it gets a button byte.  This makes
		 * the mouse work better when
		 * characters get dropped.
		 */
		if ((c&IMP_SYNC)) {
			 imp->state = IMP_BUTTONS;
		}
		imp->report[ imp->state ] = c;
		if (++imp->state > IMP_LOWY) {
			if (!(imp->report[IMP_BUTTONS] & IMP_SYNC)
			    || (imp->report[IMP_LOWX] & IMP_SYNC)
			    || (imp->report[IMP_LOWY] & IMP_SYNC)) {
#ifdef DEBUG
				cmn_err( CE_WARN, "bad imp report" ) ;
#endif /* DEBUG */
				imp->state = IMP_BUTTONS;
				imp_osync++;
				continue;
			}
			deltaY = ((imp->report[IMP_BUTTONS] & 0x0c) << 4)
					| (imp->report[IMP_LOWY] & IMP_VALBITS);

     			deltaX = ((imp->report[IMP_BUTTONS] & 0x03) << 6) 
					| (imp->report[IMP_LOWX] & IMP_VALBITS);

			if (deltaX == 0x80)
			    imp->xmitter_id_changed++;
			else if (deltaY == 0x80)
			    imp->low_battery++;
			else if ((deltaX)||(deltaY)) {
				idevValuatorState	new;
				int			rtrn;
				new.firstValuator = 0;
				new.nValuators = 2;
				new.value[0] =	bias(imp->bias, deltaX);
				new.value[1] =	bias(imp->bias, deltaY);
				(void)idevGenValEvents(&imp->info,&new,0);
			}

			buttons = imp->report[IMP_BUTTONS];
			newButtons = 0;
			if (buttons & IMP_CENTERB)
			    newButtons = (1 << imp->btnmap[CNBUTTON]);
			if (buttons & IMP_LFB)
			    newButtons |= (1 << imp->btnmap[LFBUTTON]);
			if (buttons & IMP_RFB)
			    newButtons |= (1 << imp->btnmap[RFBUTTON]);
			if (imp->report[IMP_LOWX] & IMP_LRB)
			    newButtons |= (1 << imp->btnmap[LRBUTTON]);
			if (imp->report[IMP_LOWY] & IMP_RRB)
			    newButtons |= (1 << imp->btnmap[RRBUTTON]);

			if ( imp->bstate != newButtons ) {
				unsigned char mask = ALLBUTTONS;
				idevGenBtnEvents( &imp->info,
						&mask, &newButtons );
			}
			imp->state = IMP_BUTTONS;
		}
	}

	return;
}

static int
imp_streams_ioctl(idevInfo *pInfo, int cmd, uint count, caddr_t data)
{
mblk_t	*iob;
struct iocblk	*iocb;

	iob = allocb( sizeof(struct iocblk), BPRI_MED );
	if (iob==NULL) {
		cmn_err(CE_WARN,"Can't allocate streams buffer for M_IOCTL. Line settings not changed.\n");
		return 0;
	}
	iob->b_cont= allocb( sizeof(struct termio), BPRI_MED );
	if (iob->b_cont==NULL) {
		cmn_err(CE_WARN,"Can't allocate streams buffer for M_DATA. Line settings not changed.\n");
		freemsg(iob);
		return 0;
	}
	iob->b_datap->db_type = M_IOCTL;
	iocb = (struct iocblk *)iob->b_wptr;
	iob->b_wptr += sizeof(struct iocblk);
	iocb->ioc_cmd = cmd;
/* XXXjwag - uid and gid are now part of cred struct which hasn't
 * been allocated.. but who uses this anyway??
	iocb->ioc_uid = 0;
	iocb->ioc_gid = 0;
 */
	iocb->ioc_id =  0;
	iocb->ioc_count = count;
	iocb->ioc_error = 0;
	iocb->ioc_rval = 0;
	bcopy(data, iob->b_cont->b_wptr, count);
	iob->b_cont->b_wptr += count;
	putnext( pInfo->sInfo.wq, iob );
	return 1;
}

static int
imp_init_device( imp_state_t* imp )
{
    int arg;
    int ret;
    int i;

    ret = imp_streams_ioctl(&imp->info, TCSETAW,
		            sizeof(_imp_initial_termio),
			    (caddr_t)&_imp_initial_termio);
    if (!ret) return ret;
    /* Drop RTS for a little while */
    arg = 2;
    ret = imp_streams_ioctl(&imp->info, TCXONC, sizeof(arg), (caddr_t)&arg);  
    if (!ret) return ret;
    STREAMS_DELAY(HZ*0.08);
    /* Raise RTS and wait for 2 sign on characters */
    arg = 3;
    ret = imp_streams_ioctl(&imp->info, TCXONC, sizeof(arg), (caddr_t)&arg);  
    if (!ret) return ret;
    imp->wantsignon = 2; i = 2;
    while (imp->wantsignon && i-- > 0)
	STREAMS_DELAY(HZ*0.1);
    imp->wantsignon = 0;
    /* Drop RTS for long enough to cause imp to switch baud rates */
    arg = 2;
    ret = imp_streams_ioctl(&imp->info, TCXONC, sizeof(arg), (caddr_t)&arg);  
    if (!ret) return ret;
    STREAMS_DELAY(HZ*1.7);
    /* Raise RTS */
    arg = 3;
    ret = imp_streams_ioctl(&imp->info, TCXONC,
			    sizeof(arg), (caddr_t)&arg);  
    if (!ret) return ret;
    /* Change settings */
    ret = imp_streams_ioctl(&imp->info, TCSETAW,
			    sizeof(_imp_normal_termio),
			    (caddr_t)&_imp_normal_termio);
    return ret;
}

/* ARGSUSED */
static int
imp_other_control(imp_state_t *imp, idevOtherControl *pCtrl)
{
    pCtrl->name[IDEV_CTRL_NAME_LEN]=    '\0';
    pCtrl->data[IDEV_CTRL_DATA_LEN]=    '\0';

    if (strcmp(pCtrl->name, "bias") == 0) {
        if (isdigit(pCtrl->data[0])) {
	    int bias = atoi(pCtrl->data);
	    if (bias <= IMP_BIAS_MAX) {
		imp->bias = bias;
		return 1;
	    }
	}
    } else if (strncmp(pCtrl->name, "scale",5) == 0) {
	idevTransform *pTrans= NULL;
	if ((pCtrl->name[5]=='x') || (pCtrl->name[5]=='X'))
	    pTrans = &imp->info.pInfo.xTransform;
	else if ((pCtrl->name[5] == 'y') || (pCtrl->name[5]=='Y'))
	    pTrans= &imp->info.pInfo.yTransform;
	if ((pTrans!=NULL)&&(isdigit(pCtrl->data[0]))) {
	    int denom, num = atoi(pCtrl->data);
	    char *str= pCtrl->data;
	    while ((*str)&&(isdigit(*str)))
		str++;
	    if (*str=='/')
		str++;
	    if (isdigit(*str))
		denom= atoi(str);
	    else
	    	denom= 1;
	    if (num==0)	num= 1;
	    if (denom==0) denom= 1;

	    pTrans->numerator= num;
	    pTrans->denominator= denom;
	    return 1;
	}
    } else if (strcmp(pCtrl->name,"btnmap") == 0) {
	/* Remap the buttons */
	int i, j;
	const char* bn;
	char *str= pCtrl->data;
	if (strlen(str) >= IMP_NUM_BUTTONS*3 - 1) {
	    int i;
	    for (i = 0; i < IMP_NUM_BUTTONS; i++, str += 3) {
		if (strncmp(str, cnb_name, 2) == 0)
		    imp->btnmap[CNBUTTON] = i; 
		if (strncmp(str, lfb_name, 2) == 0)
		    imp->btnmap[LFBUTTON] = i; 
		if (strncmp(str, rfb_name, 2) == 0)
		    imp->btnmap[RFBUTTON] = i; 
		if (strncmp(str, lrb_name, 2) == 0)
		    imp->btnmap[LRBUTTON] = i; 
		if (strncmp(str, rrb_name, 2) == 0)
		    imp->btnmap[RRBUTTON] = i;
	    }
	    return 1;
	} 
    }

    return 0;
}

/* ARGSUSED */
static int
imp_other_query(imp_state_t *imp, idevOtherQuery *pQuery)
{
    if (strcmp(pQuery->name, "bias") == 0) {
        sprintf(pQuery->data, "%d", imp->bias);
	return 1;
    } else if (strncmp(pQuery->name, "scale",5) == 0) {
	idevTransform *pTrans= NULL;
	if ((pQuery->name[5]=='x') || (pQuery->name[5]=='X'))
	    pTrans = &imp->info.pInfo.xTransform;
	else if ((pQuery->name[5] == 'y') || (pQuery->name[5]=='Y'))
	    pTrans= &imp->info.pInfo.yTransform;
	if ((pTrans!=NULL)) { 
	    sprintf(pQuery->data, "%d/%d",
		    pTrans->numerator, pTrans->denominator);
	    return 1;
        }
    } else if (strcmp(pQuery->name,"btnmap") == 0) {
	int i, j;
	char* str = pQuery->data;
	const char* bn;
	
	for (i = 0; i < IMP_NUM_BUTTONS; i++, str += 3)
	    for (j = 0; j < IMP_NUM_BUTTONS; j++) {
		if (imp->btnmap[j] == i) {
		    switch (j) {
			case CNBUTTON: bn = cnb_name; break;
			case LFBUTTON: bn = lfb_name; break;
			case RFBUTTON: bn = rfb_name; break;
			case LRBUTTON: bn = lrb_name; break;
			case RRBUTTON: bn = rrb_name; break;
		    }
		    sprintf(str, i == IMP_NUM_BUTTONS-1 ? "%s" : "%s, ", bn);
	            break; 
		} 
	    }
	return 1;
    }

    return 0;
}

static int
imp_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
imp_state_t *imp= (imp_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _imp_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&imp->info,
					(idevGetSetValDesc*)stuff);
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(IMP_NUM_BUTTONS)) {
				stuff[0]= imp->bstate;
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&imp->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals	*vals= (idevBitVals *)stuff;
				ok= 1;
				imp->bactive &= ~vals->mask[0];
				imp->bactive |= (vals->mask[0]&vals->value[0]);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals	*vals= (idevBitVals *)stuff;
				imp->vactive &= ~vals->mask[0];
				imp->vactive |= (vals->mask[0]&vals->value[0]);
				ok= 1;
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&imp->info,
					(idevValuatorState *)stuff,
					IDEV_VALS_ABSOLUTE|QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&imp->info,
					(idevValuatorState *)stuff,QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevSetValDesc(&imp->info,
					(idevGetSetValDesc *)stuff);
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&imp->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&imp->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&imp->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevSetTransform(&imp->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&imp->info,
					(idevGetSetTransform *)stuff);
			break;
                case IDEVOTHERCONTROL:
                        found++;
                        if (size>=sizeof(idevOtherControl))
                            ok = imp_other_control(imp,(idevOtherControl*)stuff);
			else
			    ok = 0;
                        break;
                case IDEVOTHERQUERY:
                        found++;
                        if (size>=sizeof(idevOtherQuery))
                            ok = imp_other_query(imp,(idevOtherQuery*)stuff);
			else
			    ok = 0;
                        break;
		case IDEVINITDEVICE:
			found++;
			ok = imp_init_device(imp);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
imp_open( register queue_t *rq, dev_t *devp, int flag, int sflag, struct cred *crp )
{
	register imp_state_t *imp;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO;

	if ( !rq->q_ptr ) {
		if ( !(imp = (imp_state_t *)kmem_alloc(sizeof *imp, KM_SLEEP)))
			return ENOMEM;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( imp, sizeof *imp ) ;

		imp->dropsync = 0;
		for (i=0;i<IMP_NUM_VALUATORS;i++) {
			imp->vdesc[i] = _imp_dflt_val_desc;
			imp->vstate[i] = 0;
			imp->vtrans[i] = impDfltAccel;
		}
		imp->vactive= 0x03;
		imp->bstate=  0x00;
		imp->bactive= ALLBUTTONS;
		bcopy(imp_dflt_btnmap, imp->btnmap, sizeof(imp_dflt_btnmap));
		
		imp->bias = IMP_BIAS_DFLT;

		imp->info.bInfo.nBtn = IMP_NUM_BUTTONS;
		imp->info.bInfo.active = &imp->bactive;
		imp->info.bInfo.state = &imp->bstate;

		imp->info.vInfo.nVal = IMP_NUM_VALUATORS;
		imp->info.vInfo.sysValue = imp->vstate;
		imp->info.vInfo.desc = imp->vdesc;
		imp->info.vInfo.transform = imp->vtrans;
		imp->info.vInfo.active = &imp->vactive;
		imp->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;

		imp->info.pInfo.xAxis = imp->info.pInfo.yAxis = 255;
		imp->info.pInfo.minX = imp->info.pInfo.minY= 0;
		imp->info.pInfo.maxX = imp->info.pInfo.maxY = 0;
		imp->info.pInfo.x= imp->info.pInfo.y= 0;
		imp->info.pInfo.xTransform = impDfltAccel;
		imp->info.pInfo.yTransform = impDfltAccel;

		imp->info.sInfo.shmiqid.devminor = 0;
		imp->info.sInfo.shmiqid.index = 0;
		imp->info.sInfo.rq = rq;
		imp->info.sInfo.wq =	WR(rq);
		imp->info.sInfo.readData = imp_intr;
		imp->info.sInfo.writeIoctl = imp_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &imp->info;
	}
	impref++;
	return 0;
}

/* ARGSUSED */
static int
imp_close( register queue_t *rq, int flag, struct cred *crp )
{
	imp_state_t *imp = (imp_state_t *)rq->q_ptr;

	kmem_free((char *)imp, sizeof(*imp));
	impref--;
	return 0;
}

int
impunload(void)
{
        return(impref ? EBUSY : 0);
}

	/*
	 * stream module definition
	 */

static struct module_info imp_mod_info = {
	0,				/* module ID */
	IMP_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit imp_rinit = {
	idev_rput, NULL, imp_open, imp_close,
	NULL, &imp_mod_info, NULL
};

static struct qinit imp_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &imp_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab impinfo = {
	&imp_rinit, &imp_winit, 0, 0
} ;
