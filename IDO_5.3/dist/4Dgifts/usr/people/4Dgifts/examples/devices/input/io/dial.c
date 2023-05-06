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
#ident "$Revision: 1.13 $"

#ifdef INPUT_TEST
#define _KERNEL
#include <sys/types.h>
#undef _KERNEL
#else
#include <sys/types.h>
#endif
#include <sys/errno.h>
#include <sys/cmn_err.h>
#include <sys/param.h>
#include <sys/kmem.h>
#include <sys/mload.h>

#include <sys/ioctl.h>
#include <sys/termio.h>
#include <sys/debug.h>

/* Streams stuff */
#include <sys/stream.h>
#include <sys/strmp.h>

#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h"
#define _KERNEL
#endif /* INPUT_TEST */

/* my stuff */
#include <sys/shmiq.h>
#include <sys/idev.h>
#include <sys/dial.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to DIAL 
	 * go here
	 */

static idevDesc _dial_desc = {
	DIAL_NAME,		/* devName */
	DIAL_TYPE,		/* devType */
	DIAL_NUM_BUTTONS,	/* nButtons */
	DIAL_NUM_VALUATORS,	/* nValuators */
	DIAL_NUM_LEDS,		/* nLEDs */
	DIAL_NUM_STR_DPYS,	/* nStrDpys */
	0,			/* nIntDpys */
	0,			/* nBells */
	0,			/* flags */
};

static idevValuatorDesc _dial_dflt_val_desc = {
	DIAL_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	DIAL_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	DIAL_VALUATOR_MIN,		/* hwMinVal */
	DIAL_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_EITHER,			/* possibleModes */
	IDEV_ABSOLUTE,			/* mode */
	DIAL_VALUATOR_RESOLUTION,	/* resolution */
	DIAL_VALUATOR_MIN,		/* minVal */
	DIAL_VALUATOR_MAX		/* maxVal */
};

static idevStrDpyDesc _dial_sdpy = {
	0,
	DIAL_STR_DPY_MAX_LEN,
	DIAL_STR_DPY_ALL_SAME,
	DIAL_STR_DPY_SYMBOL_SET,
};

static int dialref = 0;			/* reference count for unload */
char *dialmversion = M_VERSION;
int dialdevflag = 0;

	/* dial parser state machine states */
#define DIAL_WHICH_DEVICE       0
#define DIAL_VALUE_HIGH         1
#define DIAL_VALUE_LOW          2
#define DIAL_N_STATES           3

	/* dial/button box commands */
#define DIAL_INITIALIZE                 0x20
#define DIAL_SET_LEDS                   0x75
#define DIAL_SET_TEXT                   0x61
#define DIAL_SET_AUTO_DIALS             0x50
#define DIAL_SET_AUTO_DELTA_DIALS       0x51
#define DIAL_SET_FILTER			0x53
#define DIAL_SET_BUTTONS_MOM_TYPE       0x71
#define DIAL_SET_AUTO_MOM_BUTTONS       0x73

	/* dial/button box replies and events */
#define DIAL_INITIALIZED        0x20
#define DIAL_BASE               0x30
#define DIAL_DELTA_BASE         0x40
#define DIAL_PRESS_BASE         0xc0
#define DIAL_RELEASE_BASE       0xe0

	/* macros to determine replie type */
#define IS_DIAL_EVENT(ch)       (((ch)>=DIAL_BASE)&&((ch)<DIAL_BASE+DIAL_NUM_VALUATORS))
#define IS_KEY_PRESS(ch)        (((ch)>=DIAL_PRESS_BASE)&&((ch)<DIAL_PRESS_BASE+DIAL_NUM_BUTTONS))
#define IS_KEY_RELEASE(ch)      (((ch)>=DIAL_RELEASE_BASE)&&((ch)<DIAL_RELEASE_BASE+DIAL_NUM_BUTTONS))
#define IS_INIT_EVENT(ch)       ((ch)==DIAL_INITIALIZED)

static void dial_check_init(dial_state_t *);

static int
dial_enable_buttons(dial_state_t *dial, register idevBitVals *btns)
{
	int i;
	mblk_t *mbp;
	register char *dcmd;

	for (i=0;i<idevSize(DIAL_NUM_BUTTONS);i++) {
		dial->bactive[i] &= ~btns->mask[i];
		dial->bactive[i] |= (btns->mask[i]&btns->value[i]);
	}
	if (mbp = allocb(5,BPRI_MED)) {
		dcmd = mbp->b_wptr;
		dcmd[0] = DIAL_SET_AUTO_MOM_BUTTONS;
		dcmd[1] = btns->mask[0];
		dcmd[2] = btns->mask[1];
		dcmd[3] = btns->mask[2];
		dcmd[4] = btns->mask[3];
		mbp->b_wptr += 5;
		putnext( dial->info.sInfo.wq, mbp );
	}
	else {
		cmn_err(CE_WARN,"alloc failed, active buttons not changed\n");
		return 0;
	}
	if (mbp= allocb(5,BPRI_MED)) {
		dcmd = mbp->b_wptr;
		dcmd[0] = DIAL_SET_BUTTONS_MOM_TYPE;
		dcmd[1] = btns->mask[0];
		dcmd[2] = btns->mask[1];
		dcmd[3] = btns->mask[2];
		dcmd[4] = btns->mask[3];
		mbp->b_wptr +=   5;
		putnext( dial->info.sInfo.wq, mbp );
	}
	else {
		cmn_err(CE_WARN,"alloc failed, active buttons not changed\n");
		return 0;
	}
	return 1;
}

/* ARGSUSED */
static int
dial_enable_valuators(dial_state_t *dial, register idevBitVals *vals)
{
	int i;
	mblk_t *mbp;
	register char *dcmd;

	for (i=0;i<idevSize(DIAL_NUM_VALUATORS);i++) {
		dial->vactive[i]&= ~vals->mask[i];
		dial->vactive[i]|= (vals->mask[i]&vals->value[i]);
	}
	if (mbp= allocb(3,BPRI_MED)) {
		dcmd = mbp->b_wptr;
		dcmd[0] = DIAL_SET_AUTO_DIALS;
#ifdef NOTDEF
		dcmd[1] = dial->vactive[0];
		dcmd[2] = dial->vactive[1];
#else
		dcmd[1] = 0xff;
		dcmd[2] = 0xff;
#endif
		mbp->b_wptr +=   3;
		putnext( dial->info.sInfo.wq, mbp );
	}
	else {
		cmn_err(CE_WARN,"alloc failed, active dials not changed\n");
		return 0;
	}
	return 1;
}

/* ARGSUSED */
static int
dial_set_resolution(dial_state_t *dial, register idevGetSetValDesc *vals)
{

    if ((vals==NULL)||(vals->flags&IDEV_SET_RESOLUTION)) {
	mblk_t  *mbp;
	unsigned char filter;

	if (vals==NULL)				filter= 0;
	else if (vals->desc.resolution>768)	filter= 0;
	else if (vals->desc.resolution>384)	filter= 1;
	else if (vals->desc.resolution>192)	filter= 2;
	else if (vals->desc.resolution>96)	filter= 3;
	else if (vals->desc.resolution>48)	filter= 4;
	else if (vals->desc.resolution>24)	filter= 5;
	else if (vals->desc.resolution>12)	filter= 6;
	else 					filter= 7;
	if ((vals==NULL)||(vals->flags&IDEV_SET_ALL)) {
		if (mbp= allocb(DIAL_NUM_VALUATORS*3,BPRI_MED)) {
			register int i;
			for (i=0;i<DIAL_NUM_VALUATORS;i++) {
				mbp->b_wptr[0] = DIAL_SET_FILTER;
				mbp->b_wptr[1] = i;
				mbp->b_wptr[2] = filter;
				mbp->b_wptr += 3;
				dial->vdesc[i].resolution= 1024/(1<<filter);
			}
		}
	}
	else {
		if (mbp= allocb(3,BPRI_MED)) {
			mbp->b_wptr[0] = DIAL_SET_FILTER;
			mbp->b_wptr[1] = vals->valNum;
			mbp->b_wptr[2] = filter;
			mbp->b_wptr += 3;
			dial->vdesc[vals->valNum].resolution= 1024/(1<<filter);
		}
	}
	if (mbp!=NULL) {
		putnext( dial->info.sInfo.wq, mbp );
	}
	else {
		cmn_err(CE_DEBUG,"Couldn't allocate streams buffer.  Dial filter value not changed.\n");
	}
    }
    return 1;
}

static int
dial_set_LEDs(dial_state_t *dial, register idevBitVals *LEDs)
{
	int i;
	mblk_t *mbp;
	register char *dcmd;

	for (i=0;i<idevSize(DIAL_NUM_LEDS);i++) {
		dial->ledstate[i]&= ~(LEDs->mask[i]);
		dial->ledstate[i]|= (LEDs->mask[i]&LEDs->value[i]);
	}
	if (mbp = allocb(5,BPRI_MED)) {
		dcmd = mbp->b_wptr;
		dcmd[0] = DIAL_SET_LEDS;
		dcmd[1] = dial->ledstate[0];
		dcmd[2] = dial->ledstate[1];
		dcmd[3] = dial->ledstate[2];
		dcmd[4] = dial->ledstate[3];
		mbp->b_wptr +=   5;
		putnext( dial->info.sInfo.wq, mbp );
	}
	else {
		cmn_err(CE_WARN,"Couldn't allocate streams buffer -- LEDs not changed.\n");
		return 0;
	}
	return 1;
}

static int
dial_get_str_dpy_state(dial_state_t *dial, idevStrDpyState *dpy)
{
	if (dpy->nDpy==0) {
		dpy->on=	dial->sdpy_on;
		dpy->str_len=	dial->sdpy_len;
		if (dpy->str_len>0) {
		    register int i;
		    for (i=0;i<dpy->str_len;i++) {
			dpy->string[i]=	dial->sdpystate[i];
		    }
		    dpy->string[i]= 0xffff;
		}
		return 1;
	}
	return 0;
}

static int
dial_set_str_dpy(dial_state_t *dial, register idevStrDpyState *dpy)
{
	mblk_t	*mbp;

	if (dpy->nDpy == 0) {
		register int i;
		register char *str= dial->sdpystate;
		register unsigned short *syms= dpy->string;

		if (dpy->str_len>DIAL_STR_DPY_MAX_LEN)
		    dpy->str_len= DIAL_STR_DPY_MAX_LEN;

		dial->sdpy_on= dpy->on;
		if (dpy->on)
			dial->sdpy_len=	dpy->str_len;
		else 	dial->sdpy_len= 0;
		for (i=0;i<dpy->str_len;i++) {
		    str[i]= syms[i]&0xff;
		}
		while (i<DIAL_STR_DPY_MAX_LEN) {
		    str[i]= ' ';
		    i++;
		}
		str[DIAL_STR_DPY_MAX_LEN]= '\0';
		if (mbp= allocb(DIAL_STR_DPY_MAX_LEN+1,BPRI_MED)) {
			mbp->b_wptr[0]= 'a';
			bcopy(str,&mbp->b_wptr[1],DIAL_STR_DPY_MAX_LEN);
			mbp->b_wptr+= DIAL_STR_DPY_MAX_LEN+1;
			putnext( dial->info.sInfo.wq, mbp );
		}
		else {
			cmn_err(CE_WARN,"Couldn't allocate streams buffer -- string display not changed\n");
			return 0;
		}
		return 1;
	}
	return 0;

}

/* ARGSUSED */
static int
dial_init_device(dial_state_t *dial, int size, char *stuff)
{
	mblk_t *mbp;

	if (!idevChangeLineSettings(&dial->info,NULL)) {
		cmn_err(CE_WARN,"Dial box not initialized.\n");
		return 0;
	}
	if (mbp= allocb(1,BPRI_MED)) {
		mbp->b_wptr[0] = DIAL_INITIALIZED;
		mbp->b_wptr += 1;
		putnext( dial->info.sInfo.wq, mbp );
	}
	else {
		cmn_err(CE_WARN,"Couldn't allocate streams buffer -- dial box not initialized\n");
		return 0;
	}
	dial->initialized= 1;
	dial->checkinit_id = STREAMS_TIMEOUT1((strtimeoutfunc_t)dial_check_init, dial, 50, NULL);
	return 1;
}

static void
dial_check_init( dial_state_t *dial )
{
	if ((!dial->ready)&&(dial->checkinit_id)) {
	    if (dial->errorCount++<5) {
		dial_init_device(dial, 0, NULL);
		return;
	    }
	    else {
		cmn_err(CE_WARN,"Dial box not responding.\n");
	    }
	}
	dial->checkinit_id= 0;
        return;
}


static  void
dial_setup(dial_state_t *dial)
{
idevBitVals     vals;
int		i;

	dial->ready=        1;
	for (i=0;i<idevSize(DIAL_NUM_LEDS);i++) {
		vals.mask[i] = ~0;
		vals.value[i] = dial->ledstate[i];
	}
	dial_set_LEDs(dial,&vals);

	for (i=0;i<idevSize(DIAL_NUM_BUTTONS);i++) {
		vals.mask[i] = ~0;
		vals.value[i] = dial->bactive[i];
	}
	dial_enable_buttons(dial,&vals);

	for (i=0;i<idevSize(DIAL_NUM_VALUATORS);i++) {
		vals.mask[i] = ~0;
		vals.value[i] = dial->vactive[i];
	}
	dial_enable_valuators(dial,&vals);

	dial_set_resolution(dial,NULL);
	dial->errorCount= 0;
	return;
}

static void
dial_intr( idevInfo *pInfo, unsigned char *str, int len )
{
	dial_state_t *dial= (dial_state_t *)pInfo;
	register unsigned char ch;
	while (len > 0) {
		ch = *str++;
		if ((dial->state != 0) || IS_DIAL_EVENT(ch)) {
			switch (dial->state) {
				case DIAL_WHICH_DEVICE:
					dial->which = ch-DIAL_BASE;
					dial->state++;
					break;
				case DIAL_VALUE_HIGH:
					dial->value = (ch<<8);
					dial->state++;
					break;
				case DIAL_VALUE_LOW:
				{
					int delta;

					dial->value|= ch;
					delta= dial->value-
						dial->hwvstate[dial->which];

					dial->hwvstate[dial->which]=
						dial->value;

					idevGenValEvent(&dial->info,
							dial->which,delta,0);
					dial->state = 0;
					break;
				}
				default:
					cmn_err(CE_WARN,"Impossible state %d in dial_intr.\n");
					dial->state = 0;
					break;
			}
			dial->errorCount = 0;
		}
		else if (IS_KEY_PRESS(ch)) {
			unsigned char btnNum = (ch-DIAL_PRESS_BASE)^0x18;
			idevGenBtnEvent(&dial->info,btnNum,
						IDEV_BTN_PRESS|IDEV_FAKE_EVENT);
			dial->errorCount = 0;
		}
		else if (IS_KEY_RELEASE(ch)) {
			unsigned char btnNum = (ch-DIAL_RELEASE_BASE)^0x18;
			idevGenBtnEvent(&dial->info,btnNum,IDEV_FAKE_EVENT);
			dial->errorCount = 0;
		}
		else if (ch == DIAL_INITIALIZED) {
			dial_setup(dial);
			dial->errorCount = 0;
		}
		else {
			cmn_err(CE_WARN,"unexpected char %d from dial\n",ch);
			if (dial->errorCount++>5) { /* try to reinitialize */
				cmn_err(CE_WARN,"Reinitializing dial box\n");
				dial_init_device(dial,0,NULL);
			}
		}
		len--;
	}
	return;
}


	/*
	 * DIAL ioctl routine
	 * Handles DIAL ioctls, returns TRUE if the ioctl
	 * was known and successful.  Returns FALSE 
	 * otherwise.  Sets *pFound to TRUE if the ioctl
	 * was known, FALSE otherwise.
	 */
static int
dial_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
dial_state_t *dial= (dial_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _dial_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&dial->info,
					(idevGetSetValDesc*)stuff);
			break;
		case IDEVGETSTRDPYDESC:
			found++;
			if (size>=sizeof(idevStrDpyDesc)) {
				idevStrDpyDesc *pDpy= (idevStrDpyDesc *)stuff;
				if (pDpy->dpyNum==0) {
					*pDpy= _dial_sdpy;
					ok = 1;
				}
			}
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(DIAL_NUM_BUTTONS)) {
				bcopy(dial->bstate,stuff,
						idevSize(DIAL_NUM_BUTTONS));
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&dial->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVGETLEDS:
			found++;
			if (size>=idevSize(DIAL_NUM_LEDS)) {
				bcopy(dial->ledstate,stuff,
						idevSize(DIAL_NUM_LEDS));
				ok = 1;
			}
			break;
		case IDEVGETSTRDPY:
			found++;
			if (size>=sizeof(idevStrDpyState))
				ok = dial_get_str_dpy_state(dial,
					(idevStrDpyState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= dial_enable_buttons(dial,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= dial_enable_valuators(dial,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&dial->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|IDEV_VALS_ABSOLUTE
					|QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&dial->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc)) {
				idevGetSetValDesc *pSet;
				pSet=(idevGetSetValDesc *)stuff;
				ok = idevSetValDesc(&dial->info,pSet) &&
					dial_set_resolution(dial, pSet);
			}
			break;
		case IDEVPTRCONTROL:
			found++;
			if (size>=sizeof(idevPtrControl))
				ok = idevSetPtrCtrl(&dial->info,
					(idevPtrControl *)stuff);
			break;
		case IDEVSETLEDS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok = dial_set_LEDs(dial,(idevBitVals *)stuff);
			}
			break;
		case IDEVSETSTRDPY:
			found++;
			if (size>=sizeof(idevStrDpyState))
				ok = dial_set_str_dpy(dial,
					(idevStrDpyState *)stuff);
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&dial->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&dial->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&dial->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevSetTransform(&dial->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&dial->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVINITDEVICE:
			found++;
			ok = dial_init_device(dial,size,stuff);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
dial_open( queue_t *rq, dev_t *dev, int flag, int sflag, struct cred *cred )
{
	register dial_state_t *dial;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO;

	if ( !rq->q_ptr ) {
		if ( !(dial = (dial_state_t *)kmem_alloc(sizeof *dial, KM_SLEEP)))
			return ENOMEM;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( dial, sizeof *dial ) ;

		dial->initialized = 1;
		dial->errorCount = 0;
		dial->state = 0;
		for (i=0;i<DIAL_NUM_VALUATORS;i++) {
			dial->vdesc[i]= _dial_dflt_val_desc;
			dial->hwvstate[i]= 0;
			dial->sysvstate[i]= 0;
			dial->vtrans[i] = idevDfltAccel;
			dial->vtrans[i].possible = IDEV_ACCEL|IDEV_CURVE;
			dial->vtrans[i].which = IDEV_ACCEL;
		}
		for (i=idevSize(DIAL_NUM_VALUATORS)-1;i>=0;i--) {
			dial->vactive[i]= ~0;
		}
		for (i=idevSize(DIAL_NUM_BUTTONS)-1;i>=0;i--) {
			dial->bstate[i]=	0;
			dial->bactive[i]=	~0;
		}
		for (i=idevSize(DIAL_NUM_LEDS)-1;i>=0;i--) {
			dial->ledstate[i]=	0;
		}
		for (i=0;i<DIAL_STR_DPY_MAX_LEN;i++) {
		    dial->sdpystate[i]= ' ';
		}
		dial->sdpy_on=	0;
		dial->sdpy_len= 0;

		dial->info.bInfo.nBtn = DIAL_NUM_BUTTONS;
		dial->info.bInfo.active = dial->bactive;
		dial->info.bInfo.state = dial->bstate;

		dial->info.vInfo.nVal = DIAL_NUM_VALUATORS;
		dial->info.vInfo.sysValue = dial->sysvstate;
		dial->info.vInfo.desc = dial->vdesc;
		dial->info.vInfo.transform = dial->vtrans;
		dial->info.vInfo.active = dial->vactive;
		dial->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;

		dial->info.pInfo.xAxis = dial->info.pInfo.yAxis = 255;
		dial->info.pInfo.minX = dial->info.pInfo.minY = 0;
		dial->info.pInfo.maxX = dial->info.pInfo.maxY = 0;
		dial->info.pInfo.x= dial->info.pInfo.y= 0;
		dial->info.pInfo.xTransform = idevDfltScale;
		dial->info.pInfo.yTransform = idevDfltScale;
		dial->info.pInfo.xTransform.possible= IDEV_ACCEL|IDEV_CURVE;
		dial->info.pInfo.yTransform.possible= IDEV_ACCEL|IDEV_CURVE;
		dial->info.pInfo.xTransform.which= IDEV_ACCEL;
		dial->info.pInfo.yTransform.which= IDEV_ACCEL;

		dial->info.sInfo.shmiqid.devminor = 0;
		dial->info.sInfo.shmiqid.index = 0;
		dial->info.sInfo.rq = rq;
		dial->info.sInfo.wq =	WR(rq);
		dial->info.sInfo.readData = dial_intr;
		dial->info.sInfo.writeIoctl = dial_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &dial->info;
	}

	dialref++;

	return 0;
}

/* ARGSUSED */
static int
dial_close( register queue_t *rq, int flag, struct cred *cred )
{
	dial_state_t *dial = (dial_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	if (dial->checkinit_id) {
	    untimeout(dial->checkinit_id);
	    dial->checkinit_id= 0;
	}
	kmem_free( (char *)dial , sizeof *dial);
	dialref--;
	return 0;
}

int
dialunload(void)
{
	return(dialref ? EBUSY : 0);
}

	/*
	 * stream module definition
	 */

static struct module_info dial_mod_info = {
	0,				/* module ID */
	DIAL_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit dial_rinit = {
	idev_rput, NULL, dial_open, dial_close,
	NULL, &dial_mod_info, NULL
};

static struct qinit dial_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &dial_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab dialinfo = {
	&dial_rinit, &dial_winit, 0, 0
} ;
