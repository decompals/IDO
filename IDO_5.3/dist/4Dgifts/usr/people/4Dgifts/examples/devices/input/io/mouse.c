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
#ident "$Revision: 1.9 $"

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

#include <sys/ioctl.h>
#include <sys/termio.h>
#include <sys/debug.h>

/* Streams stuff */
#include <sys/stream.h>

#ifdef INPUT_TEST
#include <stdio.h>
#define	_KERNEL
#include "inputtest.h"
#endif /* INPUT_TEST */

/* my stuff */
#include <sys/shmiq.h>
#include <sys/idev.h>
#include <sys/mouse.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to MS 
	 * go here
	 */

static idevDesc _ms_desc = {
	MS_NAME,		/* devName */
	MS_TYPE,		/* devType */
	MS_NUM_BUTTONS,		/* nButtons */
	MS_NUM_VALUATORS,	/* nValuators */
	0,			/* nLEDs */
	0,			/* nStrDpys */
	0,			/* nIntDpys */
	0,			/* nBells */
	0			/* flags */
};

static struct termio _ms_termio = {
	IGNBRK|IGNPAR,	/* c_iflag */
	0,		/* c_oflag */
	B4800|CS8|CREAD|HUPCL, /* c_cflag */
	0,			/* c_lflag */
	LDISC1,			/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   5,    1,     0  }
};

static idevValuatorDesc _ms_dflt_val_desc = {
	MS_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	MS_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	MS_VALUATOR_MIN,		/* hwMinVal */
	MS_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_EITHER,			/* possibleModes */
	IDEV_ABSOLUTE,			/* mode */
	MS_VALUATOR_RESOLUTION,		/* resolution */
	MS_VALUATOR_MIN,		/* minVal */
	MS_VALUATOR_MAX			/* maxVal */
};


static char mouseMap[]= {
	7,3,5,1,6,2,4,0
};

/* ARGSUSED */
static void
ms_intr( idevInfo *pInfo, char *str, int len )
{

	ms_state_t *ms = (ms_state_t *)pInfo;
	unsigned char currentButtons ;
	int deltaX,deltaY ;

	while ( len-- ) {
		register c = *str++;
		/*
		 * Restart the state machine whenever
		 * it gets a button byte.  This makes
		 * the mouse work better when
		 * characters get dropped.
		 */
		if ((c&0xF8)==0x80) {
			/* Of course if this is really 
			 * a delta < -120, we're
			 * screwed!
			 */
			 ms->state = MS_BUT_STATE;
		}
		ms->report[ ms->state&MS_STATE_MASK ] = c;
		if ( ++ms->state >= MS_LAST_STATE ) {
			if ( ( ms->report[MS_BUT_STATE] & 0xf8 ) != 0x80 ) {
#ifdef DEBUG
				cmn_err( CE_WARN, "bad mouse report" ) ;
#endif /* DEBUG */
				ms->state = MS_BUT_STATE;
				continue;
			}
			deltaY = -1*(ms->report[MS_DY1_STATE]+
						ms->report[MS_DY2_STATE]);

			deltaX = ms->report[MS_DX1_STATE] + 
						ms->report[MS_DX2_STATE];

			if ((deltaX)||(deltaY)) {
				idevValuatorState	new;
				int			rtrn;
				new.firstValuator = 0;
				new.nValuators = 2;
				new.value[0] =	deltaX;
				new.value[1] =	deltaY;
				rtrn = idevGenValEvents(&ms->info,&new,0);
			}

			currentButtons = ms->report[MS_BUT_STATE] ;
			currentButtons = mouseMap[currentButtons&ALLBUTTONS];

			if ( ms->bstate != currentButtons ) {
				unsigned char mask= ALLBUTTONS;
				idevGenBtnEvents( &ms->info, 
						&mask, &currentButtons );
			}
			ms->state = MS_BUT_STATE;
		}
	}

	return;
}


	/*
	 * MS ioctl routine
	 * Handles MS ioctls, returns TRUE if the ioctl
	 * was known and successful.  Returns FALSE 
	 * otherwise.  Sets *pFound to TRUE if the ioctl
	 * was known, FALSE otherwise.
	 */
static int
ms_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
ms_state_t *ms= (ms_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _ms_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&ms->info,
					(idevGetSetValDesc*)stuff);
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(MS_NUM_BUTTONS)) {
				stuff[0]= ms->bstate;
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&ms->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals	*vals= (idevBitVals *)stuff;
				ok= 1;
				ms->bactive &= ~vals->mask[0];
				ms->bactive |= (vals->mask[0]&vals->value[0]);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals	*vals= (idevBitVals *)stuff;
				ms->vactive &= ~vals->mask[0];
				ms->vactive |= (vals->mask[0]&vals->value[0]);
				ok= 1;
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&ms->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|IDEV_VALS_ABSOLUTE|
					QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&ms->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevSetValDesc(&ms->info,
					(idevGetSetValDesc *)stuff);
			break;
		case IDEVPTRCONTROL:
			found++;
			if (size>=sizeof(idevPtrControl))
				ok = idevSetPtrCtrl(&ms->info,
					(idevPtrControl *)stuff);
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&ms->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&ms->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&ms->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevSetTransform(&ms->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&ms->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVINITDEVICE:
			found++;
			ok= idevChangeLineSettings(&ms->info,&_ms_termio);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
ms_open( queue_t *rq, dev_t *dev, int flag, int sflag, struct cred *cred )
{
	register ms_state_t *ms;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO;

	if ( !rq->q_ptr ) {
		if ( !(ms = (ms_state_t *)kmem_alloc(sizeof *ms, KM_SLEEP)))
			return ENOMEM;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( ms, sizeof *ms ) ;

		ms->initialized = 1;
		for (i=0;i<MS_NUM_VALUATORS;i++) {
			ms->vdesc[i]= _ms_dflt_val_desc;
			ms->vstate[i]= 0;
			ms->vtrans[i] = idevDfltAccel;
			ms->vtrans[i].possible = IDEV_ACCEL;
			ms->vtrans[i].which = 0;
		}
		ms->vactive = 0xffffffff;
		ms->bstate = 0;
		ms->bactive = 0xffffffff;

		ms->info.bInfo.nBtn = MS_NUM_BUTTONS;
		ms->info.bInfo.active = &ms->bactive;
		ms->info.bInfo.state = &ms->bstate;

		ms->info.vInfo.nVal = MS_NUM_VALUATORS;
		ms->info.vInfo.sysValue = ms->vstate;
		ms->info.vInfo.desc = ms->vdesc;
		ms->info.vInfo.transform = ms->vtrans;
		ms->info.vInfo.active = &ms->vactive;
		ms->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;

		ms->info.pInfo.xAxis = ms->info.pInfo.yAxis = 255;
		ms->info.pInfo.minX = ms->info.pInfo.minY = 0;
		ms->info.pInfo.maxX = ms->info.pInfo.maxY = 0;
		ms->info.pInfo.x = ms->info.pInfo.y = 0;
		ms->info.pInfo.xTransform = idevDfltAccel;
		ms->info.pInfo.yTransform = idevDfltAccel;

		ms->info.sInfo.shmiqid.devminor = 0;
		ms->info.sInfo.shmiqid.index = 0;
		ms->info.sInfo.rq = rq;
		ms->info.sInfo.wq =	WR(rq);
		ms->info.sInfo.readData = ms_intr;
		ms->info.sInfo.writeIoctl = ms_wioctl;

		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &ms->info;
	}

	return 0;
}

/* ARGSUSED */
static int
ms_close( queue_t *rq, int flags, struct cred *cred )
{
	ms_state_t *ms = (ms_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	kmem_free( (char *)ms , sizeof *ms);
	return 0;
}

	/*
	 * stream module definition
	 */

static struct module_info ms_mod_info = {
	0,				/* module ID */
	MS_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit ms_rinit = {
	idev_rput, NULL, ms_open, ms_close,
	NULL, &ms_mod_info, NULL
};

static struct qinit ms_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &ms_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab mouseinfo = {
	&ms_rinit, &ms_winit, 0, 0
} ;

int mousedevflag = 0;
