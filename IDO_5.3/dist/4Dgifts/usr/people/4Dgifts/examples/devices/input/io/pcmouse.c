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
#ident "$Revision: 1.3 $"

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
#include "inputtest.h"
#define	_KERNEL
#endif /* INPUT_TEST */

/* my stuff */
#include <sys/shmiq.h>
#include <sys/idev.h>
#include <sys/pcmouse.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to PCMOUSE 
	 * go here
	 */

static idevDesc _pcmouse_desc = {
	PCMOUSE_NAME,		/* devName */
	PCMOUSE_TYPE,		/* devType */
	PCMOUSE_NUM_BUTTONS,	/* nButtons */
	PCMOUSE_NUM_VALUATORS,	/* nValuators */
	PCMOUSE_NUM_LEDS,	/* nLEDs */
	PCMOUSE_NUM_STR_DPYS,	/* nStrDpys */
	PCMOUSE_NUM_INT_DPYS,	/* nIntDpys */
	PCMOUSE_NUM_BELLS,	/* nBells */
	PCMOUSE_FLAGS		/* flags */
};

static struct termio _pcmouse_termio = {
	IGNBRK|IGNPAR,	/* c_iflag */
	0,		/* c_oflag */
	B1200|CS8|CREAD|HUPCL, /* c_cflag */
	0,		/* c_lflag */
	0,		/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   5,    1,     0  }
};

static idevValuatorDesc _pcmouse_dflt_val_desc = {
	PCMOUSE_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	PCMOUSE_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	PCMOUSE_VALUATOR_MIN,		/* hwMinVal */
	PCMOUSE_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_EITHER,			/* possibleModes */
	IDEV_EITHER,			/* mode */
	PCMOUSE_VALUATOR_RESOLUTION,	/* resolution */
	PCMOUSE_VALUATOR_MIN,		/* minVal */
	PCMOUSE_VALUATOR_MAX		/* maxVal */
};

static char pcmouse_btn_cvt[]=	{ 0,1,4,5,2,3,6,7 };

static int pcmouse_osync;		/* track out of sync events */
int mousedevflag = 0;

/* ARGSUSED */
static void
pcmouse_intr( idevInfo *pInfo, unsigned char *str, int len )
{
	pcmouse_state_t *pcmouse= (pcmouse_state_t *)pInfo;
	register signed char ch;

	while (len--) {
		ch = *str++;

		if (pcmouse->dropsync) {
			pcmouse->dropsync--;
			continue;
		}

		switch (pcmouse->state++) {
			case PCMOUSE_BUTTONS:
				pcmouse->flags= ch;
				break;
			case PCMOUSE_X:
				pcmouse->deltaX= ch;
				break;
			case PCMOUSE_Y:
				pcmouse->deltaY= -ch;

				/* make sure sync byte sign bits match */
				if ((pcmouse->flags&0x30) !=
				    (((pcmouse->deltaX&0x80) >> 3) |
				    ((ch&0x80) >> 2))) {
					pcmouse_osync++;
					pcmouse->state = PCMOUSE_BUTTONS;
					pcmouse->dropsync = 2;
					continue;
				}

				if ((pcmouse->deltaX)||(pcmouse->deltaY)) {
					idevValuatorState	new;
					int			rtrn;
					new.firstValuator = 0;
					new.nValuators = 2;
					new.value[0] =	pcmouse->deltaX;
					new.value[1] =	pcmouse->deltaY;
					idevGenValEvents(pInfo,&new,0);
				}
				pcmouse->flags=
					pcmouse_btn_cvt[pcmouse->flags&0x7];
				if (pcmouse->flags!=pcmouse->bstate) {
					unsigned char mask= 0x07;
					idevGenBtnEvents( &pcmouse->info,
						&mask, &pcmouse->flags );
				}
				pcmouse->state= PCMOUSE_BUTTONS;
				break;
		}
	}
	return;
}


static int
pcmouse_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
pcmouse_state_t *pcmouse= (pcmouse_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _pcmouse_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&pcmouse->info,
					(idevGetSetValDesc*)stuff);
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(PCMOUSE_NUM_BUTTONS)) {
				stuff[0]= pcmouse->bstate;
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&pcmouse->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals	*vals= (idevBitVals *)stuff;
				ok= 1;
				pcmouse->bactive &= ~vals->mask[0];
				pcmouse->bactive |= (vals->mask[0]&vals->value[0]);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals	*vals= (idevBitVals *)stuff;
				pcmouse->vactive &= ~vals->mask[0];
				pcmouse->vactive |= (vals->mask[0]&vals->value[0]);
				ok= 1;
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&pcmouse->info,
					(idevValuatorState *)stuff,
					IDEV_VALS_ABSOLUTE|QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&pcmouse->info,
					(idevValuatorState *)stuff,QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevSetValDesc(&pcmouse->info,
					(idevGetSetValDesc *)stuff);
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&pcmouse->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&pcmouse->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&pcmouse->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevSetTransform(&pcmouse->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&pcmouse->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVINITDEVICE:
			found++;
			ok = idevChangeLineSettings(&pcmouse->info,&_pcmouse_termio);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
pcmouse_open( register queue_t *rq, dev_t *devp, int flag, int sflag, struct cred *crp )
{
	register pcmouse_state_t *pcmouse;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO;

	if ( !rq->q_ptr ) {
		if ( !(pcmouse = (pcmouse_state_t *)kmem_alloc(sizeof *pcmouse, KM_SLEEP)))
			return ENOMEM;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( pcmouse, sizeof *pcmouse ) ;

		pcmouse->dropsync = 0;
		for (i=0;i<PCMOUSE_NUM_VALUATORS;i++) {
			pcmouse->vdesc[i] = _pcmouse_dflt_val_desc;
			pcmouse->vstate[i] = 0;
			pcmouse->vtrans[i] = idevDfltAccel;
			pcmouse->vtrans[i].possible = IDEV_ACCEL;
			pcmouse->vtrans[i].which = 0;
		}
		pcmouse->vactive= 0x03;
		pcmouse->bstate=  0x00;
		pcmouse->bactive= 0x07;

		pcmouse->info.bInfo.nBtn = PCMOUSE_NUM_BUTTONS;
		pcmouse->info.bInfo.active = &pcmouse->bactive;
		pcmouse->info.bInfo.state = &pcmouse->bstate;

		pcmouse->info.vInfo.nVal = PCMOUSE_NUM_VALUATORS;
		pcmouse->info.vInfo.sysValue = pcmouse->vstate;
		pcmouse->info.vInfo.desc = pcmouse->vdesc;
		pcmouse->info.vInfo.transform = pcmouse->vtrans;
		pcmouse->info.vInfo.active = &pcmouse->vactive;
		pcmouse->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;

		pcmouse->info.pInfo.xAxis = pcmouse->info.pInfo.yAxis = 255;
		pcmouse->info.pInfo.minX = pcmouse->info.pInfo.minY= 0;
		pcmouse->info.pInfo.maxX = pcmouse->info.pInfo.maxY = 0;
		pcmouse->info.pInfo.x= pcmouse->info.pInfo.y= 0;
		pcmouse->info.pInfo.xTransform = idevDfltAccel;
		pcmouse->info.pInfo.yTransform = idevDfltAccel;

		pcmouse->info.sInfo.shmiqid.devminor = 0;
		pcmouse->info.sInfo.shmiqid.index = 0;
		pcmouse->info.sInfo.rq = rq;
		pcmouse->info.sInfo.wq =	WR(rq);
		pcmouse->info.sInfo.readData = pcmouse_intr;
		pcmouse->info.sInfo.writeIoctl = pcmouse_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &pcmouse->info;
	}

	return 0;
}

/* ARGSUSED */
static int
pcmouse_close( register queue_t *rq, int flag, struct cred *crp )
{
	pcmouse_state_t *pcmouse = (pcmouse_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	kmem_free( (char *)pcmouse , sizeof *pcmouse);
	return 0;
}

	/*
	 * stream module definition
	 */

static struct module_info pcmouse_mod_info = {
	0,				/* module ID */
	PCMOUSE_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit pcmouse_rinit = {
	idev_rput, NULL, pcmouse_open, pcmouse_close,
	NULL, &pcmouse_mod_info, NULL
};

static struct qinit pcmouse_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &pcmouse_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab mouseinfo = {
	&pcmouse_rinit, &pcmouse_winit, 0, 0
} ;
