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
#ident "$Revision: 1.8 $"

#include <bstring.h>
#ifdef INPUT_TEST
#define _KERNEL
#include <sys/types.h>
#undef _KERNEL
#else
#include <sys/types.h>
#endif
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/termio.h>
#include <sys/cmn_err.h>
#include <sys/debug.h>
#include <sys/param.h>

/* Streams stuff */
#include <sys/stream.h>
#include <sys/strids.h>
#include <sys/stropts.h>

#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h>
#define	_KERNEL
#else
#include <sys/systm.h>
#endif /* INPUT_TEST */

/* my stuff */
#include <sys/shmiq.h>
#include <sys/idev.h>
#include <sys/XXX.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to XXX 
	 * go here
	 */

static idevDesc _xxx_desc = {
	XXX_NAME,		/* devName */
	XXX_TYPE,		/* devType */
	XXX_NUM_BUTTONS,	/* nButtons */
	XXX_NUM_VALUATORS,	/* nValuators */
	XXX_NUM_LEDS,		/* nLEDs */
	XXX_NUM_STR_DPYS,	/* nStrDpys */
	XXX_NUM_INT_DPYS,	/* nIntDpys */
	XXX_NUM_BELLS,		/* nBells */
	XXX_FLAGS		/* flags */
};

static struct termio _xxx_termio = {
	IGNBRK|IGNPAR,	/* c_iflag */
	0,		/* c_oflag */
	B9600|CS8|CREAD|HUPCL, /* c_cflag */
	0,			/* c_lflag */
	LDISC1,			/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   5,    1,     0  }
};

static idevValuatorDesc _xxx_dflt_val_desc = {
	XXX_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	XXX_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	XXX_VALUATOR_MIN,		/* hwMinVal */
	XXX_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_EITHER,			/* possibleModes */
	IDEV_ABSOLUTE,			/* mode */
	XXX_VALUATOR_RESOLUTION,	/* resolution */
	XXX_VALUATOR_MIN,		/* minVal */
	XXX_VALUATOR_MAX		/* maxVal */
};

static idevStrDpyDesc _xxx_sdpy = {
	0,
	XXX_STR_DPY_MAX_LEN,
	XXX_STR_DPY_ALL_SAME,
	XXX_STR_DPY_SYMBOL_SET,
};

static idevIntDpyDesc _xxx_idpy = {
	0,
	XXX_INT_DPY_ALL_SAME,
	0,
	XXX_INT_DPY_MIN,
	XXX_INT_DPY_MAX,
	XXX_INT_DPY_RESOLUTION
};

	/*
	 * get_str_dpy_state:
	 *	- returns the state of a string display
	 */

/* ARGSUSED */
static int
xxx_get_str_dpy_state(xxx_state_t *xxx, idevStrDpyState *dpy)
{
	if (dpy->nDpy<XXX_NUM_STR_DPYS) {
		bcopy(xxx->sdpystate[dpy->nDpy], dpy->string,
			XXX_STR_DPY_MAX_LEN);
		dpy->string[XXX_STR_DPY_MAX_LEN]= '\0';
		if (idevNth(xxx->sdpyon, dpy->nDpy))
			dpy->on = 1;
		else
			dpy->on = 0;
		return 1;
	}
	return 0;
}

	/*
	 * get_int_dpy_state:
	 *	- returns the state of one or more device
	 *	  integer displays.
	 */

/* ARGSUSED */
static int
xxx_get_int_dpy_state(xxx_state_t *xxx, register idevIntDpyState *dpys)
{
	if ((dpys->firstDpy+dpys->nDpys<XXX_NUM_INT_DPYS)
		 &&(dpys->nDpys<=IDEV_INT_DPY_MAX)) {
		register int i;
		bcopy(&xxx->idpystate[dpys->firstDpy], dpys->value,
			2*dpys->nDpys);
		for (i=0; i<dpys->nDpys; i++) {
			if (idevNth(xxx->idpyon, dpys->firstDpy+i))
				dpys->on |= (1<<i);
			else
				dpys->on &= ~(1<<i);
	}
	return 1;
    }
    return 0;
}

	/*
	 * set_str_dpy
	 *	- sets a string display
	 */

/* ARGSUSED */
static int
xxx_set_str_dpy(xxx_state_t *xxx, register idevStrDpyState *dpy)
{
	if (dpy->nDpy<XXX_NUM_STR_DPYS) {
		int len;
		register int i;
		register char *str = xxx->sdpystate[dpy->nDpy];

		len = strlen(dpy->string);
		for (i=0; i<len; i++) {
			str[i]= dpy->string[i];
		}
		for (;i< XXX_STR_DPY_MAX_LEN; i++) {
			str[i]= ' ';
		}
		str[XXX_STR_DPY_MAX_LEN] = '\0';
		if (dpy->on)
			idevSetNth(xxx->sdpyon,dpy->nDpy);
		else 
			idevClearNth(xxx->sdpyon,dpy->nDpy);
		XXX_TellDevice(IDEVSETSTRDPY,dpy);
		return 1;
	}
	return 0;
}

	/*
	 * set_int_dpy
	 *	- sets an integer display
	 */

/* ARGSUSED */
static int
xxx_set_int_dpy(xxx_state_t *xxx, register idevIntDpyState *dpy)
{
	if ((dpy->firstDpy+dpy->nDpys<XXX_NUM_INT_DPYS)
		 &&(dpy->nDpys<=IDEV_INT_DPY_MAX)) {
		register int i,mask;
		for (mask=1, i=0; i<dpy->nDpys; i++, (mask<<=1)) {
			xxx->idpystate[dpy->firstDpy+i] = dpy->value[i];
			if (dpy->on&mask)
				idevSetNth(xxx->idpyon, dpy->firstDpy+i);
			else	
				idevClearNth(xxx->idpyon, dpy->firstDpy+i);
		}
		XXX_TellDevice(IDEVSETINTDPYS,dpy);
		return 1;
	}
	return 0 ;
}

/* ARGSUSED */
static int
xxx_ring_bell(xxx_state_t *xxx, register idevBellSettings *bell)
{
#if XXX_HAS_BELL
	XXX_TellDevice(IDEVRINGBELL, bell);
	return 1;
#else
	return 0;
#endif
}

/* ARGSUSED */
static int
xxx_keybd_control(xxx_state_t *xxx, idevKeybdControl *pCtrl)
{
	/* implement keyboard device controls here */
	return 0;
}

/* ARGSUSED */
static int
xxx_ptr_control(xxx_state_t *xxx, idevPtrControl *pCtrl)
{
	/* implement pointer device controls here */
	return 0;
}

/* ARGSUSED */
static int
xxx_other_control(xxx_state_t *xxx, idevOtherControl *pCtrl)
{

	/* implement other device controls here */
	return 0;
}

/* ARGSUSED */
static int
xxx_other_query(xxx_state_t *xxx, idevOtherControl *pCtrl)
{

	/* implement other device queries here */
	return 0;
}

#ifdef OTHER_INIT
/* ARGSUSED */
static int
xxx_init_device(xxx_state_t *xxx, int size, char *stuff)
{
	if (idevChangeLineSettings(&xxx->info,&_xxx_termio)) {
		cmn_err(CE_WARN,"XXX not initialized.\n");
		return 1;
	}
	return 0;
}
#endif /* OTHER_INIT */


	/*
	 * XXX interrupt routine
	 *
	 *	Called from duart interrupt.
	 *	Should parse the byte stream from from the device and
	 *	call idevGenValEvents or idevGenBtnEvent
	 *	when it receives full events.
	 */
/* ARGSUSED */
static void
xxx_intr( idevInfo *pInfo, unsigned char *str, int len )
{
	return;
}


	/*
	 * XXX ioctl routine
	 * Handles XXX ioctls, returns TRUE if the ioctl
	 * was known and successful.  Returns FALSE 
	 * otherwise.  Sets *pFound to TRUE if the ioctl
	 * was known, FALSE otherwise.
	 * Note that 'stuff' can be NULL if size is 0, so
	 * never dereference stuff without first checking
	 * size.
	 */
static int
xxx_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
xxx_state_t *xxx= (xxx_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _xxx_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&xxx->info,
					(idevGetSetValDesc*)stuff);
			break;
#if XXX_HAS_KEYMAP != 0
		case IDEVGETKEYMAPDESC:
			found++;
			if (size>=sizeof(idevKeymapDesc)) {
				strncpy(((idevKeymapDesc *)stuff)->name,
							XXX_KEYMAP_NAME,
							IDEV_KEYMAP_NAME_LEN);
				ok= 1;
			}
			break;
#endif
		case IDEVGETSTRDPYDESC:
			found++;
			if (size>=sizeof(idevStrDpyDesc)) {
				idevStrDpyDesc *pDpy= (idevStrDpyDesc *)stuff;
				if (pDpy->dpyNum<XXX_NUM_STR_DPYS) {
					int num= pDpy->dpyNum;
					*pDpy= _xxx_sdpy;
					pDpy->dpyNum= num;
					ok = 1;
				}
			}
			break;
		case IDEVGETINTDPYDESC:
			found++;
			if (size>=sizeof(idevIntDpyDesc)) {
				idevIntDpyDesc *pDpy= (idevIntDpyDesc *)stuff;
				if (pDpy->dpyNum<XXX_NUM_INT_DPYS) {
					int num= pDpy->dpyNum;
					*pDpy= _xxx_idpy;
					pDpy->dpyNum= num;
					ok = 1;
				}
			}
			break;
#if XXX_HAS_BELL
		case IDEVGETNUMBELLS:
			found++;
			if (size>=sizeof(unsigned long)) {
			    *((unsigned long *)stuff)= XXX_NUM_BELLS;
			}
			break;
#endif
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(XXX_NUM_BUTTONS)) {
				bcopy(xxx->bstate,stuff,
						idevSize(XXX_NUM_BUTTONS));
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&xxx->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVGETLEDS:
			found++;
			if (size>=idevSize(XXX_NUM_LEDS)) {
				bcopy(xxx->ledstate,stuff,
						idevSize(XXX_NUM_LEDS));
				ok = 1;
			}
			break;
		case IDEVGETSTRDPY:
			found++;
			if (size>=sizeof(idevStrDpyState))
				ok = xxx_get_str_dpy_state(xxx,
					(idevStrDpyState*)stuff);
			break;
		case IDEVGETINTDPYS:
			found++;
			if (size>=sizeof(idevIntDpyState))
				ok = xxx_get_int_dpy_state(xxx,
					(idevIntDpyState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= 1;
				idevUpdateBitArray(idevSize(XXX_NUM_BUTTONS),
							xxx->bactive,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= 1;
				idevUpdateBitArray(idevSize(XXX_NUM_VALUATORS),
							xxx->vactive,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&xxx->info,
					(idevValuatorState *)stuff,
					IDEV_VALS_ABSOLUTE|QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&xxx->info,
					(idevValuatorState *)stuff,QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevSetValDesc(&xxx->info,
					(idevGetSetValDesc *)stuff);
			break;
#if XXX_NUM_LEDS > 0
		case IDEVSETLEDS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevUpdateBitArray(idevSize(XXX_NUM_LEDS),
							xxx->ledstate,
							(idevBitVals *)stuff);
				ok= XXX_TellDevice(IDEVSETLEDS);
			}
			break;
#endif
		case IDEVSETSTRDPY:
			found++;
			if (size>=sizeof(idevStrDpyState))
				ok = xxx_set_str_dpy(xxx,
					(idevStrDpyState *)stuff);
			break;
		case IDEVSETINTDPYS:
			found++;
			if (size>=sizeof(idevIntDpyState))
				ok = xxx_set_int_dpy(xxx,
					(idevIntDpyState *)stuff);
			break;
		case IDEVRINGBELL:
			found++;
			if (size>=sizeof(idevBellSettings))
				ok = xxx_ring_bell(xxx,
					(idevBellSettings *)stuff);
			break;
		case IDEVKEYBDCONTROL:
			found++;
			if (size>=sizeof(idevKeybdControl))
				ok = xxx_keybd_control(xxx,
					(idevKeybdControl *)stuff);
			break;
		case IDEVPTRCONTROL:
			found++;
			if (size>=sizeof(idevPtrControl))
				ok = xxx_ptr_control(xxx,
					(idevPtrControl *)stuff);
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&xxx->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&xxx->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&xxx->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevSetTransform(&xxx->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&xxx->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVOTHERQUERY:
			found++;
			if (size==sizeof(idevOtherQuery)) {
				ok = xxx_other_query(xxx,
					(idevOtherQuery *)stuff);
			}
			else ok = 0;
			break;
		case IDEVOTHERCONTROL:
			found++;
			if (size==sizeof(idevOtherControl)) {
				ok = xxx_other_control(xxx,
					(idevOtherControl *)stuff);
			}
			else ok = 0;
			break;
		case IDEVINITDEVICE:
			found++;
#ifdef OTHER_INIT
			ok = xxx_init_device(xxx,size,stuff);
#else
			ok = idevChangeLineSettings(&xxx->info,&_xxx_termio);
#endif
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
xxx_open( register queue_t *rq, dev_t *dev, int flag, int sflag,
	  struct cred *crp )
{
	register xxx_state_t *xxx;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO ;

	if ( !rq->q_ptr ) {
		if ( !(xxx = (xxx_state_t *)malloc(sizeof *xxx)))
			return ENOMEM ;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( xxx, sizeof *xxx ) ;

		xxx->initialized = 1;
		xxx->errorCount = 0;
		for (i=0;i<XXX_NUM_VALUATORS;i++) {
			xxx->vdesc[i] = _xxx_dflt_val_desc;
			xxx->vstate[i] = 0;
			xxx->vtrans[i] = idevDfltAccel;
			xxx->vtrans[i].possible = IDEV_ACCEL|IDEV_SCALE;
			xxx->vtrans[i].which = 0;
		}
		idevSetAll(xxx->vactive);
		idevClearAll(xxx->bstate);
		idevSetAll(xxx->bactive);
		idevClearAll(xxx->ledstate);
		xxx->info.bInfo.nBtn = XXX_NUM_BUTTONS;
		xxx->info.bInfo.active = xxx->bactive;
		xxx->info.bInfo.state = xxx->bstate;

		xxx->info.vInfo.nVal = XXX_NUM_VALUATORS;
		xxx->info.vInfo.sysValue = xxx->vstate;
		xxx->info.vInfo.desc = xxx->vdesc;
		xxx->info.vInfo.transform = xxx->vtrans;
		xxx->info.vInfo.active = xxx->vactive;
		xxx->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;

		xxx->info.pInfo.xAxis = xxx->info.pInfo.yAxis = 255;
		xxx->info.pInfo.minX = xxx->info.pInfo.minY= 0;
		xxx->info.pInfo.maxX = xxx->info.pInfo.maxY = 0;
		xxx->info.pInfo.x= xxx->info.pInfo.y= 0;
		xxx->info.pInfo.xTransform = idevDfltAccel;
		xxx->info.pInfo.yTransform = idevDfltAccel;

		for (i=0;i<XXX_NUM_STR_DPYS;i++) {
			memset(xxx->sdpystate[i], ' ', XXX_STR_DPY_MAX_LEN);
			xxx->sdpystate[i][XXX_STR_DPY_MAX_LEN]= '\0';
		}
		idevClearAll(xxx->sdpyon);
		for (i=0;i<XXX_NUM_INT_DPYS;i++) {
			xxx->idpystate[i] = 0;
		}
		idevClearAll(xxx->idpyon);

		xxx->info.sInfo.shmiqid.devminor = 0;
		xxx->info.sInfo.shmiqid.index = 0;
		xxx->info.sInfo.rq = rq;
		xxx->info.sInfo.wq =	WR(rq);
		xxx->info.sInfo.readData = xxx_intr;
		xxx->info.sInfo.writeIoctl = xxx_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &xxx->info;
	}

	return 0;
}

/* ARGSUSED */
static int
xxx_close( register queue_t *rq, int flag, struct cred *crp )
{
	xxx_state_t *xxx = (xxx_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	XXX_TellDevice(~MODOPEN, (xxx_state_t *)xxx);
	free( (char *)xxx );
	return 0;
}

	/*
	 * stream module definition
	 */

static struct module_info xxx_mod_info = {
	0,				/* module ID */
	XXX_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit xxx_rinit = {
	idev_rput, NULL, xxx_open, xxx_close,
	NULL, &xxx_mod_info, NULL
};

static struct qinit xxx_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &xxx_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab xxxinfo = {
	&xxx_rinit, &xxx_winit, 0, 0
} ;

int xxxdevflag = 0;
