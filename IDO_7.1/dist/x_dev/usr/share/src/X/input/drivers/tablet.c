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
#ident "$Revision: 1.12 $"

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
#include <string.h>

/* Streams stuff */
#include <sys/stream.h>

#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h"
#define _KERNEL
#endif /* INPUT_TEST */

/* my stuff */
#include <sys/shmiq.h>
#include <sys/idev.h>
#include <sys/tablet.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to TABLET 
	 * go here
	 */

static idevDesc _tbt_desc = {
	TABLET_NAME,		/* devName */
	TABLET_TYPE,		/* devType */
	TABLET_NUM_BUTTONS,	/* nButtons */
	TABLET_NUM_VALUATORS,	/* nValuators */
	0,			/* nLEDs */
	0,			/* nStrDpys */
	0,			/* nIntDpys */
	0,			/* nBells */
	0			/* flags */
};

static idevValuatorDesc _tbt_dflt_val_desc = {
	TABLET_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	TABLET_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	TABLET_VALUATOR_MIN,		/* hwMinVal */
	TABLET_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_ABSOLUTE,			/* possibleModes */
	IDEV_ABSOLUTE,			/* mode */
	TABLET_VALUATOR_RESOLUTION,	/* resolution */
	TABLET_VALUATOR_MIN,		/* minVal */
	TABLET_VALUATOR_MAX		/* maxVal */
};

static int tabletref = 0;		/* reference count for unload */
char *tabletmversion = M_VERSION;
int tabletdevflag = 0;

/* ARGSUSED */
static int
tbt_other_control(tbt_state_t *tbt, idevOtherControl *pCtrl)
{

	/* implement other device controls here */
	pCtrl->name[IDEV_CTRL_NAME_LEN]= '\0';
	pCtrl->data[IDEV_CTRL_DATA_LEN]= '\0';
	if (strcmp(pCtrl->name,"invert")==0) {
	    char *str= pCtrl->data;
	    int	on= 1;
	    idevTransform *pTrans;

	    while (*str) {
		pTrans= NULL;
		if (*str=='!')		on= 0;
		else if (*str=='x') 	pTrans= &tbt->info.pInfo.xTransform;
		else if (*str=='y') 	pTrans= &tbt->info.pInfo.yTransform;
		else if (*str=='0') 	pTrans= &tbt->info.vInfo.transform[0];
		else if (*str=='1') 	pTrans= &tbt->info.vInfo.transform[1];
		if (pTrans) {
			if (on)	pTrans->flags|= IDEV_INVERT;
			else	pTrans->flags&= (~IDEV_INVERT);
		}
		str++;
	    }
	    return 1;
	}
	return 0;
}

static void
tbt_intr( idevInfo *pInfo, unsigned char *str, int len )
{
tbt_state_t *tbt= (tbt_state_t *)pInfo;

    while (len--) {
	switch (tbt->state++) {
	    case TABLET_NOT_SYNCHED:
		if (!(*str++ & TABLET_BUTTON_FLAG)) {
		    tbt->state=	TABLET_NOT_SYNCHED;
		}
		break;
	    case TABLET_LOW_X:
		tbt->vals.value[0]= (*str++)&0x3f;
		break;
	    case TABLET_HIGH_X:
		tbt->vals.value[0]|= ((*str++)&0x3f)<<6;
		break;
	    case TABLET_LOW_Y:
		tbt->vals.value[1]= (*str++)&0x3f;
		break;
	    case TABLET_HIGH_Y:
		tbt->vals.value[1]|= ((*str++)&0x3f)<<6;
		break;
	    case TABLET_BUTTONS:
		if (!(*str&TABLET_BUTTON_FLAG)) {
			tbt->state= TABLET_NOT_SYNCHED;
/*			cmn_err(CE_DEBUG,"tablet: out of sync\n"); */
			break;
		}
		idevGenValEvents(&tbt->info,&tbt->vals,IDEV_VALS_ABSOLUTE);

		{
			unsigned char mask= 0xf;
			unsigned char newBtns=	((*str++)&0x3f)>>2;

			if ((newBtns&0xf)!=(tbt->bstate&0xf)) 
		    		idevGenBtnEvents(&tbt->info,&mask,&newBtns);
		}
		tbt->state=	TABLET_LOW_X;
		break;
	    default:
		cmn_err(CE_WARN,"tablet: Illegal state %d!!\n",tbt->state);
		tbt->state=	TABLET_NOT_SYNCHED;
		break;
	}
    }
    return;
}

static int
tbt_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
tbt_state_t *tbt= (tbt_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _tbt_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&tbt->info,
					(idevGetSetValDesc*)stuff);
			break;
		case IDEVPTRCONTROL:
			found++;
			if (size>=sizeof(idevPtrControl))
				ok = idevSetPtrCtrl(&tbt->info,
					(idevPtrControl *)stuff);
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(TABLET_NUM_BUTTONS)) {
				stuff[0]= tbt->bstate;
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&tbt->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals *vals= (idevBitVals *)stuff;
				ok= 1;
				tbt->bactive &= ~vals->mask[0];
				tbt->bactive |= (vals->mask[0]&vals->value[0]);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				idevBitVals *vals= (idevBitVals *)stuff;
				ok= 1;
				tbt->vactive &= ~vals->mask[0];
				tbt->vactive |= (vals->mask[0]&vals->value[0]);
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&tbt->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|IDEV_VALS_ABSOLUTE
					|QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&tbt->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc)) {
				ok = idevSetValDesc(&tbt->info,
					(idevGetSetValDesc *)stuff);
			}
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&tbt->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&tbt->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&tbt->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform)) {
				ok = idevSetTransform(&tbt->info,
					(idevGetSetTransform *)stuff);
			}
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&tbt->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVOTHERCONTROL:
			found++;
			if (size==sizeof(idevOtherControl)) {
				ok = tbt_other_control(tbt,
					(idevOtherControl *)stuff);
			}
			else ok = 0;
			break;
		case IDEVINITDEVICE:
			found++;
			ok = idevChangeLineSettings(&tbt->info,NULL);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
tbt_open( queue_t *rq, dev_t *dev, int flag, int sflag, struct cred *cred )
{
	register tbt_state_t *tbt;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO;

	if ( !rq->q_ptr ) {
		if ( !(tbt = (tbt_state_t *)kmem_alloc(sizeof *tbt, KM_SLEEP)))
			return ENOMEM;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( tbt, sizeof *tbt ) ;

		tbt->initialized = 1;
		tbt->state = 0;
		tbt->vals.firstValuator = 0;
		tbt->vals.nValuators = 2;
		for (i=0;i<TABLET_NUM_VALUATORS;i++) {
			tbt->vdesc[i]= _tbt_dflt_val_desc;
			tbt->vstate[i]= 0;
			tbt->vtrans[i] = idevDfltScale;
			tbt->vtrans[i].possible = IDEV_SCALE;
			tbt->vtrans[i].which = IDEV_SCALE;
		}
		tbt->vactive=	TABLET_ALL_VALUATORS;
		tbt->bstate=	0;
		tbt->bactive=	TABLET_ALL_BUTTONS;

		tbt->info.bInfo.nBtn = TABLET_NUM_BUTTONS;
		tbt->info.bInfo.active = &tbt->bactive;
		tbt->info.bInfo.state = &tbt->bstate;

		tbt->info.vInfo.nVal = TABLET_NUM_VALUATORS;
		tbt->info.vInfo.sysValue = tbt->vstate;
		tbt->info.vInfo.desc = tbt->vdesc;
		tbt->info.vInfo.transform = tbt->vtrans;
		tbt->info.vInfo.active = &tbt->vactive;
		tbt->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;
		tbt->info.pInfo.xTransform = idevDfltScale;
		tbt->info.pInfo.yTransform = idevDfltScale;
		tbt->info.pInfo.yTransform.flags= IDEV_INVERT;

		tbt->info.pInfo.xAxis = tbt->info.pInfo.yAxis = 255;
		tbt->info.pInfo.minX = tbt->info.pInfo.minY = 0;
		tbt->info.pInfo.maxX = tbt->info.pInfo.maxY = 0;
		tbt->info.pInfo.x= tbt->info.pInfo.y= 0;

		tbt->info.sInfo.shmiqid.devminor = 0;
		tbt->info.sInfo.shmiqid.index = 0;
		tbt->info.sInfo.rq = rq;
		tbt->info.sInfo.wq =	WR(rq);
		tbt->info.sInfo.readData = (void (*)())tbt_intr;
		tbt->info.sInfo.writeIoctl = tbt_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &tbt->info;
	}

	tabletref++;

	return 0;
}

/* ARGSUSED */
static int
tbt_close( queue_t *rq, int flag, struct cred *cred )
{
	tbt_state_t *tbt = (tbt_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	kmem_free( (char *)tbt , sizeof *tbt);
	tabletref--;
	return 0;
}

int
tabletunload(void)
{
	return(tabletref ? EBUSY : 0);
}

	/*
	 * stream module definition
	 */

static struct module_info tbt_mod_info = {
	0,				/* module ID */
	TABLET_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit tbt_rinit = {
	idev_rput, NULL, tbt_open, tbt_close,
	NULL, &tbt_mod_info, NULL
};

static struct qinit tbt_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &tbt_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab tabletinfo = {
	&tbt_rinit, &tbt_winit, 0, 0
} ;
