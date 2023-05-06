
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
#ident "$Revision: 1.10 $"

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
#include <sys/stropts.h>

/* input device stuff */
#ifdef INPUT_TEST
#define	_KERNEL
#include "inputtest.h"
#endif
#include <sys/shmiq.h>
#include <sys/idev.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

/***====================================================================***/

/*
 * Implementation of "mousewarp" plus.
 *	Solve the curve :
 *		delta * ( delta - inflection ) * ( MAXRAWDELTA - delta )
 *	where the result is negative below the inflection point and
 *	positive above it.
 *
 *	cookedDelta = CONST1 * rawDelta + CONST2 * curveDelta
 *
 *	If greater than the threshold: Readjust
 *	    cookedDelta +=
 *		( ( cookedDelta - threshold ) * acceleratorN ) / acceleratorD
 *
 */

#define MAXRAWDELTA 254 /* abs. value of the sum of two signed char's */

/* delta must be >= -254 <= 254 !! */
static int
adjustDelta( idevTransform *pAccel, int delta, int applyCurve )
{
	register int sum ;
	register int sign ;
	register int accelerated ;

	sign = 0 ;
	if ( delta < 0 ) {
		sign = 1 ;
		delta = - delta ;
	}

	if ( applyCurve ) {
		sum = ( pAccel->m1 * delta ) << 4 ;
		if ( pAccel->cM )
			sum += ( (int)  ( pAccel->cM * (int) delta
				* ( (int) delta - pAccel->inflection )
				* ( MAXRAWDELTA - (int) delta ) ) ) >> 12 ;
	}
	else sum = (delta << 8);

	/* here sum is represented as fixed-point 8.8 */
	/* We keep the curve continuous by applying the acceleration
	 * to only the part of delta above the threshold
	 */
	if ( pAccel->numerator
	  && ( accelerated = sum - ( pAccel->threshold << 8 ) ) > 0 ) {
		accelerated *= pAccel->numerator;
		accelerated /= pAccel->denominator ;
		sum = (pAccel->threshold<<8)+accelerated ;
	}
	sum= (sum+128)>>8;
	return sign ? -sum : sum ; /* New Delta */
}

/***====================================================================***/

static int
idevTransformCoord(idevTransform *pTrans, int pos, int last, 
				int minVal, int maxVal, unsigned flags)
{
	if ((pTrans->which&IDEV_ACCEL)&&(!(flags&IDEV_NO_TRANSFORM))) {
		if (flags&IDEV_VALS_ABSOLUTE)
			pos-= last;
		pos= adjustDelta( pTrans, pos, (pTrans->which&IDEV_CURVE) );
		if (pTrans->flags&IDEV_INVERT)
			pos*= -1;
		pos+= last;
	}
	else {
		if (!(flags&IDEV_VALS_ABSOLUTE))
			pos+= last;
		if ((pTrans->which&IDEV_SCALE)&&(!(flags&IDEV_NO_TRANSFORM))) {
			pos*= pTrans->numerator;
			pos/= pTrans->denominator;
			if (pTrans->flags&IDEV_INVERT)
				pos= (maxVal-minVal)-(pos-minVal)+minVal;
		}
	}
	return pos;
}

/***====================================================================***/

int
idevGenPtrEvent(idevInfo		*pInfo,
		 idevValuatorState 	*vNew,
		 unsigned 		 flags)
{
int	ptrFlags= 0;
int	pos;
register idevPtrInfo	*ptrInfo= &pInfo->pInfo;

    if (idevValInState(vNew,ptrInfo->xAxis)
					&&(pInfo->vInfo.mode&IDEV_GEN_PTR_X)) {
	pos= vNew->value[ptrInfo->xAxis-vNew->firstValuator];
	pos= idevTransformCoord(&ptrInfo->xTransform,pos,ptrInfo->x,
					ptrInfo->minX,ptrInfo->maxX,flags);
	/* at this point, pos must be an absolute coordinate */
	if (pos<ptrInfo->minX) {
	    pos= ptrInfo->minX;
	    ptrFlags|= QE_X_CLAMPED;
	}
	else if (pos>ptrInfo->maxX) {
	    pos= ptrInfo->maxX;
	    ptrFlags|= QE_X_CLAMPED;
	}
	if ((pos!=ptrInfo->x)||(flags&(QE_RESPONSE|IDEV_FORCE_EVENT))||
				(ptrFlags&QE_X_CLAMPED)) {
	   ptrInfo->x= pos;
	   ptrFlags|= IDEV_GEN_PTR_X;
	}
	else ptrFlags&= (~QE_X_CLAMPED);
    }
    if (idevValInState(vNew,ptrInfo->yAxis)
					&&(pInfo->vInfo.mode&IDEV_GEN_PTR_Y)) {
	pos= vNew->value[ptrInfo->yAxis-vNew->firstValuator];
	pos= idevTransformCoord(&ptrInfo->yTransform,pos,ptrInfo->y,
					ptrInfo->minY,ptrInfo->maxY,flags);
	if (pos<ptrInfo->minY) {
	    pos= ptrInfo->minY;
	    ptrFlags|= QE_Y_CLAMPED;
	}
	else if (pos>ptrInfo->maxY) {
	    pos= ptrInfo->maxY;
	    ptrFlags|= QE_Y_CLAMPED;
	}
	if ((pos!=ptrInfo->y)||(flags&(QE_RESPONSE|IDEV_FORCE_EVENT))||
				(ptrFlags&QE_Y_CLAMPED))
	{
	   ptrInfo->y= pos;
	   ptrFlags|= IDEV_GEN_PTR_Y;
	}
	else ptrFlags&= (~QE_Y_CLAMPED);
    }
    if (ptrFlags) {
	mblk_t				*mbp;
	register struct shmqevent	*msg;
	if (!(mbp=allocb(sizeof(struct shmqevent),BPRI_MED))){
	    cmn_err( CE_WARN, "Couldn't allocate streams buffer -- valuator events lost\n");
	    return(0); /* just pitch valuator events */
	}
	msg = (struct shmqevent *) mbp->b_wptr;
	msg->un.id.devminor=	pInfo->sInfo.shmiqid.devminor;
	msg->un.id.index= 	pInfo->sInfo.shmiqid.index;
	msg->data.device= 	pInfo->sInfo.shmiqid.index;
	msg->data.type=		QE_PTR_EVENT;
	msg->data.which=	0;
	msg->data.un.ptraxis[0]=ptrInfo->x;
	msg->data.un.ptraxis[1]=ptrInfo->y;
	msg->data.flags=	(flags&QE_RESPONSE)|ptrFlags;
	mbp->b_wptr+=	sizeof (struct shmqevent);
	putnext( pInfo->sInfo.rq, mbp );
    }
    return 1;
}


/***====================================================================***/

int
idevGenValEvents(idevInfo		*pInfo,
		 idevValuatorState 	*vNew,
		 unsigned 		 flags)
{
mblk_t				*mbp;
register struct shmqevent	*msg;
int			 	i,eventCnt;
int			 	ndx,pos;
int			 	rtrn= 1;
int			 	clamped;

	if ((vNew->nValuators<1)
		||(vNew->firstValuator+vNew->nValuators>pInfo->vInfo.nVal))
		
		return 0;
	if (pInfo->vInfo.mode&IDEV_GEN_PTR_EVENTS) 
		rtrn|= idevGenPtrEvent(pInfo,vNew,flags);
	/*
	 * purpose 2.
	 *
	 * Don't allocate a message buffer (and therefore don't send a
	 * message) if we are trying to be silent (IDEV_SILENT).  We don't
	 * want to generate "echoed" valuator events due to an explicit
	 * pointer relocation by the X server.
	 */
	if (pInfo->vInfo.mode&IDEV_GEN_NON_PTR_EVENTS && !(flags&IDEV_SILENT)) {
		/* Get a message buffer to send upstream */
		if (!(mbp=allocb(vNew->nValuators*sizeof(struct shmqevent),
								BPRI_MED))){
			cmn_err( CE_WARN, "Couldn't allocate streams buffer -- valuator events lost\n");
		}
		else msg = (struct shmqevent *) mbp->b_wptr;
	}
	else mbp= NULL;

	ndx= vNew->firstValuator;
	for (eventCnt=i=0;i<vNew->nValuators;i++,ndx++) {
		pos= idevTransformCoord(&pInfo->vInfo.transform[ndx],
					vNew->value[i],
					pInfo->vInfo.sysValue[ndx],
					pInfo->vInfo.desc[ndx].minVal,
					pInfo->vInfo.desc[ndx].maxVal,
					flags);
		if ((pos==pInfo->vInfo.sysValue[ndx])
				&&(!(flags&(QE_RESPONSE|IDEV_FORCE_EVENT))))
			continue;

		else if (pos<pInfo->vInfo.desc[ndx].minVal) {
			clamped = QE_CLAMPED;
			rtrn |=	QE_CLAMPED;
			pos = pInfo->vInfo.desc[ndx].minVal;
		}
		else if (pos>pInfo->vInfo.desc[ndx].maxVal) {
			clamped = QE_CLAMPED;
			rtrn |= QE_CLAMPED;
			pos = pInfo->vInfo.desc[ndx].maxVal;
		}
		else
			clamped = 0;

		pInfo->vInfo.sysValue[ndx] = pos;

		if (mbp&&(idevNth(pInfo->vInfo.active,ndx))) {
			msg->un.id.devminor = pInfo->sInfo.shmiqid.devminor;
			msg->un.id.index = pInfo->sInfo.shmiqid.index;
			msg->data.device = pInfo->sInfo.shmiqid.index;
			msg->data.type = QE_VAL_EVENT;
			msg->data.which = ndx;
			msg->data.flags = (flags|QE_MORE_EVENTS|clamped)&0xff;
			msg->data.un.pos = pos;
			eventCnt++;
			msg++;
		}
	}

	if (mbp) {
		if (eventCnt>0) {
			if (!(flags&QE_MORE_EVENTS))
				(--msg)->data.flags &= (~QE_MORE_EVENTS);

			mbp->b_wptr += sizeof (struct shmqevent)*eventCnt;
			putnext( pInfo->sInfo.rq, mbp );
		}
		else {
			freemsg(mbp);
		}
	}
	return(rtrn);
}

/***====================================================================***/

int
idevGenValEvent(idevInfo	*pInfo,
		int		 axis,
		int		 value,
		unsigned	 flags)
{
idevValuatorState	new;
mblk_t				*mbp;
register struct shmqevent	*msg;
int				 rtrn= 1;
idevValInfo			*pVInfo;

    /* make sure axis is legal and active */
    if ((axis<0)||(axis>=pInfo->vInfo.nVal))
	return 0;
    new.firstValuator = axis;
    new.nValuators = 1;
    new.value[0] = value;
    return idevGenValEvents(pInfo,&new,flags);
}

/***====================================================================***/

int
idevGetValDesc(idevInfo *pInfo, idevGetSetValDesc *axis)
{
    if (axis->valNum<pInfo->vInfo.nVal) {
	axis->desc=	pInfo->vInfo.desc[axis->valNum];
	return 1;
    }
    return 0;
}

/***====================================================================***/

/* Parameters for "Warp" */
#define MAXRAWDELTA 254 /* abs. value of the sum of two signed char's */
/* defaults -- totally arbitrary, my choice */
#define DFLT_MULTIPLIER 16
#define DFLT_CRVFACT 16
#define DFLT_INFLECTION ( MAXRAWDELTA / 3 )
/* default acceleration == ~2.7 above 8 */
#define DFLT_THRESHOLD 8
#define DFLT_ACCELERATORN 8
#define DFLT_ACCELERATORD 3

idevTransform idevDfltAccel = {
	IDEV_ACCEL,
	IDEV_ACCEL,
	0,
	DFLT_ACCELERATORN,
	DFLT_ACCELERATORD,
	DFLT_MULTIPLIER,
	DFLT_CRVFACT,
	DFLT_INFLECTION,
	DFLT_THRESHOLD
};

idevTransform idevDfltScale = {
	IDEV_SCALE,
	IDEV_SCALE,
	0,
	1, 1
};


static int
idevCopyTransform(unsigned which,idevTransform *old,idevTransform *new)
{

	if (which&IDEV_VT_SET_TRANSFORM) {
		if (new->which) {
		    if (new->which&old->possible) {
			old->which= (new->which&old->possible);
		    }
		    else {
			return 0;
		    }
		}
		else old->which= 0;
	}
	if (which&IDEV_VT_SET_FLAGS) {
		old->flags= new->flags;
	}
	if (which&IDEV_VT_SET_NUMERATOR) {
		old->numerator = new->numerator;
	}
	if (which&IDEV_VT_SET_DENOMINATOR) {
		if (new->denominator==0)
			old->denominator = 1;
		else	old->denominator = new->denominator;
	}
	if (which&IDEV_PC_SET_THRESHOLD) {
		old->threshold = new->threshold;
	}
	if (which&IDEV_PC_SET_MULTIPLIER) {
		if ( new->m1 > 0xFF )
			old->m1 = 0xFF;
		else if ( new->m1 <= 0 )
			old->m1 = DFLT_MULTIPLIER;
		else	old->m1 = new->m1;
	}
	if (which&IDEV_PC_SET_CURVE_FACTOR) {
		if ( new->cM > 0xFF )
			old->cM = 0xff;
		else if ( new->cM < 0 )
			old->cM = DFLT_CRVFACT;
		else	old->cM = new->cM;
	}
	if (which&IDEV_PC_SET_CURVE_INFLECTION) {
		if ( new->inflection < 0 )
			old->inflection = MAXRAWDELTA / 2;
		else if ( new->inflection >= MAXRAWDELTA )
			old->inflection = MAXRAWDELTA - 1;
		else	old->inflection = new->inflection;
	}
	return 1;
}

/***====================================================================***/

int
idevSetPtrCtrl(idevInfo *pInfo,idevPtrControl *ctrl)
{
idevTransform	*pXAccel= &pInfo->pInfo.xTransform;
idevTransform	*pYAccel= &pInfo->pInfo.yTransform;

	if (ctrl->which&IDEV_PC_SET_ACCEL_N) {
		pXAccel->numerator = ctrl->accelNumerator;
		pYAccel->numerator = ctrl->accelNumerator;
	}
	if (ctrl->which&IDEV_PC_SET_ACCEL_D) {
		register int d= ctrl->accelDenominator;
		if (d==0)
			d = 1;
		pXAccel->denominator = pYAccel->denominator = d;
	}
	if (ctrl->which&IDEV_PC_SET_THRESHOLD) {
		pXAccel->threshold = ctrl->threshold;
		pYAccel->threshold = ctrl->threshold;
	}
	if (ctrl->which&IDEV_PC_SET_MULTIPLIER) {
		register int m = ctrl->mult;
		if ( m > 0xFF )
			m = 0xFF;
		else if ( m <= 0 )
			m = DFLT_MULTIPLIER;
		pXAccel->m1 = pYAccel->m1= m;
	}
	if (ctrl->which&IDEV_PC_SET_CURVE_FACTOR) {
		register int c = ctrl->curveFactor;
		if ( c > 0xFF )
			c = 0xff;
		else if ( c < 0 )
			c = DFLT_CRVFACT;
		pXAccel->cM = pYAccel->cM = c;
	}
	if (ctrl->which&IDEV_PC_SET_CURVE_INFLECTION) {
		register int c = ctrl->curveInflection;
		if ( c < 0 )
			c = MAXRAWDELTA / 2;
		else if ( c >= MAXRAWDELTA )
			c = MAXRAWDELTA - 1;
		pXAccel->inflection = pYAccel->inflection = c;
	}
	return 1;
}

/***====================================================================***/

int
idevGetTransform(idevInfo *pInfo,idevGetSetTransform *trans)
{
	if (trans->which&IDEV_VT_PTR_X) {
		trans->transform= pInfo->pInfo.xTransform;
		return 1;
	}
	if (trans->which&IDEV_VT_PTR_Y) {
		trans->transform= pInfo->pInfo.yTransform;
		return 1;
	}
	if (trans->valNum<pInfo->vInfo.nVal) {
		trans->transform= pInfo->vInfo.transform[trans->valNum];
		return 1;
	}
	return 0;
}


/***====================================================================***/

int
idevSetTransform(idevInfo *pInfo,idevGetSetTransform *set)
{
unsigned which = set->which;

	if ((which&IDEV_VT_SET_PTR_AND_VAL)&&(!(which&IDEV_VT_SET_PTR))) {
		if (set->valNum==pInfo->pInfo.xAxis)
			which|= IDEV_VT_PTR_X;
		if (set->valNum==pInfo->pInfo.yAxis)
			which|= IDEV_VT_PTR_Y;
	}
	if (which&IDEV_VT_PTR_X) {
		idevCopyTransform(which,&pInfo->pInfo.xTransform,
						&set->transform);
		if ((which&IDEV_VT_SET_PTR_AND_VAL)
				&&(pInfo->pInfo.xAxis<pInfo->vInfo.nVal)) {
			idevCopyTransform(which,
				  &pInfo->vInfo.transform[pInfo->pInfo.xAxis],
				  &set->transform);
		}
	}
	if (which&IDEV_VT_PTR_Y) {
		idevCopyTransform(which,&pInfo->pInfo.yTransform,
						&set->transform);
		if ((which&IDEV_VT_SET_PTR_AND_VAL)
				&&(pInfo->pInfo.yAxis<pInfo->vInfo.nVal)) {
			idevCopyTransform(which,
				  &pInfo->vInfo.transform[pInfo->pInfo.yAxis],
				  &set->transform);
		}
	}
	if (which&IDEV_VT_SET_PTR)
		return 1;
	if (set->valNum<pInfo->vInfo.nVal) {
		idevCopyTransform(which,&pInfo->vInfo.transform[set->valNum],
			 			&set->transform);
		return 1;
	}
	return 0;
}


/***====================================================================***/

/* ARGSUSED */
int
idevSetValDesc(idevInfo *pInfo,idevGetSetValDesc *new)
{
int			count,i;
idevValuatorDesc	*old;
int			*pOldValue;
idevValInfo 		*pVInfo= &pInfo->vInfo;

    if (new->valNum>=pVInfo->nVal)
	return 0;

    if (new->flags&IDEV_SAME) {
	new->valNum=	0;
	old=		&pVInfo->desc[0];
	pOldValue=	&pVInfo->sysValue[0];
	count=		pVInfo->nVal;
    }
    else {
	old=		&pVInfo->desc[new->valNum];
	pOldValue=	&pVInfo->sysValue[new->valNum];
	count=		1;
    }

    for (i=0;i<count;i++,old++) {
	if (new->flags&IDEV_SET_MIN) {
	    old->minVal=	new->desc.minVal;
	    if (old->minVal<old->hwMinVal)
		old->minVal=	old->hwMinVal;
	    if ((*pOldValue)<old->minVal) {
		(*pOldValue)=	old->minVal;
		idevGenValEvent(pInfo,i+new->valNum,old->minVal,
						QE_CLAMPED|QE_RESPONSE);
	    }
	}
	if (new->flags&IDEV_SET_MAX) {
	    old->maxVal=	new->desc.maxVal;
	    if (old->maxVal>old->hwMaxVal) 
		old->maxVal=	old->hwMaxVal;
	    if ((*pOldValue)>old->maxVal) {
		(*pOldValue)= old->maxVal;
		idevGenValEvent(pInfo,i+new->valNum,old->maxVal,
						QE_RESPONSE|QE_CLAMPED);
	    }
	    if ((pInfo->pInfo.maxX==0)&&(i+new->valNum==pInfo->pInfo.xAxis)) {
		pInfo->pInfo.minX= old->minVal;
		pInfo->pInfo.maxX= old->maxVal;
	    }
	    else if ((pInfo->pInfo.maxY==0)
					&&(i+new->valNum==pInfo->pInfo.yAxis)) {
		pInfo->pInfo.minY= old->minVal;
		pInfo->pInfo.maxY= old->maxVal;
	    }
	}
	if (new->flags&IDEV_SET_RESOLUTION) {
	    old->resolution=	new->desc.resolution;
	    if (old->resolution<old->hwMinRes)
		 old->resolution= old->hwMinRes;
	    else if (old->resolution>old->hwMaxRes)
		 old->resolution= old->hwMaxRes;
	}
    }
    return(1);
}

/***====================================================================***/

/* ARGSUSED */
int
idevGetValState(idevInfo *pInfo,register idevValuatorState *vals)
{
    if ((vals->firstValuator+vals->nValuators<pInfo->vInfo.nVal)&&
	(vals->nValuators<=IDEV_VALUATOR_STATE_MAX)) {
	bcopy(&pInfo->vInfo.sysValue[vals->firstValuator],vals->value,
		4*vals->nValuators);
	return 1;
    }
    return 0;
}

/***====================================================================***/

	/*
	 * Passes a button event upstream (presumably to the shared
	 * memory queue).
	 */

int
idevGenBtnEvent( idevInfo *pInfo, unsigned btnNum, int flags)
{
register struct shmqevent	*msg;
int				 eventcount= 0;
mblk_t				*mbp;
unsigned			 mask,offset;


    mask = idevMask(btnNum);
    offset = idevOffset(btnNum);

    if ( pInfo->bInfo.state[offset] & mask ) {
	if (flags&IDEV_BTN_PRESS) {
	    if (flags&IDEV_FAKE_EVENT)
		eventcount = 2;
	    else if (flags&IDEV_FORCE_EVENT)
		eventcount = 1;
	    else eventcount = 0;
	}
	else eventcount = 1;
    }
    else  if (!(flags&IDEV_BTN_PRESS)) {
	if (flags&IDEV_FAKE_EVENT)
	    eventcount = 2;
	else if (flags&IDEV_FORCE_EVENT)
	    eventcount = 1;
	else eventcount = 0;
    }
    else eventcount = 1;

    if (flags&IDEV_BTN_PRESS)	pInfo->bInfo.state[offset] |= mask;
    else			pInfo->bInfo.state[offset] &= ~mask;

    if ((!(pInfo->bInfo.active[offset]&mask))||eventcount<1) {
	return(0);
    }

    if (!(mbp=allocb(eventcount*sizeof(struct shmqevent), BPRI_MED))) {
	cmn_err( CE_WARN,"Couldn't allocate streams buffer -- button event lost.\n" );
	return(0); /* just pitch button events */
    }
    msg = (struct shmqevent *) mbp->b_wptr ;

    if (eventcount>1) {
	msg->un.id.devminor=	pInfo->sInfo.shmiqid.devminor;
	msg->un.id.index=	pInfo->sInfo.shmiqid.index;
	msg->data.device=	pInfo->sInfo.shmiqid.index;
	msg->data.type=		QE_BTN_EVENT;
	msg->data.which=	btnNum;
	msg->data.flags=	(flags&IDEV_BTN_PRESS?0:QE_BTN_DOWN);
	msg++;
    }
    msg->un.id.devminor=	pInfo->sInfo.shmiqid.devminor;
    msg->un.id.index=		pInfo->sInfo.shmiqid.index;
    msg->data.device=		pInfo->sInfo.shmiqid.index;
    msg->data.type=		QE_BTN_EVENT;
    msg->data.which=		btnNum;
    msg->data.flags=		(flags&IDEV_BTN_PRESS?QE_BTN_DOWN:0);

    mbp->b_wptr += eventcount*sizeof (struct shmqevent) ;
    putnext( pInfo->sInfo.rq, mbp ) ;
    return(eventcount);
}

/***====================================================================***/

	/*
	 * Passes button events upstream (presumably to the shared
	 * memory queue).
	 */

unsigned char leftmask[8]= {
	0xff, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f
};
int
idevGenBtnEvents( idevInfo *pInfo, unsigned char *newMask,
					unsigned char *newState)
{
register struct shmqevent	*msg;
mblk_t				*mbp;
register int byte,bit;
unsigned char	changed,mask;
int		events= 0;

    newMask[idevSize(pInfo->bInfo.nBtn)-1]&= leftmask[pInfo->bInfo.nBtn&0x7];
    for (byte=0;byte<idevSize(pInfo->bInfo.nBtn);byte++) {
	changed= (pInfo->bInfo.state[byte]^newState[byte])&(newMask[byte]);
	if (changed) {
	    for (mask=1,bit=0;(bit<8)&&changed;bit++,mask<<=1) {
		if (changed&pInfo->bInfo.active[byte]&mask) {
		    changed&= ~mask;
		    if (mbp=allocb(sizeof(struct shmqevent),BPRI_MED)) {
			msg = (struct shmqevent *) mbp->b_wptr ;

			msg->un.id.devminor = pInfo->sInfo.shmiqid.devminor;
			msg->un.id.index = pInfo->sInfo.shmiqid.index;
			msg->data.device = pInfo->sInfo.shmiqid.index;
			msg->data.type = QE_BTN_EVENT;
			msg->data.which = (byte<<3)+bit;
			msg->data.flags = ( newState[byte] & mask 
				? IDEV_BTN_PRESS
				: 0 );
			mbp->b_wptr += sizeof (struct shmqevent);
			putnext( pInfo->sInfo.rq, mbp );
			events++;
		    }
		    else {
			cmn_err(CE_WARN,"Couldn't allocate streams buffer -- button event lost.\n");
			continue;
		    }
		}
	    }
	    pInfo->bInfo.state[byte]&=	~newMask[byte];
	    pInfo->bInfo.state[byte]|=	(newState[byte]&newMask[byte]);
	}
    }
    return events;
}

/***====================================================================***/

int
idevSetPtrMode(idevInfo *pInfo,idevPtrMode *pMode)
{
int	rtrn= 1;

    if (pMode->mode&IDEV_GEN_PTR_EVENTS) {
	if (pMode->mode&IDEV_GEN_PTR_X) {
	    if ((pMode->xAxis>=pInfo->vInfo.nVal)||(pMode->xAxis<0))
		rtrn= 0;
	    else {
		pInfo->pInfo.xAxis= pMode->xAxis;
		if (pInfo->pInfo.minX==pInfo->pInfo.maxX) {
		    pInfo->pInfo.minX= pInfo->vInfo.desc[pMode->xAxis].minVal;
		    pInfo->pInfo.maxX= pInfo->vInfo.desc[pMode->xAxis].maxVal;
		}
	    }
	}
	if (pMode->mode&IDEV_GEN_PTR_Y) {
	    if ((pMode->yAxis>=pInfo->vInfo.nVal)||(pMode->xAxis<0))
		rtrn= 0;
	    else {
		pInfo->pInfo.yAxis= pMode->yAxis;
		if (pInfo->pInfo.minY==pInfo->pInfo.maxY) {
		    pInfo->pInfo.minY= pInfo->vInfo.desc[pMode->yAxis].minVal;
		    pInfo->pInfo.maxY= pInfo->vInfo.desc[pMode->yAxis].maxVal;
		}
	    }
	}
    }
    else {
	pInfo->pInfo.xAxis= pInfo->pInfo.yAxis= -1;
    }
    pInfo->vInfo.mode=	pMode->mode;
    return rtrn;
}

/***====================================================================***/

int
idevSetPtrBounds(idevInfo *pInfo,idevPtrBounds *pBounds)
{
int	rtrn= 1;

    if (pBounds->which&IDEV_PB_SET_MIN_X) {
	if (pBounds->minX>0)
		pInfo->pInfo.minX= pBounds->minX;
	else	pInfo->pInfo.minX= 0;
    }
    if (pBounds->which&IDEV_PB_SET_MAX_X) {
	if (pBounds->maxX>pBounds->minX)
		pInfo->pInfo.maxX= pBounds->maxX;
	else	pInfo->pInfo.maxX= pBounds->minX;
    }
    if (pBounds->which&IDEV_PB_SET_MIN_Y) {
	if (pBounds->minY>0)
		pInfo->pInfo.minY= pBounds->minY;
	else	pInfo->pInfo.minY= 0;
    }
    if (pBounds->which&IDEV_PB_SET_MAX_Y) {
	if (pBounds->maxY>pBounds->minY)
		pInfo->pInfo.maxY= pBounds->maxY;
	else	pInfo->pInfo.maxY= pBounds->minY;
    }
    return 1;
}

/***====================================================================***/

int
idevSetPtr(idevInfo *pInfo,idevPtrVals *pVals)
{
int	rtrn= 1;
idevValuatorState new;
int loudness;

    /*
     * The point of IDEV_SILENT is when the pointer is being
     * relocated explicitly by the X server.  This does two things:
     *
     * 1) Makes the pointer event be generated but has it get
     * marked as a response (QE_RESPONSE) so the X server can know
     * the relocation has been acknowledged (it wants to drop any
     * intermediate pointer events between the relocation and the
     * response).
     *
     * 2) We want to be careful not to "echo" any non-pointer
     * valuator events.
     *
     * See the "purpose 1" and "purpose 2" comments.
     */
    if(pVals->which&IDEV_SILENT) {
       loudness = QE_RESPONSE|IDEV_SILENT; /* purpose 1 */
    } else {
       loudness = 0;
    }
    new.nValuators = 1;
    new.firstValuator = pInfo->pInfo.xAxis;
    new.value[0] = pVals->x;
    if (pVals->which&IDEV_PTR_SET_X_VAL) {
	rtrn= idevGenValEvents(pInfo,&new,
	   IDEV_VALS_ABSOLUTE|IDEV_NO_TRANSFORM|loudness) &&rtrn;
    }
    else if (pVals->which&IDEV_PTR_SET_X) {
	rtrn= idevGenPtrEvent(pInfo,&new,
	   IDEV_VALS_ABSOLUTE|IDEV_NO_TRANSFORM|loudness) &&rtrn;
    }
    new.nValuators = 1;
    new.firstValuator = pInfo->pInfo.yAxis;
    new.value[0] = pVals->y;
    if (pVals->which&IDEV_PTR_SET_Y_VAL) {
	rtrn= idevGenValEvents(pInfo,&new,
	   IDEV_VALS_ABSOLUTE|IDEV_NO_TRANSFORM|loudness) &&rtrn;
    }
    else if (pVals->which&IDEV_PTR_SET_Y) {
	rtrn= idevGenPtrEvent(pInfo,&new,
	   IDEV_VALS_ABSOLUTE|IDEV_NO_TRANSFORM|loudness) &&rtrn;
    }
    return rtrn;
}

/***====================================================================***/

int
idevUpdateBitArray(int nBytes,char *old, register idevBitVals *new)
{
	register int i;

	for (i=nBytes-1; i>=0; i-- ) {
		old[i] &= ~new->mask[i];
		old[i] |= (new->mask[i]&new->value[i]);
	}
	return 1;
}

/***====================================================================***/

static struct termio idevDfltLineSettings = {
	IGNBRK|IGNPAR,	/* c_iflag */
	0,		/* c_oflag */
	B9600|CS8|CREAD, /* c_cflag */
	0,			/* c_lflag */
	LDISC1,			/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   3,    1,     0  }
};

int
idevChangeLineSettings(idevInfo *pInfo,struct termio *tio)
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
	iob->b_datap->db_type= M_IOCTL;
	iocb = (struct iocblk *)iob->b_wptr;
	iob->b_wptr+= sizeof(struct iocblk);
	iocb->ioc_cmd= TCSETAW;
/* XXXjwag - uid and gid are now part of cred struct which hasn't
 * been allocated.. but who uses this anyway??
	iocb->ioc_uid= 0;
	iocb->ioc_gid= 0;
 */
	iocb->ioc_id=  0;
	iocb->ioc_count= sizeof(struct termio);
	iocb->ioc_error= 0;
	iocb->ioc_rval= 0;
	if (tio==NULL)
		tio= &idevDfltLineSettings;
	*((struct termio *)iob->b_cont->b_wptr)= *tio;
	iob->b_cont->b_wptr+= sizeof(struct termio);
	putnext( pInfo->sInfo.wq, iob );
	return 1;
}

/***====================================================================***/

int
idev_rput( register queue_t *rq, mblk_t *mp )
{
	idevInfo *info;

	info = (idevInfo *) rq->q_ptr;

	switch ( mp->b_datap->db_type ) {
		case M_FLUSH:
			if ( *mp->b_rptr & FLUSHR ) {
				flushq( rq, FLUSHDATA ) ;
			}
		break ;

		case M_DATA:
		{
			register mblk_t *bp;
			for (bp=mp; bp!=NULL; bp= bp->b_cont) {
				(*info->sInfo.readData)( info, bp->b_rptr, 
						bp->b_wptr-bp->b_rptr);
			}
			freemsg( mp );
			return 0;
		}
		default:
			break ;
	}
	if ( queclass( mp ) > QNORM || canput( rq->q_next ) )
		putnext( rq, mp );
	else
		freemsg(mp);
	return 0;
}

/***====================================================================***/

char *
idev_ioctl_name(unsigned io)
{
    switch(io) {
	case IDEVGETDEVICEDESC: return "GETDEVICEDESC";
	case IDEVGETVALUATORDESC: return "GETVALUATORDESC";
	case IDEVGETKEYMAPDESC: return "GETKEYMAPDESC";
	case IDEVGETSTRDPYDESC: return "GETSTRDPYDESC";
	case IDEVGETINTDPYDESC: return "GETINTDPYDESC";

	case IDEVGETBUTTONS: return "GETBUTTONS";
	case IDEVGETVALUATORS: return "GETVALUATORS";
	case IDEVGETLEDS: return "GETLEDS";
	case IDEVGETSTRDPY: return "GETSTRDPY";
	case IDEVGETINTDPYS: return "GETINTDPYS";
	case IDEVENABLEBUTTONS: return "ENABLEBUTTONS";

	case IDEVENABLEVALUATORS: return "ENABLEVALUATORS";
	case IDEVSETVALUATORS: return "SETVALUATORS";
	case IDEVCHANGEVALUATORS: return "CHANGEVALUATORS";
	case IDEVSETVALUATORDESC: return "SETVALUATORDESC";

	case IDEVSETLEDS: return "SETLEDS";
	case IDEVSETSTRDPY: return "SETSTRDPY";
	case IDEVSETINTDPYS: return "SETINTDPYS";
	case IDEVRINGBELL: return "RINGBELL";

	case IDEVKEYBDCONTROL: return "KEYBDCONTROL";
	case IDEVPTRCONTROL: return "PTRCONTROL";
	case IDEVOTHERCONTROL: return "OTHERCONTROL";
	case IDEVSETPTRMODE: return "SETPTRMODE";
	case IDEVOTHERQUERY: return "OTHERQUERY";
	case IDEVSETPTRBOUNDS: return "SETPTRBOUNDS";
	case IDEVSETPTR	: return "SETPTR	";

	case IDEVSETTRANSFORM: return "SETTRANSFORM";
	case IDEVGETTRANSFORM: return "GETTRANSFORM";

	case IDEVINITDEVICE: return "INITDEVICE";
    }
    return "unknown";
}

/***====================================================================***/

int
idev_wput( register queue_t *wq, register mblk_t *mp )
{
	idevInfo *info;
	register char *stuff;
	int size;

	info = (idevInfo *) wq->q_ptr;
	stuff = mp->b_rptr;
	size = mp->b_wptr-mp->b_rptr;

	switch ( mp->b_datap->db_type ) {
		case M_PCPROTO :
			if ((mp->b_wptr-stuff==sizeof(struct shmqntc))
				 && (((struct shmqntc *)stuff )->mtype == SHMIQ_NOTICE)) {
				info->sInfo.shmiqid.devminor =
					((struct shmqntc *)stuff)->id.devminor;
				info->sInfo.shmiqid.index =
					((struct shmqntc *)stuff)->id.index;
				((struct shmqntc *)stuff)->mtype = SHMIQ_PLAY;
				qreply( wq, mp ) ;
				return 0;
			}
			break;

		case M_FLUSH:
			break;

		case M_IOCTL:
		{
			struct iocblk *iocp;
			int found = 0;
			mblk_t *nmp = mp->b_cont;
		
			iocp = (struct iocblk *)stuff;

			if (nmp == NULL)  {
				stuff= NULL;
				size= NULL;
			}
			else if (nmp->b_datap->db_type != M_DATA) {
				mp->b_datap->db_type = M_IOCNAK;
				qreply(wq, mp);
				return 0;
			}
			else {
				stuff = nmp->b_rptr;
				size = nmp->b_wptr-nmp->b_rptr;
			}

			if ((*info->sInfo.writeIoctl)(info, iocp->ioc_cmd, 
						 size, stuff, &found))
			{
				mp->b_datap->db_type = M_IOCACK;
				iocp->ioc_rval = 0;
				qreply(wq, mp);
				return 0;
			}
			else if (found) {
				iocp->ioc_error = EINVAL;
				iocp->ioc_rval =	-1;
				mp->b_datap->db_type = M_IOCNAK;
				qreply(wq,mp);
				return 0;
			}
			/* FALL THROUGH TO "default" case */
		}
		default:
			break;
	}
	if ( canput( wq->q_next ) )
		putnext(wq, mp);
	else 
		freemsg(mp);
	return 0;
}
