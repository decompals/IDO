/**************************************************************************
 *                                                                        *
 *              Copyright ( C ) 1990, Silicon Graphics, Inc.              *
 *                                                                        *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *                                                                        *
 **************************************************************************/
#ident "hitachi.c $Revision: 1.2 $"

#ifdef INPUT_TEST
#define _KERNEL
#include "sys/types.h"
#undef _KERNEL
#endif
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/param.h"
#include "sys/kmem.h"
#include "sys/mload.h"

#include "sys/ioctl.h"
#include "sys/termio.h"
#include "sys/debug.h"
#include "string.h"

/* Streams stuff */
#include "sys/stream.h"
#include "sys/strmp.h"

#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h"
#define _KERNEL
extern int sginap(long);
#define STREAMS_DELAY(t) sginap(t)
#endif /* INPUT_TEST */

/* my stuff */
#include "sys/shmiq.h"
#include "sys/idev.h"
#include "sys/hitachi.h"

#include "sys/ddi.h"

#ifndef NULL
#define NULL 0
#endif

#ifndef isdigit
#define	isdigit(c)	(((c)>='0')&&((c)<='9'))
#endif

	/*
	 * Driver for Hitachi HDG-J series tablet running in
	 * Hitachi III mode.
	 *
	 * This driver uses 7/10 byte Hitachi III mode.  That's good
	 * enough only for small tablets.  Large tablets will need to
	 * use 12 byte mode to cover the full area of the tablet.
	 * htchi_intr must be modified to understand 12-byte packets.
	 *
	 * Written by Mark Callow - August 1994
	 */
	 
        /*
         * Macros and variables specific to HITACHI 
         * go here
         */

#define X_VAL	    0
#define Y_VAL	    1
#define PRESSURE_VAL   2
#define HEIGHT_VAL  3
#define XTILT_VAL   4
#define YTILT_VAL   5

static idevDesc _htchi_desc = {
        HITACHI_NAME,             /* devName */
        HITACHI_TYPE,             /* devType */
        HITACHI_NUM_BUTTONS,      /* nButtons */
        HITACHI_NUM_VALUATORS,    /* nValuators */
        0,                      /* nLEDs */
        0,                      /* nStrDpys */
        0,                      /* nIntDpys */
        0                       /* flags */
};

static struct termio _htchi_termio = {
        IGNBRK|IGNPAR,		/* c_iflag */
        0,			/* c_oflag */
        B9600|CS8|CREAD|HUPCL,	/* c_cflag */
        0,                      /* c_lflag */
        LDISC1,                 /* c_line */
        /* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
        {      0,    0,     0,    0,   0,   7,    1,     0  }
};

static idevValuatorDesc _htchi_dflt_val_desc = {
        HITACHI_AXIS_VALUATOR_MIN_RESOLUTION,       /* hwMinRes */
        HITACHI_AXIS_VALUATOR_MAX_RESOLUTION,       /* hwMaxRes */
        HITACHI_X_VALUATOR_MIN,			    /* hwMinVal */
        HITACHI_X_VALUATOR_MAX,			    /* hwMaxVal */
        IDEV_ABSOLUTE,				    /* possibleModes */
        IDEV_ABSOLUTE,				    /* mode */
        HITACHI_AXIS_VALUATOR_RESOLUTION,	    /* resolution */
        HITACHI_X_VALUATOR_MIN,			    /* minVal */
        HITACHI_X_VALUATOR_MAX			    /* maxVal */
};

static idevValuatorDesc _htchi_tilt_val_desc = {
	1,					    /* hwMinRes */
	1,					    /* hwMaxRes */
	HITACHI_TILT_VALUATOR_MIN,		    /* hwMinVal */
	HITACHI_TILT_VALUATOR_MAX,		    /* hwMaxVal */
	IDEV_ABSOLUTE,				    /* possibleModes */
	IDEV_ABSOLUTE,				    /* mode */
	1,					    /* resolution */
	HITACHI_TILT_VALUATOR_MIN,		    /* minVal */
	HITACHI_TILT_VALUATOR_MAX		    /* maxVal */
};

static idevValuatorDesc _htchi_pressure_val_desc = {
	1,					    /* hwMinRes */
	1,					    /* hwMaxRes */
	HITACHI_PRESSURE_VALUATOR_MIN,		    /* hwMinVal */
	HITACHI_PRESSURE_VALUATOR_MAX,		    /* hwMaxVal */
	IDEV_ABSOLUTE,				    /* possibleModes */
	IDEV_ABSOLUTE,				    /* mode */
	1,					    /* resolution */
	HITACHI_PRESSURE_VALUATOR_MIN,		    /* hwMaxVal */
	HITACHI_PRESSURE_VALUATOR_MAX,		    /* hwMinVal */
};

static int hitachiref = 0;                /* reference count for unload */
char *hitachimversion = M_VERSION;
int hitachidevflag = 0;

#define HTCHI_SCALE(n)	(((n)*4)/10)

/*
 * This driver is for 7/10 byte Hitachi III mode.  That's good enough
 * only for small tablets.  Large tablets will have to use 12 byte mode.
 */
#define HTCHI_SETUP       "\1SU"
#define HTCHI_CUSTOM	  "\1CU"
#define HTCHI_HIII	  "\1HJ"
#define HTCHI_MODE	  "\DF\1FU\1AD"	    /* upper-left origin, disable buzz */
#define HTCHI_BINARY	  "\1BI\1D8"	    /* 7-byte binary mode */
#define HTCHI_RESOLUTION  "\1MT\1HE\1L2"    /* mm, high res, 0.01mm */
#define HTCHI_PRESSURE    "\1PP\1ZE\1TE"    /* Pressure, height & tilt */
#define HTCHI_RATE        "\1R1"	    /* max output rate */
#define HTCHI_STREAM      "\1CN"	    /* run mode */
#define HTCHI_INCR        "\1IC"	    /* incremental mode */
#define HTCHI_EXECUTE	  "\1EX"
/*
 * We may have to add a RESET command at the end.  If so, we must
 * add a half-second delay after sending EX, before we send RS
 */
/* #define HTCHI_RESET	  "\1RS" */
#define HTCHI_INIT_LEN    (sizeof(HTCHI_MODE) + sizeof(HTCHI_BINARY) \
			   + sizeof(HTCHI_RESOLUTION) + sizeof(HTCHI_PRESSURE) \
                           + sizeof(HTCHI_RATE) + sizeof(HTCHI_STREAM) \
			   + sizeof(HTCHI_INCR)+ sizeof(HTCHI_EXECUTE))

int
htchi_init_device(htchi_state_t *htchi)
{
mblk_t                  *mbp;
int			init_len;

    if (!idevChangeLineSettings(&htchi->info,&_htchi_termio)) {
        cmn_err(CE_WARN,"Hitachi tablet not initialized.\n");
        return 0;
    }
    if (mbp = allocb(sizeof(HTCHI_SETUP), BPRI_MED)) {
	strcpy(mbp->b_wptr, HTCHI_SETUP);
	putnext(htchi->info.sInfo.wq, mbp);
	STREAMS_DELAY(HZ/2);
	if (mbp = allocb(sizeof(HTCHI_CUSTOM), BPRI_MED)) {
	    strcpy(mbp->b_wptr, HTCHI_CUSTOM);
	    putnext(htchi->info.sInfo.wq, mbp);
	    STREAMS_DELAY(HZ/2);
	    if (mbp = allocb(sizeof(HTCHI_HIII), BPRI_MED)) {
		strcpy(mbp->b_wptr, HTCHI_HIII);
		putnext(htchi->info.sInfo.wq, mbp);
		STREAMS_DELAY(HZ/2);
		if (mbp= allocb(HTCHI_INIT_LEN,BPRI_MED)) {
		    strcpy(mbp->b_wptr,HTCHI_MODE);
		    strcat(mbp->b_wptr,HTCHI_BINARY);
		    strcat(mbp->b_wptr,HTCHI_RESOLUTION);
		    strcat(mbp->b_wptr,HTCHI_PRESSURE);
		    strcat(mbp->b_wptr,HTCHI_RATE);
		    strcat(mbp->b_wptr,HTCHI_STREAM);
		    strcat(mbp->b_wptr,HTCHI_INCR);
		    strcat(mbp->b_wptr,HTCHI_EXECUTE);
		    mbp->b_wptr += strlen(mbp->b_wptr);
		    putnext( htchi->info.sInfo.wq, mbp );
		    return 1;
		}
	    }
	}
    }
    cmn_err(CE_WARN,"Couldn't allocate a streams buffer. hitachi tablet not initialized\n");
    return 0;
}

/*
 * This function decodes the Hitachi III 10 byte binary data packets
 * including pressure, height and tilt data.
 */
/* ARGSUSED */
static void
htchi_pressure_intr( idevInfo *pInfo, unsigned char *str, int len )
{
    htchi_state_t *htchi = (htchi_state_t *)pInfo;
    
    while (len--) {
        switch (htchi->state++) {
            case HITACHI_NOT_SYNCHED:
                if (*str & HITACHI_FLAG_BIT) {
                    htchi->state = HITACHI_HIGH_X;
		    /* Falls through to HITACHI_FLAGS */
                } else {
		    htchi->state = HITACHI_NOT_SYNCHED;
                    str++;
                    break;
                }
            case HITACHI_FLAGS:
                if (*str & HITACHI_FLAG_BIT) {
		    /* Good we passed that extra little check */
		    htchi->flags = *((struct hflags *)str);
		    htchi->vals.value[X_VAL] = (int)(htchi->flags.topx<<14);
		} else {
                    htchi->state = HITACHI_NOT_SYNCHED;
                }
                str++;
                break;

            case HITACHI_HIGH_X:
                htchi->vals.value[X_VAL] |= (int)(((*str++)&0x7f)<<7);
                break;

            case HITACHI_LOW_X:
                htchi->vals.value[X_VAL] |= (*str++)&0x7f;
                break;

	    case HITACHI_KEYS:
		/* Get the top Y bits */
		htchi->vals.value[Y_VAL] = (int)(((*str)&0x3)<<14);
		/* and the keys */
		htchi->keys = ((*str)&0x7c) >> 2;
                str++;
                break;

            case HITACHI_HIGH_Y:
                htchi->vals.value[Y_VAL] |= (int)(((*str++)&0x7f)<<7);
                break;

            case HITACHI_LOW_Y:
                htchi->vals.value[Y_VAL] |= (*str++)&0x7f;
		break;
		
	    case HITACHI_PRESSURE:
		htchi->vals.value[PRESSURE_VAL] = (*str++)&0x7f;
		break;
		
	    case HITACHI_HEIGHT:
		htchi->vals.value[HEIGHT_VAL] = (*str++)&0x7f;
		break;
		
	    case HITACHI_XTILT:
		htchi->vals.value[XTILT_VAL] = ((*str++)&0x7f);
		if (htchi->vals.value[XTILT_VAL] > 63)
		    htchi->vals.value[XTILT_VAL] -= 128;
		break;

	    case HITACHI_YTILT:
		htchi->vals.value[YTILT_VAL] = (char)((*str++)&0x7f);
		if (htchi->vals.value[YTILT_VAL] > 63)
		    htchi->vals.value[YTILT_VAL] -= 128;
		idevGenValEvents(&htchi->info,&htchi->vals,IDEV_VALS_ABSOLUTE);
		{
                    unsigned char mask = 0xf;
		    unsigned char newBtns;
		    if (htchi->flags.pick_up == 0x2) {
			/* four button puck */
			newBtns = (1 << htchi->keys) - 1;
		    } else {
			newBtns = htchi->keys & 0x2;
			newBtns |= (htchi->vals.value[PRESSURE_VAL] > htchi->btn_pressure);
		    }
		    if (newBtns != (htchi->bstate&0xf)) {
			idevGenBtnEvents(&htchi->info,&mask,&newBtns);
		    }
		}
		/*
		 * The packet contains additional info not used here
		 * so return to NOT_SYNCED state to throw rest of it
		 * away.
		 */
                htchi->state = HITACHI_FLAGS;
                break;
            default:
                cmn_err(CE_WARN,"Illegal hitachi state %d!!\n",htchi->state);
                htchi->state=HITACHI_NOT_SYNCHED;
                break;
        }
    }
    return;
}


/*
 * This function decodes the Hitachi III 7 or 10 byte binary data packets.
 * It ignores pressure, height and tilt data.
 */
/* ARGSUSED */
static void
htchi_intr( idevInfo *pInfo, unsigned char *str, int len )
{
    htchi_state_t *htchi = (htchi_state_t *)pInfo;
    
    while (len--) {
        switch (htchi->state++) {
            case HITACHI_NOT_SYNCHED:
                if (*str & HITACHI_FLAG_BIT) {
                    htchi->state = HITACHI_HIGH_X;
		    /* Falls through to HITACHI_FLAGS */
                } else {
		    htchi->state = HITACHI_NOT_SYNCHED;
                    str++;
                    break;
                }
            case HITACHI_FLAGS:
                if (*str & HITACHI_FLAG_BIT) {
		    /* Good we passed that extra little check */
		    htchi->flags = *((struct hflags *)str);
		    htchi->vals.value[X_VAL] = (int)(htchi->flags.topx<<14);
		} else {
                    htchi->state = HITACHI_NOT_SYNCHED;
                }
                str++;
                break;

            case HITACHI_HIGH_X:
                htchi->vals.value[X_VAL] |= (int)(((*str++)&0x7f)<<7);
                break;

            case HITACHI_LOW_X:
                htchi->vals.value[X_VAL] |= (*str++)&0x7f;
                break;

	    case HITACHI_KEYS:
		/* Get the top Y bits */
		htchi->vals.value[Y_VAL] = (int)(((*str)&0x3)<<14);
		/* and now the keys */
		htchi->keys = ((*str)&0x7c) >> 2;
                str++;
                break;

            case HITACHI_HIGH_Y:
                htchi->vals.value[Y_VAL] |= (int)(((*str++)&0x7f)<<7);
                break;

            case HITACHI_LOW_Y:
                htchi->vals.value[Y_VAL] |= (*str++)&0x7f;
		idevGenValEvents(&htchi->info,&htchi->vals,IDEV_VALS_ABSOLUTE);
		{
                    unsigned char mask = 0xf;
		    unsigned char newBtns;
		    if (htchi->flags.pick_up == 0x2) {
			/* four button puck */
			newBtns = (1 << htchi->keys) - 1;
		    } else {
			newBtns = htchi->keys & 0x3;
		    }
		    if (newBtns != (htchi->bstate&0xf)) {
			idevGenBtnEvents(&htchi->info,&mask,&newBtns);
		    }
		}
		/*
		 * The packet contains additional info not used here
		 * so return to NOT_SYNCED state to throw rest of it
		 * away.
		 */
                htchi->state = HITACHI_NOT_SYNCHED;
                break;
            default:
                cmn_err(CE_WARN,"Illegal hitachi state %d!!\n",htchi->state);
                htchi->state=HITACHI_NOT_SYNCHED;
                break;
        }
    }
    return;
}

/* ARGSUSED */
static int
htchi_other_control(htchi_state_t *tbt, idevOtherControl *pCtrl)
{

    pCtrl->name[IDEV_CTRL_NAME_LEN]=    '\0';
    pCtrl->data[IDEV_CTRL_DATA_LEN]=    '\0';

    if (strcmp(pCtrl->name, "pressure") == 0) {
	if (strcmp(pCtrl->data,"on") == 0) {
	    tbt->vactive = 0x3f;
	    tbt->vals.nValuators = HITACHI_NUM_VALUATORS;
	    tbt->info.vInfo.nVal = HITACHI_NUM_VALUATORS;
	    tbt->info.sInfo.readData = htchi_pressure_intr;
	} else {
	    tbt->vactive = 0x3;
	    tbt->vals.nValuators = HITACHI_NUM_AXES;
	    tbt->info.vInfo.nVal = HITACHI_NUM_AXES;
	    tbt->info.sInfo.readData = htchi_intr;
	}
	return 1;
    } else if (strcmp(pCtrl->name, "btnpressure") == 0) {
        if (pCtrl->data[0] <= tbt->vdesc[PRESSURE_VAL].hwMaxVal) {
            tbt->btn_pressure = pCtrl->data[0];
            return 1;
        }
    } else if (strncmp(pCtrl->name, "scale",5) == 0) {
	idevTransform *pTrans= NULL;
	if ((pCtrl->name[5]=='x') || (pCtrl->name[5]=='X'))
	    pTrans = &tbt->info.pInfo.xTransform;
	else if ((pCtrl->name[5] == 'y') || (pCtrl->name[5]=='Y'))
	    pTrans= &tbt->info.pInfo.yTransform;
	else if (isdigit(pCtrl->name[5])) {
	    unsigned n= atoi(&pCtrl->name[5]);
	    if (n<tbt->info.vInfo.nVal)
		pTrans= &tbt->info.vInfo.transform[n];
	}
	if ((pTrans!=NULL)&&(isdigit(pCtrl->data[0]))) {
	    int denom,num= atoi(pCtrl->data);
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
	/* deal with button mapping here */
    }

    return 0;
}

/* ARGSUSED */
static int
htchi_other_query(htchi_state_t *tbt, idevOtherQuery *pQuery)
{

    if (strcmp(pQuery->name, "pressure") == 0) {
	if (tbt->vactive == 0x3f)
	    strcpy(pQuery->data,"on");
	else
	    strcpy(pQuery->data,"off");
	return 1;
    } else if (strcmp(pQuery->name, "btnpressure") == 0) {
        sprintf(pQuery->data, "%d", tbt->btn_pressure);
	return 1;
    } else if (strncmp(pQuery->name, "scale",5) == 0) {
	idevTransform *pTrans= NULL;
	if ((pQuery->name[5]=='x') || (pQuery->name[5]=='X'))
	    pTrans = &tbt->info.pInfo.xTransform;
	else if ((pQuery->name[5] == 'y') || (pQuery->name[5]=='Y'))
	    pTrans= &tbt->info.pInfo.yTransform;
	else if (isdigit(pQuery->name[5])) {
	    unsigned n= atoi(&pQuery->name[5]);
	    if (n<tbt->info.vInfo.nVal)
		pTrans= &tbt->info.vInfo.transform[n];
	}
	if (pTrans!=NULL) {
	    sprintf(pQuery->data, "%d/%d",
		    pTrans->numerator, 
		    pTrans->denominator);
	    return 1;
	}
    } else if (strcmp(pQuery->name,"btnmap") == 0) {
	/* deal with button mapping here */
    }

    return 0;
}

        /*
         * HITACHI ioctl routine
         * Handles HITACHI ioctls, returns TRUE if the ioctl
         * was known and successful.  Returns FALSE 
         * otherwise.  Sets *pFound to TRUE if the ioctl
         * was known, FALSE otherwise.
         */
static int
htchi_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
htchi_state_t *htchi= (htchi_state_t *)pInfo;
int     ok = 0, found = 0;

        switch ( cmd ) {
                case IDEVGETDEVICEDESC:
                        found++;
                        if (size>=sizeof(idevDesc)) {
                            *((idevDesc *)stuff)= _htchi_desc;
                            ok= 1;
                        }
                        break;
                case IDEVGETVALUATORDESC:
                        found++;
                        if (size>=sizeof(idevGetSetValDesc))
                                ok = idevGetValDesc(&htchi->info,
                                        (idevGetSetValDesc*)stuff);
                        break;
                case IDEVPTRCONTROL:
                        found++;
                        if (size>=sizeof(idevPtrControl))
                                ok = idevSetPtrCtrl(&htchi->info,
                                        (idevPtrControl *)stuff);
                        break;
                case IDEVGETBUTTONS:
                        found++;
                        if (size>=idevSize(HITACHI_NUM_BUTTONS)) {
                                stuff[0]= htchi->bstate;
                                ok = 1;
                        }
                        break;
                case IDEVGETVALUATORS:
                        found++;
                        if (size>=sizeof(idevValuatorState))
                                ok = idevGetValState(&htchi->info,
                                                (idevValuatorState*)stuff);
                        break;
                case IDEVENABLEBUTTONS:
                        found++;
                        if (size>=sizeof(idevBitVals)) {
                                idevBitVals     *vals= (idevBitVals *)stuff;
                                htchi->bactive &= ~vals->mask[0];
                                htchi->bactive |= (vals->mask[0]&vals->value[0]);
                                ok= 1;
                        }
                        break;
                case IDEVENABLEVALUATORS:
                        found++;
                        if (size>=sizeof(idevBitVals)) {
                                idevBitVals     *vals= (idevBitVals *)stuff;
                                htchi->vactive &= ~vals->mask[0];
                                htchi->vactive |= (vals->mask[0]&vals->value[0]);
                                ok= 1;
                        }
                        break;
                case IDEVSETVALUATORS:
                        found++;
                        if (size>=sizeof(idevValuatorState))
                                ok = idevGenValEvents(&htchi->info,
                                        (idevValuatorState *)stuff,
                                        IDEV_NO_TRANSFORM|IDEV_VALS_ABSOLUTE|
                                        QE_RESPONSE);
                        break;
                case IDEVCHANGEVALUATORS:
                        found++;
                        if (size>=sizeof(idevValuatorState))
                                ok = idevGenValEvents(&htchi->info,
                                        (idevValuatorState *)stuff,
                                        IDEV_NO_TRANSFORM|QE_RESPONSE);
                        break;
                case IDEVSETVALUATORDESC:
                        found++;
                        if (size>=sizeof(idevGetSetValDesc)) {
                                ok = idevSetValDesc(&htchi->info,
                                        (idevGetSetValDesc *)stuff);
                        }
                        break;
                case IDEVSETPTRMODE:
                        found++;
                        if (size>=sizeof(idevPtrMode))
                                ok = idevSetPtrMode(&htchi->info,
                                        (idevPtrMode *)stuff);
                        break;
                case IDEVSETPTRBOUNDS:
                        found++;
                        if (size>=sizeof(idevPtrBounds))
                                ok = idevSetPtrBounds(&htchi->info,
                                        (idevPtrBounds *)stuff);
                        break;
                case IDEVSETPTR:
                        found++;
                        if (size>=sizeof(idevPtrVals))
                                ok = idevSetPtr(&htchi->info,
                                        (idevPtrVals *)stuff);
                        break;
                case IDEVSETTRANSFORM:
                        found++;
                        if (size>=sizeof(idevGetSetTransform))
                                ok = idevSetTransform(&htchi->info,
                                        (idevGetSetTransform *)stuff);
                        break;
                case IDEVGETTRANSFORM:
                        found++;
                        if (size>=sizeof(idevGetSetTransform))
                                ok = idevGetTransform(&htchi->info,
                                        (idevGetSetTransform *)stuff);
                        break;
                case IDEVOTHERCONTROL:
                        found++;
                        if (size>=sizeof(idevOtherControl))
                            ok= htchi_other_control(htchi,(idevOtherControl*)stuff);
	                else
			    ok = 0;
                        break;
                case IDEVOTHERQUERY:
                        found++;
                        if (size>=sizeof(idevOtherQuery))
                            ok= htchi_other_query(htchi,(idevOtherQuery*)stuff);
	                else
			    ok = 0;
                        break;
                case IDEVINITDEVICE:
                        found++;
                        ok= htchi_init_device(htchi);
                        break;
                default: /* send other msgs down */
                        break; /* FALL THROUGH */
        }
        *pFound=        found;
        return ok && found;
}

/* ARGSUSED */
static int
htchi_open( queue_t *rq, dev_t *dev, int flag, int sflag, struct cred *cred )
{
        register htchi_state_t *htchi;

        if ( sflag != MODOPEN )
                return ENXIO;

        if ( !rq->q_ptr ) {
                register int i;
                if ( !(htchi = (htchi_state_t *)kmem_alloc(sizeof *htchi, KM_SLEEP)))
                        return ENOMEM;

                /* Set defaults -- totally arbitrary, my choice */
                bzero( htchi, sizeof *htchi ) ;

                htchi->initialized=               1;
		/* The table seems to output a min pressure of 6 or 7 */
                htchi->btn_pressure=              15;
                htchi->vals.firstValuator=        0;
                htchi->vals.nValuators=           HITACHI_NUM_AXES;
		for (i=0; i<HITACHI_NUM_VALUATORS; i++) {
                        htchi->vdesc[i] = _htchi_dflt_val_desc;
                        htchi->vstate[i] = 0;
                        htchi->vtrans[i] = idevDfltScale;
                        htchi->vtrans[i].possible = IDEV_SCALE;
                        htchi->vtrans[i].which = IDEV_SCALE;
                }
                htchi->vdesc[Y_VAL].hwMinVal = HITACHI_Y_VALUATOR_MIN;
                htchi->vdesc[Y_VAL].minVal = HITACHI_Y_VALUATOR_MIN;
                htchi->vdesc[Y_VAL].hwMaxVal = HITACHI_Y_VALUATOR_MAX;
                htchi->vdesc[Y_VAL].maxVal = HITACHI_Y_VALUATOR_MAX;
		htchi->vdesc[PRESSURE_VAL] = _htchi_pressure_val_desc;
		htchi->vdesc[HEIGHT_VAL] = _htchi_pressure_val_desc;
		/*
		 * NOTE: In today's tablet, pressure and height have the same
		 * min and max values.  If they were different we should stuff
		 * vdesc[HEIGHT_VAL] with the defined values from the .h file.
		 */
		htchi->vdesc[XTILT_VAL] = _htchi_tilt_val_desc;
		htchi->vdesc[YTILT_VAL] = _htchi_tilt_val_desc;
		
                htchi->vactive =   HITACHI_ALL_VALUATORS;
                htchi->bstate =    0;
                htchi->bactive =   HITACHI_ALL_BUTTONS;

                htchi->info.bInfo.nBtn = HITACHI_NUM_BUTTONS;
                htchi->info.bInfo.active = &htchi->bactive;
                htchi->info.bInfo.state = &htchi->bstate;

                htchi->info.vInfo.nVal = HITACHI_NUM_AXES;
                htchi->info.vInfo.sysValue = htchi->vstate;
                htchi->info.vInfo.desc = htchi->vdesc;
                htchi->info.vInfo.transform = htchi->vtrans;
                htchi->info.vInfo.active = &htchi->vactive;
                htchi->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;

                htchi->info.pInfo.xAxis = htchi->info.pInfo.yAxis = 255;
                htchi->info.pInfo.minX = htchi->info.pInfo.minY = 0;
                htchi->info.pInfo.maxX = htchi->info.pInfo.maxY = 0;
                htchi->info.pInfo.x = htchi->info.pInfo.y= 0;
                htchi->info.pInfo.xTransform = idevDfltScale;
                htchi->info.pInfo.yTransform = idevDfltScale;

                htchi->info.sInfo.shmiqid.devminor = 0;
                htchi->info.sInfo.shmiqid.index = 0;
                htchi->info.sInfo.rq = rq;
                htchi->info.sInfo.wq =    WR(rq);
                htchi->info.sInfo.readData = htchi_intr;
                htchi->info.sInfo.writeIoctl = htchi_wioctl;
                WR(rq)->q_ptr = rq->q_ptr = (caddr_t) &htchi->info;
        }

        hitachiref++;

        return 0;
}

/* ARGSUSED */
static int
htchi_close( queue_t *rq, int flags, struct cred *cred )
{
        htchi_state_t *htchi = (htchi_state_t *)rq->q_ptr;

        kmem_free( (char *)htchi , sizeof *htchi);
        hitachiref--;
        return 0;
}

int
hitachiunload(void)
{
        return(hitachiref ? EBUSY : 0);
}

        /*
         * stream module definition
         */

static struct module_info htchi_mod_info = {
        0,                              /* module ID */
        HITACHI_NAME,                   /* module name */
        0,                              /* minimum packet size */
        INFPSZ,                         /* infinite maximum packet size */
        256,                            /* hi-water mark */
        16,                             /* lo-water mark */
};

static struct qinit htchi_rinit = {
        idev_rput, NULL, htchi_open, htchi_close,
        NULL, &htchi_mod_info, NULL
};

static struct qinit htchi_winit = {
        idev_wput, NULL, NULL, NULL,
        NULL, &htchi_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab hitachiinfo = {
        &htchi_rinit, &htchi_winit, 0, 0
} ;
