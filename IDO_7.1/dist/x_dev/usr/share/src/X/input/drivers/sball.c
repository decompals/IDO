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
#ident "$Revision: 1.16 $"

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

#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h"
#define	_KERNEL
#endif /* INPUT_TEST */
#include <string.h>

/* my stuff */
#include <sys/shmiq.h>
#include <sys/idev.h>
#include <sys/sball.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to SB 
	 * go here
	 */

static idevDesc _sb_desc = {
	SB_NAME,		/* devName */
	SB_TYPE,		/* devType */
	SB_NUM_BUTTONS,		/* nButtons */
	SB_NUM_VALUATORS,	/* nValuators */
	0,			/* nLEDs */
	0,			/* nStrDpys */
	0,			/* nIntDpys */
	SB_NUM_BELLS,		/* nBells */
	0			/* flags */
};

static idevValuatorDesc _sb_dflt_val_desc = {
	SB_VALUATOR_MIN_RESOLUTION,	/* hwMinRes */
	SB_VALUATOR_MAX_RESOLUTION,	/* hwMaxRes */
	SB_VALUATOR_MIN,		/* hwMinVal */
	SB_VALUATOR_MAX,		/* hwMaxVal */
	IDEV_ABSOLUTE,			/* possibleModes */
	IDEV_ABSOLUTE,			/* mode */
	SB_VALUATOR_RESOLUTION,		/* resolution */
	SB_VALUATOR_MIN,		/* minVal */
	SB_VALUATOR_MAX			/* maxVal */
};

static int sballref = 0;		/* reference count for unload */
char *sballmversion = M_VERSION;
int sballdevflag = 0;

#define	SB_VALS_ABSOLUTE	(0x0100)
#define	SB_VALS_RELATIVE	(0x0200)
#define	SB_GENERATE_ALL		(0x0400)

#define	SB_VERSION_PREFIX "HvFirmware version "
#define	SB_DATE_PREFIX "created on "

static int
sb_ring_bell(sb_state_t *sb,register idevBellSettings *bell)
{
mblk_t			*mbp;
int			 duration= (bell->duration*32)/1000;

    if		(duration<0)	duration=	1;
    else if	(duration&0x1f)	duration=	0x1f;

    if (mbp= allocb(3,BPRI_MED)) {
	mbp->b_wptr[0]=	'B';
	mbp->b_wptr[1]=	(0x20)|duration;
	mbp->b_wptr[2]= '\015';
	mbp->b_wptr+=	3;
	putnext( sb->info.sInfo.wq, mbp );
    }
    else {
	cmn_err(CE_WARN,"sball: Couldn't allocate streams buffer. Spaceball bell not rung.\n");
    }
    return 1;
}

#define	SB_RESET_CMD	"@reset\015"
sb_issue_reset(sb_state_t *sb,int send_reset)
{
mblk_t			*mbp;

    if (mbp= allocb(sizeof(SB_RESET_CMD),BPRI_MED)) {
	if (send_reset)	{
		strcpy((char *)mbp->b_wptr,SB_RESET_CMD);
		mbp->b_wptr+= sizeof(SB_RESET_CMD)-1;
	}
	else {
		strcpy((char *)mbp->b_wptr,"\n");
		mbp->b_wptr+=	1;
	}
	putnext( sb->info.sInfo.wq, mbp );
    }
    else {
	cmn_err(CE_WARN,"sball: Couldn't allocate streams buffer.  Spaceball not Reset.\n");
    }
    return 1;
}


/***====================================================================***/
/***                 SPACEBALL INPUT STREAM PARSERS                     ***/
/***====================================================================***/


static int
sb_ParsePrintableWords(int len,char *str,int *period,int *rtrn)
{
unsigned i,val6,byteVal;
unsigned spill=0,spillLen;
int	packed;
short	nBytes,nWords,lastByte;

    nWords= nBytes= packed= 0;
    for (i=0;i<len;i++) {
	if (!packed) {
	    if (str[i]!='^')  {
		nBytes++;
		if (nBytes&0x1)	lastByte=	(str[i]&0xff)<<8;
		else if (nWords==0) {
		    *period= (short)(lastByte|(str[i]&0xff));
		}
		else if (nWords<6) {
		    rtrn[nWords-1]= (short)(lastByte|(str[i]&0xff));
		}
		nWords++;
	    }
	    else {
		packed=	1;
		spillLen= 0;
	    }
	}
	else {
	    if (str[i]&0x40)		val6=	(str[i]&0x3F);
	    else if (str[i]==0x3F)	val6=	0x3F;
	    else {
		cmn_err(CE_WARN,"sball: unexpected byte 0x%x in packed mode\n",str[i]);
		return(0);
	    }
	    spill<<= 6;
	    spill|= (val6&0x3f);
	    spillLen+= 6;
	    if (spillLen>=8) {
		byteVal= (spill>>(spillLen%8))&0xff;
		nBytes++;
		if (nBytes&0x1)	lastByte=	(byteVal&0xff)<<8;
		else if (nWords==0) {
		    *period= (short)(lastByte|(byteVal&0xff));
		}
		else if (nWords<7) {
		    rtrn[nWords-1]= (short)(lastByte|(byteVal&0xff));
		}
		nWords++;
		spillLen-= 8;
	    }
	}
    }
    return(nWords);
}

/***====================================================================***/

static int
sb_ParseBinaryWords(int	len,char *str,int *period,int *rtrn)
{
unsigned i,byteVal;
short	nBytes,nWords,lastByte;

    nWords= nBytes= 0;
    for (i=0;i<len;i++) {
	if (str[i]=='^') {
	    if (i==len-1) {
		cmn_err(CE_WARN,"sball: Error! binary mode string ends in '^'\n");
		return(0);
	    }
	    i++;
	    switch (str[i]) {
		case 'Q':
		case 'S': 
		case 'M': byteVal= str[i]-'A'+1; break;
		case '^': byteVal= '^'; break;
		default:  cmn_err(CE_WARN,"sball: illegal binary sequence \"^%c\"\n",
								str[i]);
			  return(0);
	    }
	}
	else	byteVal= str[i];
	nBytes++;
	if (nBytes&1)	lastByte=	(byteVal&0xff)<<8;
	else {
	    if (nWords==0) {
		 *period=	(short)(lastByte|(byteVal&0xff));
	    }
	    else if (nWords<7) {
		 rtrn[nWords-1]=	(short)(lastByte|(byteVal&0xff));
	    }
	    nWords++;
	}
    }
    return(nWords-1);
}

/***====================================================================***/

void
sb_GenNormalEvent(sb_state_t *sb,int len,char *str)
{
idevValuatorState	vals;
int	period;
int	n;

    if (!sb->parseData) {
	cmn_err(CE_WARN,"sball: data event with unknown format.\n");
	sb_issue_reset(sb,1);
	sb->parseData= sb_ParsePrintableWords;
    }
    n= (*sb->parseData)(len,str,&period,&vals.value[0]);
    if ((n!=3)&&(n!=6)) {
	cmn_err(CE_WARN,"sball: Bad length for data event (%d words)\n",n);
	sb_issue_reset(sb,0);
	return;
    }

    if (sb->periodMode==SB_GEN_PERIOD) {
	idevGenValEvent( &sb->info, 6, period, 
					SB_VALS_ABSOLUTE|IDEV_FORCE_EVENT );
    }
    else if (sb->periodMode==SB_PERIOD_PREMULT) {
	register int i;
	for (i=0;i<n;i++) {
	    vals.value[i]*= period;
	}
    }


    vals.firstValuator= 0;
    vals.nValuators= n;
    idevGenValEvents( &sb->info, &vals, SB_VALS_ABSOLUTE|IDEV_FORCE_EVENT );
    return;
}

/***====================================================================***/

void
sb_GenDFilterEvent(sb_state_t *sb,int len,char *str)
{
register int nVals;
int	period;
int	values[6];

    if (!sb->parseData) {
	cmn_err(CE_WARN,"sball: data event with unknown format.\n");
	sb_issue_reset(sb,1);
	sb->parseData= sb_ParsePrintableWords;
    }
    nVals=	(*sb->parseData)(len,str,&period,values);

    if ((nVals!=3)&&(nVals!=6)) {
	cmn_err(CE_WARN,"sball: Bad length for data event (%d words)\n",nVals);
	sb_issue_reset(sb,0);
	return;
    }

    if (nVals>0) {
	register int i,maxAxis,maxVal;
	maxAxis= nVals-1;
	maxVal=	 (values[maxAxis]>=0?values[maxAxis]:-values[maxAxis]);
	for (i=maxAxis-1;i>=0;i--) {
	    if ((values[i]>=0)&&(values[i]>maxVal)) {
		maxVal= values[i];
		maxAxis= i;
	    }
	    else if ((values[i]<0)&&(-values[i]>maxVal)) {
		maxVal= -values[i];
		maxAxis= i;
	    }
        }

	if (sb->periodMode==SB_GEN_PERIOD) {
	    idevGenValEvent( &sb->info, 6, period, 
					SB_VALS_ABSOLUTE|IDEV_FORCE_EVENT );
	}
	else if (sb->periodMode==SB_PERIOD_PREMULT) {
	    values[maxAxis]*= period;
	}
	idevGenValEvent( &sb->info, maxAxis,  values[maxAxis],
					SB_VALS_ABSOLUTE|IDEV_FORCE_EVENT );
    }
    return;
}

#define	SB_MODE_STR		"\015CB\015"
#define	SB_MODE_STR_LEN		sizeof(SB_MODE_STR)
#define	SB_SENSITIVITY		"NT\015FT?\015FR?\015"
#define	SB_SENSITIVITY_LEN 	sizeof(SB_SENSITIVITY)
#define SB_DATA_RATE		"P@r@r\015"
#define SB_DATA_RATE_LEN 	sizeof(SB_DATA_RATE)
#define	SB_EVENT_TYPE		"MSSV\015"
#define	SB_EVENT_TYPE_LEN 	sizeof(SB_EVENT_TYPE)
#define	SB_REZERO		"Z\015"
#define	SB_REZERO_LEN		sizeof(SB_REZERO)
#define	SB_BELL			"BcCc\015"
#define	SB_BELL_LEN		sizeof(SB_BELL)
#define	SB_VERSION		"hv\015"
#define	SB_VERSION_LEN		sizeof(SB_VERSION)

#define SEND_SBSTRING(s,len)  \
    if (mbp= allocb(len,BPRI_MED)) {\
	strcpy((char *)mbp->b_wptr,(const char *)s);\
	mbp->b_wptr+= strlen((char *)mbp->b_wptr);\
	putnext( sb->info.sInfo.wq, mbp );\
    }\
    else {\
	cmn_err(CE_WARN,\
	"sball: Couldn't allocate streams buffer -- spaceball not initialized.\n");\
    }

void
sb_Reset(sb_state_t *sb)
{
mblk_t			*mbp;


    SEND_SBSTRING(SB_MODE_STR   ,SB_MODE_STR_LEN);
    SEND_SBSTRING(SB_EVENT_TYPE ,SB_EVENT_TYPE_LEN);
    SEND_SBSTRING(SB_SENSITIVITY,SB_SENSITIVITY_LEN);
    SEND_SBSTRING(SB_DATA_RATE  ,SB_DATA_RATE_LEN);
    SEND_SBSTRING(SB_REZERO     ,SB_REZERO_LEN);
    SEND_SBSTRING(SB_BELL       ,SB_BELL_LEN);
    SEND_SBSTRING(SB_VERSION    ,SB_VERSION_LEN);
    return;
}


void
sb_ParseCmd(sb_state_t *sb)
{
register char	*str= sb->msg;
register int	 len= sb->msgLen;


    switch(str[0]) {
	case 'C': 
		if (len>1) {
		    if ((str[1]=='B')||(str[1]=='b'))
			 sb->parseData=	sb_ParseBinaryWords;
		    else if ((str[1]=='P')||(str[1]=='p')) 
			 sb->parseData=	sb_ParsePrintableWords;
		    else { 
			cmn_err(CE_WARN,"sball: Unknown communications mode %c\n",
								str[1]);
			sb_issue_reset(sb,1);
		    }
		}
		break;
	case 'K':
		if (len==3) {
		    unsigned char mask[2];
		    unsigned char value[2];
		    mask[0]= 0xff; mask[1]= 0x1;
		    value[0]= ((str[1]&0xf)<<4)|(str[2]&0xf);
		    value[1]= (str[1]&0x10)?1:0;
		    idevGenBtnEvents(&sb->info,mask,value);
		}
		else {
		    cmn_err(CE_WARN,"sball: Illegal key event ignored.\n");
		    sb_issue_reset(sb,1);
		}
		break;
	case 'D':
		if (sb->genValEvent)
			(*sb->genValEvent)(sb,len-1,&str[1]);
		break;
	case 'M':
		if (len>1)	sb->transMode=	str[1];
		if (len>2)	sb->rotMode=	str[2];
		if (len>3)	sb->rotForm=	str[3];
		break;
	case 'S':
		if (len>1)	sb->spinExp=		str[1]&0x3f;
		if (len>2)	sb->spinMantissa=	str[1]&0x3f;
		break;
	case 'Z':
		strcpy(sb->zero,&str[1]);
		break;
	case 'E':
		cmn_err(CE_WARN,"sball: Error \"%s\" occured\n",&str[1]);
		break;

	case 'H':
		{
			register int i;

			if (len<sizeof(SB_VERSION_PREFIX)-1)
				return;
			for (i=0;i<sizeof(SB_VERSION_PREFIX)-1;i++,len--) {
				if (*str++!=SB_VERSION_PREFIX[i])
					return;
			}
			for (i=0;(i<SB_MAX_VERSION_LEN-1)&&(len>0)&&(*str!=' ');
								i++,len--) {
				sb->version[i]= *str++;
			}
			sb->version[i]= '\0';
			str++;len--;
			if (len<sizeof(SB_DATE_PREFIX)-1)
				return;
			for (i=0;i<sizeof(SB_DATE_PREFIX)-1;i++,len--) {
				if (*str!=SB_DATE_PREFIX[i]) {
					return;
				}
				str++;
			}
			for (i=0;(i<SB_MAX_DATE_LEN-1)&&(len>0)&&(*str!=' ');
								i++,len--) {
				sb->date[i]= *str++;
			}
			sb->date[i]= '\0';
		}
		break;
	case '@':
		if ((len>1)&&(str[1]=='2'))
			sb_Reset(sb);
		break;
	case '?':
		cmn_err(CE_WARN,"sball: Error message \"%s\" ignored\n",&str[1]);
		break;
#ifdef NOTDEF
	case 'B': case 'b':
		/* do nothing for bell events */
		break;
	case 'P': case 'p':
		/* do nothing for poll events */
		break;
	case 'A': case 'a':
		/* do nothing for absolute rotation matrix events */
		break;
	case 'O': case 'o':
		/* do nothing for orientation matrix events */
		break;
	case 'L': case 'l':
		/* do nothing for coordinate systems events */
		break;
	case 'T': case 't':
		/* do nothing for translation freedom events */
		break;
	case 'R': case 'r':
		/* do nothing for rotation freedom events */
		break;
	case 'N': case 'n':
		/* do nothing for null sphere events */
		break;
	case 'Q': case 'q':
		/* do nothing for set translation quality events */
		break;
	default:
		cmn_err(CE_WARN,"sball: Unknown packet \"%c...\" ignored\n",str[0]);
		break;
#endif /* NOTDEF */
    }
    return;
}

/* ARGSUSED */
static int
sb_other_query(sb_state_t *sb, idevOtherQuery *pCtrl)
{
    pCtrl->name[IDEV_CTRL_NAME_LEN]=	'\0';
    if (strcmp(pCtrl->name,"sbversion")==0) {
	strcpy(pCtrl->data,sb->version);
	strcat(pCtrl->data," ");
	strcat(pCtrl->data,sb->date);
    }
    else return 0;
    return 1;
}

/* ARGSUSED */
static int
sb_other_control(sb_state_t *sb, idevOtherControl *pCtrl)
{
mblk_t			*mbp;

    pCtrl->name[IDEV_CTRL_NAME_LEN]=	'\0';
    pCtrl->data[IDEV_CTRL_DATA_LEN]=	'\0';

    if (strcmp(pCtrl->name,"sbprivate")==0) {
	int size= strlen(pCtrl->data);

	if (mbp= allocb(size,BPRI_MED)) {
	    strcpy((char *)mbp->b_wptr,pCtrl->data);
	    mbp->b_wptr+= strlen((char *)mbp->b_wptr);
	    putnext( sb->info.sInfo.wq, mbp );
	}
	else {
	    cmn_err(CE_WARN,"sball: Couldn't allocate streams buffer -- spaceball control ignored.\n");
	}
    }
    else if (strcmp(pCtrl->name,"sbperiod")==0) {
	if (strcmp(pCtrl->data,"off")==0)
	     sb->periodMode= SB_NO_PERIOD;
	else if (strcmp(pCtrl->data,"premult")==0)
	     sb->periodMode= SB_PERIOD_PREMULT;
	else sb->periodMode= SB_GEN_PERIOD;
    }
    else if (strcmp(pCtrl->name,"sbdfilter")==0) {
	if (strcmp(pCtrl->data,"off")==0) 
	     sb->genValEvent= sb_GenDFilterEvent;
	else sb->genValEvent= sb_GenNormalEvent;
    }
    else return 0;
    return 1;
}

static void
sb_intr( idevInfo *pInfo, unsigned char *str, int len )
{
sb_state_t	*sb = (sb_state_t *)pInfo;
int	i;

    for (i=0;i<len;i++,str++) {
	if (*str=='\015') {
	    sb->msg[sb->msgLen]= '\0';
	    if ((!sb->error)&&(sb->msgLen>0))	sb_ParseCmd(sb);
	    else				sb->error=	0;
	    sb->msgLen=	0;
	}
	else {
	    if ((sb->msgLen!=0)||((*str)!='\012'))
		sb->msg[sb->msgLen++]=	*str;
	    if (sb->msgLen>=SB_MAX_MSG_LEN) {
		sb->error= 1;
		sb->msgLen= 0;
	    }
	}
    }
    return;
}

	/*
	 * SB ioctl routine
	 * Handles SB ioctls, returns TRUE if the ioctl
	 * was known and successful.  Returns FALSE 
	 * otherwise.  Sets *pFound to TRUE if the ioctl
	 * was known, FALSE otherwise.
	 */
static int
sb_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
sb_state_t *sb= (sb_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _sb_desc;
			    ok= 1;
			}
			break;
		case IDEVGETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevGetValDesc(&sb->info,
					(idevGetSetValDesc*)stuff);
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(SB_NUM_BUTTONS)) {
				bcopy(sb->bstate,stuff,
						idevSize(SB_NUM_BUTTONS));
				ok = 1;
			}
			break;
		case IDEVGETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGetValState(&sb->info,
						(idevValuatorState*)stuff);
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= 1;
				idevUpdateBitArray(idevSize(SB_NUM_BUTTONS),
							(char *)sb->bactive,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVENABLEVALUATORS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= 1;
				idevUpdateBitArray(idevSize(SB_NUM_VALUATORS),
							(char *)&sb->vactive,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVSETVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&sb->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|IDEV_VALS_ABSOLUTE
					|QE_RESPONSE);
			break;
		case IDEVCHANGEVALUATORS:
			found++;
			if (size>=sizeof(idevValuatorState))
				ok = idevGenValEvents(&sb->info,
					(idevValuatorState *)stuff,
					IDEV_NO_TRANSFORM|QE_RESPONSE);
			break;
		case IDEVSETVALUATORDESC:
			found++;
			if (size>=sizeof(idevGetSetValDesc))
				ok = idevSetValDesc(&sb->info,
					(idevGetSetValDesc *)stuff);
			break;
		case IDEVRINGBELL:
			found++;
			if (size>=sizeof(idevBellSettings))
				ok = sb_ring_bell(sb,
					(idevBellSettings *)stuff);
			break;
		case IDEVSETPTRMODE:
			found++;
			if (size>=sizeof(idevPtrMode))
				ok = idevSetPtrMode(&sb->info,
					(idevPtrMode *)stuff);
			break;
		case IDEVSETPTRBOUNDS:
			found++;
			if (size>=sizeof(idevPtrBounds))
				ok = idevSetPtrBounds(&sb->info,
					(idevPtrBounds *)stuff);
			break;
		case IDEVSETPTR:
			found++;
			if (size>=sizeof(idevPtrVals))
				ok = idevSetPtr(&sb->info,
					(idevPtrVals *)stuff);
			break;
		case IDEVSETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevSetTransform(&sb->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVGETTRANSFORM:
			found++;
			if (size>=sizeof(idevGetSetTransform))
				ok = idevGetTransform(&sb->info,
					(idevGetSetTransform *)stuff);
			break;
		case IDEVOTHERQUERY:
			found++;
			if (size>=sizeof(idevOtherQuery)) {
			    ok= sb_other_query(sb,(idevOtherQuery *)stuff);
			}
			else ok = 0;
			break;
		case IDEVOTHERCONTROL:
			found++;
			if (size>=sizeof(idevOtherControl)) {
			    ok= sb_other_control(sb,(idevOtherControl *)stuff);
			}
			else ok = 0;
			break;
		case IDEVINITDEVICE:
			found++;
			ok = idevChangeLineSettings(&sb->info,NULL);
			if (ok)
				sb_Reset(sb);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/*ARGSUSED*/
static int
sb_open( queue_t *rq, dev_t *dev, int flag, int sflag, struct cred *cred )
{
	register sb_state_t *sb;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO ;

	if ( !rq->q_ptr ) {
		if ( !(sb = (sb_state_t *)kmem_alloc(sizeof *sb, KM_SLEEP)))
			return ENOMEM ;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( sb, sizeof *sb ) ;

		sb->initialized=	1;
		sb->error=		0;
		sb->eventMode=		SB_NORMAL_MODE;
		sb->periodMode=		SB_GEN_PERIOD;
		sb->genValEvent=	sb_GenNormalEvent;
		strcpy(sb->version,"0.00");
		strcpy(sb->date,"06-Dec-0000");

		for (i=0;i<SB_NUM_VALUATORS;i++) {
			sb->vdesc[i]= _sb_dflt_val_desc;
			sb->vstate[i]= 0;
			sb->vtrans[i] = idevDfltAccel;
			sb->vtrans[i].possible = IDEV_ACCEL|IDEV_SCALE;
			sb->vtrans[i].which = 0;
		}
		sb->vactive=	SB_VALUATOR_MASK;
		sb->bstate[0]=	sb->bstate[1]=	0;
		sb->bactive[0]=	SB_BUTTON_MASK&0xff;
		sb->bactive[1]=	(SB_BUTTON_MASK>>8)&0xff;

		sb->info.bInfo.nBtn = SB_NUM_BUTTONS;
		sb->info.bInfo.active = sb->bactive;
		sb->info.bInfo.state = sb->bstate;

		sb->info.vInfo.nVal = SB_NUM_VALUATORS;
		sb->info.vInfo.sysValue = sb->vstate;
		sb->info.vInfo.desc = sb->vdesc;
		sb->info.vInfo.transform = sb->vtrans;
		sb->info.vInfo.active = &sb->vactive;
		sb->info.vInfo.mode = IDEV_GEN_NON_PTR_EVENTS;
		sb->info.pInfo.xTransform = idevDfltAccel;
		sb->info.pInfo.yTransform = idevDfltAccel;

		sb->info.pInfo.xAxis = sb->info.pInfo.yAxis = 255;
		sb->info.pInfo.minX = sb->info.pInfo.minY = 0;
		sb->info.pInfo.maxX = sb->info.pInfo.maxY = 0;
		sb->info.pInfo.x = sb->info.pInfo.y= 0;

		sb->info.sInfo.shmiqid.devminor = 0;
		sb->info.sInfo.shmiqid.index = 0;
		sb->info.sInfo.rq = rq;
		sb->info.sInfo.wq =	WR(rq);
		sb->info.sInfo.readData = (void (*)())sb_intr;
		sb->info.sInfo.writeIoctl = sb_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &sb->info;
	}

	sballref++;

	return 0;
}

/*ARGSUSED*/
static int
sb_close( queue_t *rq, int flag, struct cred *cred )
{
	sb_state_t *sb = (sb_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	kmem_free( (char *)sb , sizeof *sb);
	sballref--;
	return 0;
}

int
sballunload(void)
{
	return(sballref ? EBUSY : 0);
}

	/*
	 * stream module definition
	 */

static struct module_info sb_mod_info = {
	0,				/* module ID */
	SB_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit sb_rinit = {
	idev_rput, NULL, sb_open, sb_close,
	NULL, &sb_mod_info, NULL
};

static struct qinit sb_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &sb_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab sballinfo = {
	&sb_rinit, &sb_winit, 0, 0
} ;
