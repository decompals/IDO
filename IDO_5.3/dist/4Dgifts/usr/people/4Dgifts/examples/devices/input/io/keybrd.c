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
#include <string.h>

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
#include <sys/keybrd.h>

#include <sys/ddi.h>

#ifndef NULL
#define NULL 0
#endif

	/*
	 * Macros and variables specific to KBD 
	 * go here
	 */

static idevDesc _kbd_desc = {
	KBD_NAME,		/* devName */
	KBD_TYPE,		/* devType */
	KBD_NUM_KEYS,		/* nButtons */
	0,			/* nValuators */
	KBD_NUM_LEDS,		/* nLEDs */
	0,			/* nStrDpys */
	0,			/* nIntDpys */
	KBD_NUM_BELLS,		/* nBells */
	KBD_FLAGS		/* flags */
};

static struct termio _kbd_termio = {
	IGNBRK,		/* c_iflag */
	0,		/* c_oflag */
	B600|CS8|CREAD|PARENB|PARODD|HUPCL, /* c_cflag */
	0,			/* c_lflag */
	LDISC1,			/* c_line */
	/* VINTR VQUIT VERASE VKILL VEOF VMIN VTIME VSWTCH */
	{      0,    0,     0,    0,   0,   1,    0,     0  }
};

/* returned by config request */
#define CONFIG_BYTE_OLDKB	0xaa
#define CONFIG_BYTE_NEWKB	0x6e

/* config byte 0 bits */
#define KBD_BEEP		0x02
#define KBD_LONGBEEP		0x04
#define KBD_NOCLICK		0x08
#define KBD_GETCONFIG		0x10
#define KBD_NUMLOCK_LIGHT	0x20
#define KBD_CAPSLOCK_LIGHT	0x40
#define KBD_AUTOREPEAT		0x80
#define KBD_SCROLL_LOCK_LIGHT	0x04

#define KB_LED_OFFSET		3
#define KBD_LED_MAX		4
#define KB_LED_MASK		0x78

static void
autorepeat( kbd_state_t *kbd, int seqid )
{
        /* If any autorepeat is scheduled and we are on the
         *      sequence number Try to send the character:
         *      If it was delivered, continue to auto-repeat.
         */
        if ( seqid == kbd->seqid 
		&& (!(kbd->lastchar&KBD_UPSTROKE))
		&& idevGenBtnEvent( &kbd->info, kbd->lastchar,
					IDEV_BTN_PRESS|IDEV_FORCE_EVENT) ) {
                ++kbd->seqid;
		kbd->repeating= 1;
                kbd->reptid = STREAMS_TIMEOUT1((strtimeoutfunc_t)autorepeat,
					       kbd, kbd->rep_rate,
					       (void *)kbd->seqid);
	}
	else kbd->repeating= 0;
        return ;
}

static int
kbd_set_LEDs(kbd_state_t *kbd, register idevBitVals *LEDs)
{
	mblk_t *mbp;

	kbd->ledstate &= ~LEDs->mask[0];
	kbd->ledstate |= (LEDs->mask[0]&LEDs->value[0]);

	kbd->cntrl0 &= ~KB_LED_MASK;
	kbd->cntrl0 |= (kbd->ledstate<<KB_LED_OFFSET)&KB_LED_MASK;
	if ( mbp = allocb( 1, BPRI_MED ) ) {
		*mbp->b_wptr++=	kbd->cntrl0;
		putnext( kbd->info.sInfo.wq, mbp ) ;
		return 1;
        }
	cmn_err(CE_WARN,"Can't change keyboard LED (out of streams buffers).\n");
	return 0;
}

/* ARGSUSED */
static int
kbd_ring_bell(kbd_state_t *kbd, register idevBellSettings *bell)
{
	extern long lbolt;
	mblk_t *mbp;

	if ((bell->volume>=50)&&(lbolt!=kbd->last_bell)) {
		if ( mbp = allocb( 1, BPRI_MED ) ) {
			if (bell->duration>200)
				*mbp->b_wptr++= kbd->cntrl0|KBD_LONGBEEP;
			else
				*mbp->b_wptr++=	kbd->cntrl0|KBD_BEEP;
			putnext( kbd->info.sInfo.wq, mbp ) ;
			kbd->last_bell= lbolt;
			return 1;
		}
		if (lbolt-kbd->last_bell>100) {
		    cmn_err(CE_WARN,"Can't ring keyboard bell (out of streams buffers).\n");
		    kbd->last_bell= lbolt;
		}
	}
	return 0;
}

static int
kbd_keybd_control(kbd_state_t *kbd, idevKeybdControl *pCtrl)
{
mblk_t *mbp;

	if (pCtrl->which&IDEV_KC_SET_REPEAT) {
		kbd->rep_on = pCtrl->repeatOn;
	}
 	if (pCtrl->which&IDEV_KC_SET_KEY_REPEAT) {
		register int i;
		for (i=0;i<idevSize(KBD_NUM_KEYS);i++) {
			kbd->brepeatable[i]&=
				~pCtrl->keyRepeat.mask[i];
			kbd->brepeatable[i]|=
				(pCtrl->keyRepeat.mask[i]&
				 pCtrl->keyRepeat.value[i]);
		}
	}
	if (pCtrl->which&IDEV_KC_SET_CLICK_VOL) {
		if (pCtrl->clickVolume>49)
			kbd->cntrl0 &= ~KBD_NOCLICK;
		else	kbd->cntrl0 |= KBD_NOCLICK;
	}
	if (pCtrl->which&IDEV_KC_SET_LEDS) {
		kbd->ledstate = pCtrl->leds;

		kbd->cntrl1 &= ~KB_LED_MASK;
		kbd->cntrl1 |= (kbd->ledstate<<KB_LED_OFFSET)&KB_LED_MASK;
	}
	if (pCtrl->which&IDEV_KC_SET_KBD_LEDS) {
		if (pCtrl->kbdleds_mask&IDEV_KC_CAPS_LOCK) {
			if (pCtrl->kbdleds_value&IDEV_KC_CAPS_LOCK)
				kbd->cntrl0 |=  KBD_CAPSLOCK_LIGHT;
			else	kbd->cntrl0 &= ~KBD_CAPSLOCK_LIGHT;
		}
		if (pCtrl->kbdleds_mask&IDEV_KC_NUM_LOCK) {
			if (pCtrl->kbdleds_value&IDEV_KC_NUM_LOCK)
				kbd->cntrl0 |=  KBD_NUMLOCK_LIGHT;
			else	kbd->cntrl0 &= ~KBD_NUMLOCK_LIGHT;
		}
		if (pCtrl->kbdleds_mask&IDEV_KC_SCROLL_LOCK) {
			if (pCtrl->kbdleds_value&IDEV_KC_SCROLL_LOCK)
				kbd->cntrl1 |=  KBD_SCROLL_LOCK_LIGHT;
			else	kbd->cntrl1 &= ~KBD_SCROLL_LOCK_LIGHT;
		}
	}

	if ( mbp = allocb( 2, BPRI_MED ) ) {
		*mbp->b_wptr++=	kbd->cntrl0;
		*mbp->b_wptr++=	kbd->cntrl1;
		putnext( kbd->info.sInfo.wq, mbp ) ;
	}
	else cmn_err(CE_WARN, "Can't change keyboard settings (out of streams buffers).\n");

	if (pCtrl->which&IDEV_KC_SET_REP_DELAY) {
		/* s == 0 implies no autorepeat */
		if ( pCtrl->repeatDelay 
			&& pCtrl->repeatDelay < KBD_MIN_REPEAT_DELAY )
			kbd->rep_delay = KBD_MIN_REPEAT_DELAY;
		else
			kbd->rep_delay = pCtrl->repeatDelay;
	}
	if (pCtrl->which&IDEV_KC_SET_REP_RATE) {
		if ( pCtrl->repeatRate < KBD_MAX_REPEAT_RATE )
			kbd->rep_rate = KBD_MAX_REPEAT_RATE ;
		else
			kbd->rep_rate = pCtrl->repeatRate ;

	}
	return 0;
}

/* ARGSUSED */
static int
kbd_init_device(kbd_state_t *kbd, int size, char *stuff)
{
	mblk_t *mbp;

	if (idevChangeLineSettings(&kbd->info,&_kbd_termio)) {
		if ( mbp = allocb( 2, BPRI_MED ) ) {
			*mbp->b_wptr++=	kbd->cntrl0;
			*mbp->b_wptr++=	kbd->cntrl1;
			putnext( kbd->info.sInfo.wq, mbp ) ;
			return 1;
		}
        }
	else {
		cmn_err( CE_WARN, "Keyboard not initialized.\n");
	}
	return 0;
}

static void
killManager( register queue_t *who )
{
	register mblk_t *mbp ;

	/* Get a message buffer to send upstream */
	if ( !( mbp = allocb( 1, BPRI_HI ) ) ) {
		if ( !bufcall( 1, BPRI_HI, killManager, (unsigned long) who ) )
		    /* give up */
		    cmn_err( CE_WARN,
			     "killManager: out of buffers. manager lives!\n" ) ;
	}
	else {
		/* Use PCSIG so as to not be subject to flow control */
		mbp->b_datap->db_type = M_PCSIG ;
		*mbp->b_wptr++ = SIGKILL ;
		putnext( who, mbp ) ;
	}
	return ;
}

/* ARGSUSED */
static void
kbd_intr( idevInfo *pInfo, char *str, int len )
{
	register char c ;
	kbd_state_t *kbd = (kbd_state_t *)pInfo;
	unsigned char mask;
	unsigned char offset;
	int tmp;

	while ( len-- ) {
		register c = *str++;

		/* discard HW autorepeat as per Wiltse */
		if ( c == kbd->lastchar )
			continue;
		else
			kbd->lastchar = c;

		/* First stop any autorepeat in progress */
		/* We'll use "while" not "if" because there is a race here
		 * So long as we can "untimeout" faster than the repeat rate
		 * we'll always win the race.
		 */
		while ( tmp = kbd->reptid ) {
			kbd->reptid = 0;
			untimeout( tmp );
		}

		if ( c & KBD_UPSTROKE ) {
			idevGenBtnEvent( &kbd->info, c & ~KBD_UPSTROKE, 0 ) ;
		}
		else {
			kbd->lastchar = c;
			offset = idevOffset(c)&KBD_OFFSET_MASK;
			mask = idevMask(c);
			if ((tmp=idevGenBtnEvent(&kbd->info,c,IDEV_BTN_PRESS))
				&& kbd->rep_on && kbd->rep_delay
				&& (kbd->brepeatable[ offset ] & mask) ) {
				++kbd->seqid;
				kbd->repeating= 1;
				kbd->reptid = STREAMS_TIMEOUT1(
					(strtimeoutfunc_t)autorepeat, kbd,
					kbd->rep_delay, (void *)kbd->seqid);
			}
		}
#ifndef NO_SAFETY_CATCH_XXX
		/*
		 * Special safety catch to blow away a hung window server
		 *
		 * If the user hits LEFT-CTRL, LEFT-SHIFT, F12 and
		 * KEYPAD-/ simultaneously, blow away the owner of the
		 * active queue ( i.e. recieving focus ).
		 */
		if ( idevNth( kbd->bstate, 2 )		/* LEFT-CTRL */
		  && idevNth( kbd->bstate, 5 )		/* L-SHIFT */
		  && idevNth( kbd->bstate, 97 )		/* F12 */
		  && idevNth( kbd->bstate, 107 ) ) {	/* KEYPAD-SLASH */
			cmn_err( CE_CONT, "Killing the input queue owner\n" ) ;
			killManager( kbd->info.sInfo.rq ) ;
		}
#endif /* NO_SAFETY_CATCH_XXX */
	}
	return;
}

/* ARGSUSED */
static int
kbd_other_control(kbd_state_t *kbd, idevOtherControl *pCtrl)
{

	/* implement other device controls here */
	pCtrl->name[IDEV_CTRL_NAME_LEN]= '\0';
	pCtrl->data[IDEV_CTRL_DATA_LEN]= '\0';
	if (strcmp(pCtrl->name,"keymap")==0) {
	    strncpy(kbd->map,pCtrl->data,IDEV_KEYMAP_NAME_LEN);
	    kbd->map[IDEV_KEYMAP_NAME_LEN]= '\0';
	    return 1;
	}
	return 0;
}

/* ARGSUSED */
static int
kbd_other_query(kbd_state_t *kbd, idevOtherControl *pCtrl)
{

	/* implement other device queries here */
	pCtrl->name[IDEV_CTRL_NAME_LEN]= '\0';
	if ((strcmp(pCtrl->name,"keymap")==0)&&(kbd->map[0]!='\0')) {
	    strncpy(pCtrl->data,kbd->map,IDEV_CTRL_DATA_LEN);
	    pCtrl->data[IDEV_CTRL_DATA_LEN]= '\0';
	    return 1;
	}
	return 0;
}

#define NUM_MAPS	16
static char *maps[NUM_MAPS] = {
	"US",		"DE",		"FR",		"IT",
	"DK", 		"ES",  		"CH",		"SE",
	"GB",		"BE",		"NO",		"OO0B",
	"OO0C",		"OO0D",		"PT",		"OO0E"
};

static void
kbd_get_map_name( kbd_state_t *kbd )
{
extern unsigned char kbdtype;

	if (kbdtype<NUM_MAPS) {
		strcpy(kbd->map,maps[kbdtype]);
	}
	else {
		char	tmp;
		strcpy(kbd->map,"UNK");
		tmp= kbdtype&0xf;
		if (tmp>9)	kbd->map[3]='A'+tmp;
		else		kbd->map[3]='0'+tmp;
		tmp= (kbdtype>>4)&0xf;
		if (tmp>9)	kbd->map[4]='A'+tmp;
		else		kbd->map[4]='0'+tmp;
		kbd->map[5]= '\0';
	}
	return;
}


	/*
	 * KBD ioctl routine
	 * Handles KBD ioctls, returns TRUE if the ioctl
	 * was known and successful.  Returns FALSE 
	 * otherwise.  Sets *pFound to TRUE if the ioctl
	 * was known, FALSE otherwise.
	 */
static int
kbd_wioctl( idevInfo *pInfo, int cmd, int size, char *stuff, int *pFound )
{
kbd_state_t *kbd= (kbd_state_t *)pInfo;
int	ok = 0, found = 0;

	switch ( cmd ) {
		case IDEVGETDEVICEDESC:
			found++;
			if (size>=sizeof(idevDesc)) {
			    *((idevDesc *)stuff)= _kbd_desc;
			    ok= 1;
			}
			break;
		case IDEVGETKEYMAPDESC:
			found++;
			if (size>=sizeof(idevKeymapDesc)) {
				idevKeymapDesc *desc= (idevKeymapDesc *)stuff;
				if (!kbd->map[0])
					kbd_get_map_name( kbd );
				strncpy(desc->name,kbd->map,
							IDEV_KEYMAP_NAME_LEN);
				desc->name[IDEV_KEYMAP_NAME_LEN]= '\0';
				ok= 1;
			}
			break;
		case IDEVGETBUTTONS:
			found++;
			if (size>=idevSize(KBD_NUM_KEYS)) {
				bcopy(kbd->bstate,stuff,
						idevSize(KBD_NUM_KEYS));
				ok = 1;
			}
			break;
		case IDEVGETLEDS:
			found++;
			if (size>=idevSize(KBD_NUM_LEDS)) {
				bcopy(&kbd->ledstate,stuff,
						idevSize(KBD_NUM_LEDS));
				ok = 1;
			}
			break;
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= 1;
				idevUpdateBitArray(idevSize(KBD_NUM_KEYS),
							kbd->bactive,
							(idevBitVals *)stuff);
			}
			break;
		case IDEVSETLEDS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok = kbd_set_LEDs(kbd,(idevBitVals *)stuff);
			}
			break;
		case IDEVRINGBELL:
			found++;
			if (size>=sizeof(idevBellSettings))
				ok = kbd_ring_bell(kbd,
					(idevBellSettings *)stuff);
			break;
		case IDEVKEYBDCONTROL:
			found++;
			if (size>=sizeof(idevKeybdControl))
				ok = kbd_keybd_control(kbd,
					(idevKeybdControl *)stuff);
			break;
		case IDEVOTHERQUERY:
			found++;
			if (size==sizeof(idevOtherQuery)) {
				ok = kbd_other_query(kbd,
					(idevOtherQuery *)stuff);
			}
			else ok = 0;
			break;
		case IDEVOTHERCONTROL:
			found++;
			if (size==sizeof(idevOtherControl)) {
				ok = kbd_other_control(kbd,
					(idevOtherControl *)stuff);
			}
			else ok = 0;
			break;
		case IDEVINITDEVICE:
			found++;
			ok = kbd_init_device(kbd,size,stuff);
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
kbd_open( queue_t *rq, dev_t *dev, int flag, int sflag, struct cred *cred )
{
	register kbd_state_t *kbd;
	register int i;

	if ( sflag != MODOPEN )
		return ENXIO;

	if ( !rq->q_ptr ) {
		if ( !(kbd = (kbd_state_t *)kmem_alloc(sizeof *kbd, KM_SLEEP)))
			return ENOMEM;

		/* Set defaults -- totally arbitrary, my choice */
		bzero( kbd, sizeof *kbd ) ;

		kbd->initialized = 1;
		kbd->rep_delay = KBD_DFLT_REPEAT_DELAY;
		kbd->rep_rate = KBD_DFLT_REPEAT_RATE;
		kbd->rep_on = 	1;
		kbd->cntrl0=	0;
		kbd->cntrl1=	1;
		kbd->last_bell=	0;

		for (i=idevSize(KBD_NUM_KEYS)-1;i>=0;i--) {
			kbd->brepeatable[i] = ~0;
			kbd->bactive[i] = ~0;
			kbd->bstate[i] = 0;
		}

		kbd->info.bInfo.nBtn = KBD_NUM_KEYS;
		kbd->info.bInfo.active = kbd->bactive;
		kbd->info.bInfo.state = kbd->bstate;

		kbd->ledstate = 0;
		kbd->repeating= 0;

		kbd->info.sInfo.shmiqid.devminor = 0;
		kbd->info.sInfo.shmiqid.index = 0;
		kbd->info.sInfo.rq = rq;
		kbd->info.sInfo.wq =	WR(rq);
		kbd->info.sInfo.readData = kbd_intr;
		kbd->info.sInfo.writeIoctl = kbd_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &kbd->info;
	}

	return 0;
}

/* ARGSUSED */
static int
kbd_close( queue_t *rq, int flags, struct cred *cred )
{
	kbd_state_t *kbd = (kbd_state_t *)rq->q_ptr;

	ASSERT( rq->q_ptr );
	if (kbd->repeating) {
	    untimeout(kbd->reptid);
	    kbd->repeating= 0;
	}
	kmem_free( (char *) kbd , sizeof *kbd);
	return 0;
}

	/*
	 * stream module definition
	 */

static struct module_info kbd_mod_info = {
	0,				/* module ID */
	KBD_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit kbd_rinit = {
	idev_rput, NULL, kbd_open, kbd_close,
	NULL, &kbd_mod_info, NULL
};

static struct qinit kbd_winit = {
	idev_wput, NULL, NULL, NULL,
	NULL, &kbd_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab keyboardinfo = {
	&kbd_rinit, &kbd_winit, 0, 0
} ;

int keyboarddevflag = 0;
