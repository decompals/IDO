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

#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/termio.h>
#include <sys/debug.h>
#include <sys/cpu.h>
#include <string.h>

/* Streams stuff */
#include <sys/stream.h>
#include <sys/strmp.h>

#ifdef INPUT_TEST
#include <stdio.h>
#include "inputtest.h"
#define	_KERNEL
#endif /* INPUT_TEST */

/* LBOOT_SGI_PC	- lboot switchable between SGI and PC
 * SGI_ONLY	- only emulate SGI keyboard hack
 * PC_ONLY	- only do the right thing
 */
#define PC_ONLY	

#if defined(SGI_ONLY) || defined(LBOOT_SGI_PC)
#define SGI_NBUTTONS	128
#endif
#if defined(SGI_ONLY)		/* redefine KBD_NUM_KEYS to save tbl space */
#define KBD_NUM_KEYS	SGI_NBUTTONS
#endif

/* input stuff */
#include "sys/shmiq.h"
#include "sys/idev.h"
#include "sys/pckbd.h"
#include "sys/pckm.h"

#include "sys/ddi.h"
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
#if defined (PC_ONLY) || defined(LBOOT_SGI_PC)
	KBD_FLAGS		/* flags */
#else
	IDEV_HAS_KEYMAP
#endif
};

/*
 * some stuff for NDS
 */
extern int kbd_index;
extern int kbd_minor;
extern int bogus_kbd_minor;
extern int bogus_kbd_index;


#if defined(SGI_ONLY) || defined(LBOOT_SGI_PC)
/*  In the 5.0S release the kernel maps PC scan codes to SGI scan
 * codes in order to maintain strict binary compatability since some
 * applications are dependent on scan codes instead of using X11's
 * translation facilities.  The next major release of IRIX will pass
 * PC scan codes unaltered thereby giving scan code dependent apps
 * time to be ported.
 *
 *  pc2gi_map[] is a mapping from standard layout the 101 and 102 PC
 * keyboards to SGI scan codes.  SGI = pc2sgi_map[PC];
 */

char pc2sgi_map[255] = {
/*0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F		 */
  0,  0,  0,  0,  0,  0,  0, 86,  6,  0,  0,  0,  0,  8, 54, 87, /* 0x00 */
  0,  2,  5,110,  3,  9,  7, 88,  0, 83, 19, 11, 10, 15, 13, 89, /* 0x10 */
  0, 27, 20, 17, 16, 21, 14, 90,  0, 82, 28, 18, 24, 23, 22, 91, /* 0x20 */
  0, 36, 35, 26, 25, 31, 29, 92,  0, 84, 43, 33, 32, 30, 37, 93, /* 0x30 */
  0, 44, 34, 39, 40, 45, 38, 94,  0, 51, 52, 41, 42, 47, 46, 95, /* 0x40 */
  0,  0, 49, 56, 48, 53, 96, 98, 85,  4, 50, 55, 56,  0, 97, 99, /* 0x50 */
 73, 72,100, 80, 61,104, 60,101,  0, 57, 79, 62, 66,105,102,103, /* 0x60 */
 58, 65, 63, 68, 69, 67,106,107,  0, 81, 64,  0,109, 74,108,  0, /* 0x70 */
  0,  0,  0,  0, 75						 /* 0x80 */
};
#endif
#if defined(LBOOT_SGI_PC)
extern int _force_pc2sgi;
#endif

int keyboarddevflag = 0;

static void
autorepeat( kbd_state_t *kbd, int seqid, int sc )
{
	int sgisc;

	/*  If any autorepeat is scheduled and we are on the
	 * sequence number Try to send the character.  If it
	 * gets delivered, continue to auto-repeat.
	 */
        if ((seqid == kbd->seqid) && (sc == kbd->repeating)) {
#if defined(LBOOT_SGI_PC)
			sgisc = _force_pc2sgi ? pc2sgi_map[sc]
					      : sc;
#endif
#if defined(SGI_ONLY)
			sgisc = pc2sgi_map[sc];
#endif
#if defined(PC_ONLY)
			sgisc = sc;
#endif
		if (idevGenBtnEvent(&kbd->info, sgisc,
				    IDEV_BTN_PRESS|IDEV_FORCE_EVENT)) {
			++kbd->seqid;
			kbd->reptid = STREAMS_TIMEOUT2(autorepeat, kbd,
					kbd->rep_rate, kbd->seqid, sc);
			return;
		}
	}

	kbd->repeating = 0;
	return;
}

/* ARGSUSED */
static int
kbd_ring_bell(kbd_state_t *kbd, register idevBellSettings *bell)
{
	return(pckbd_bell(bell->volume,bell->pitch,bell->duration));
}

static void
kbd_reinit(kbd_state_t *kbd)
{
	mblk_t *mbp;

	/*  Reinit keyboard since it appears autorepeat may be on.  Don't
	 * worry if we cannot allocate a buffer because if autorepeat really
	 * is on, we'll get another chance shortly.
	 */
	if ( mbp = allocb( 1, BPRI_MED ) ) {
		*mbp->b_wptr++ = 2;
		putnext( kbd->info.sInfo.wq, mbp ) ;
	}

	return;
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
#if _USE_EXTRALEDS			/* hangs some keyboards */
	if (pCtrl->which&IDEV_KC_SET_LEDS) {
		kbd->modes &= ~EXTRA_LED_MASK;
		kbd->modes |= (pCtrl->leds<<EXTRA_LED_SHIFT)&EXTRA_LED_MASK;
		if ((pCtrl->which&IDEV_KC_SET_KBD_LEDS) == 0)
			goto sendmodes;
	}
#endif
	if (pCtrl->which&IDEV_KC_SET_KBD_LEDS) {
		if (pCtrl->kbdleds_mask&IDEV_KC_CAPS_LOCK) {
			if (pCtrl->kbdleds_value&IDEV_KC_CAPS_LOCK)
				kbd->modes |=  LED_CAPSLOCK;
			else
				kbd->modes &= ~LED_CAPSLOCK;
		}
		if (pCtrl->kbdleds_mask&IDEV_KC_NUM_LOCK) {
			if (pCtrl->kbdleds_value&IDEV_KC_NUM_LOCK)
				kbd->modes |=  LED_NUMLOCK;
			else
				kbd->modes &= ~LED_NUMLOCK;
		}
		if (pCtrl->kbdleds_mask&IDEV_KC_SCROLL_LOCK) {
			if (pCtrl->kbdleds_value&IDEV_KC_SCROLL_LOCK)
				kbd->modes |=  LED_SCROLL;
			else
				kbd->modes &= ~LED_SCROLL;
		}

#if _USE_EXTRALEDS			/* hangs some keyboards */
sendmodes:
#endif
		/* send LED update to pckm driver */
		if ( mbp = allocb( 2, BPRI_MED ) ) {
			*mbp->b_wptr++ = 0;
			*mbp->b_wptr++ = kbd->modes;
			putnext( kbd->info.sInfo.wq, mbp ) ;
		}
		else
			cmn_err(CE_WARN, "Can't change keyboard settings "
				"(out of streams buffers).\n");
	}
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
	kbd_state_t *kbd = (kbd_state_t *)pInfo;
	unsigned char mask;
	unsigned char offset;
	int sgisc, tmp;

	while ( len-- ) {
		register int c = *str++;

		/* First stop any autorepeat in progress */
		/* We'll use "while" not "if" because there is a race here
		 * So long as we can "untimeout" faster than the repeat rate
		 * we'll always win the race.
		 */
		kbd->repeating = 0;
		while ( tmp = kbd->reptid ) {
			kbd->reptid = 0;
			untimeout( tmp );
		}

		/*  If we get an overflow notification from the keyboard
		 * (can happen if you gorilla test the keyboard), then
		 * reset the double scan count, and skip char to avoid
		 * overflow char 0x00 matching false kbd->lastchar.
		 */
		if (c == 0x00) {
			kbd->last_dscan = 0;
			continue;
		}

		/*  Sometimes break codes seem to get dropped (seen on
		 * 3rd party keyboards), so if we have to do this
		 * check after autorepeat is stoppped.
		 *
		 *  To recover from missing break, fake out break key
		 * to generate key-up properly.  If we get multiple
		 * failures within 3 seconds the keyboard probably
		 * did not get initialized correctly, so request that
		 * it get re-initialized.
		 * sure autorepeat is really off.
		 */
		if ( c == kbd->lastchar ) {
			if ( (lbolt-kbd->last_dscan) < 3*HZ ) {
				kbd->last_dscan = 0;
				kbd_reinit(kbd);
				continue;
			}

			kbd->last_dscan = lbolt;	/* keep last failure */
			if (c == KBD_BREAK) continue;	/* drop double break */
			kbd->lastchar = KBD_BREAK;	/* synth break key */
		}

		if (kbd->lastchar == KBD_BREAK) {
#if defined(LBOOT_SGI_PC) || defined(SGI_ONLY)
#if defined(LBOOT_SGI_PC)
			if (_force_pc2sgi) {
#endif
				sgisc = pc2sgi_map[c];
				if (sgisc) {
					idevGenBtnEvent(&kbd->info, sgisc, 0);
					c = 0;	/* dont trip autorepeat */
				}
#if defined(LBOOT_SGI_PC)
			}
#endif
#endif
#if defined (LBOOT_SGI_PC) || defined(PC_ONLY)
#if defined(LBOOT_SGI_PC)
			else {
#endif
				idevGenBtnEvent( &kbd->info, c, 0 ) ;
				c = 0;		/* dont trip autorepeat */
#if defined(LBOOT_SGI_PC)
			}
#endif
#endif
		}
		else if (c != KBD_BREAK) {
#if defined(LBOOT_SGI_PC)
			sgisc = _force_pc2sgi ? pc2sgi_map[c] : c;
#endif
#if defined(SGI_ONLY)
			sgisc = pc2sgi_map[c];
#endif
#if defined(PC_ONLY)
			sgisc = c;
#endif

			offset = idevOffset(sgisc);
			mask = idevMask(sgisc);

			if (sgisc
			    && idevGenBtnEvent(&kbd->info,sgisc,IDEV_BTN_PRESS)
			    && kbd->rep_on && kbd->rep_delay
			    && (kbd->brepeatable[ offset ] & mask) ) {
				++kbd->seqid;
				if (c != SCN_CAPSLOCK && c != SCN_NUMLOCK) {
					kbd->repeating = c;
					kbd->reptid = STREAMS_TIMEOUT2(
						autorepeat, kbd,
						kbd->rep_delay, kbd->seqid,
						c);
				}
			}
		}
		kbd->lastchar = c;

		/*
		 * Special safety catch to blow away a hung window server
		 *
		 * If the user hits LEFT-CTRL, LEFT-SHIFT, F12 and
		 * KEYPAD-/ simultaneously, blow away the owner of the
		 * active queue ( i.e. recieving focus ).
		 */
#if defined(LBOOT_SGI_PC)
		if ((_force_pc2sgi &&
#else
		if (
#endif
#if defined(LBOOT_SGI_PC) || defined(SGI_ONLY)
		     idevNth(kbd->bstate, 2  ) &&	/* LEFT-CTRL */
		     idevNth(kbd->bstate, 5  ) &&	/* L-SHIFT */
		     idevNth(kbd->bstate, 97 ) &&	/* F12 */
		     idevNth(kbd->bstate, 107)		/* KEYPAD-SLASH */
#endif
#if defined(LBOOT_SGI_PC)
		     ) ||
#endif
#if defined(LBOOT_SGI_PC) || defined(PC_ONLY)
		    (idevNth(kbd->bstate, 0x5e)&&	/* F12 */
		     idevNth(kbd->bstate, 0x77)&&	/* KEYPAD-SLASH */
		     idevNth(kbd->bstate, 0x11)&&	/* LEFT-CTRL */
		     idevNth(kbd->bstate, 0x12))	/* L-SHIFT */
#endif
		     ) {
			cmn_err( CE_CONT, "Killing the input queue owner\n" ) ;
			killManager( kbd->info.sInfo.rq ) ;
		}
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
	    strncpy((char *)kbd->map,pCtrl->data,IDEV_KEYMAP_NAME_LEN);
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
	    strncpy(pCtrl->data,(const char *)kbd->map,IDEV_CTRL_DATA_LEN);
	    pCtrl->data[IDEV_CTRL_DATA_LEN]= '\0';
	    return 1;
	}
	return 0;
}

/* needs to parallel table in pckeyboard.c */
static char *maps[] = {
	"US", "DE", "FR", "IT", "DK", "ES", "de_CH", "SE", "FI",
	"GB", "BE", "NO", "PT", "fr_CH", "de_CH", "de_CH",
	"fr_CH", "fr_CH", "JP"
};

static void
kbd_get_map_name( kbd_state_t *kbd )
{
	extern unsigned char kbdtype;
	extern char arg_keybd[];

	if (kbdtype != 0xff)
		strcpy((char *)kbd->map,maps[kbdtype]);
	else
		/* pass unknown keyboard on to let X server try it out */
		strcpy((char *)kbd->map,arg_keybd);
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
#if defined(LBOOT_SGI_PC)
			if (_force_pc2sgi) {
				_kbd_desc.nButtons = SGI_NBUTTONS;
				_kbd_desc.flags = IDEV_HAS_KEYMAP;
			}
#endif
#ifdef IP22
			/* Indigo2 kernel implements bell, while Indy
			 * implements the bell in the X server.
			 */
			_kbd_desc.nBells = is_fullhouse() ? 1 : 0;
#else
			/* Assume other (like TFP Indigo2) have a bell.
			 */
			_kbd_desc.nBells = 1;
#endif
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
				strncpy(desc->name,(const char *)kbd->map,
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
		case IDEVENABLEBUTTONS:
			found++;
			if (size>=sizeof(idevBitVals)) {
				ok= 1;
				idevUpdateBitArray(idevSize(KBD_NUM_KEYS),
						   (char *)kbd->bactive,
						   (idevBitVals *)stuff);
			}
			break;
		case IDEVSETLEDS:		/* no L1-L4 on PC keyboard */
		case IDEVGETLEDS:
			found++; ok++;
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
			found++; ok++;
			break;
		default: /* send other msgs down */
			break; /* FALL THROUGH */
	}
	*pFound=	found;
	return ok && found;
}

/* ARGSUSED */
static int
kbd_open( register queue_t *rq, dev_t *devp, int flag, int sflag, struct cred *crp )
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
		kbd->modes=	0;
		kbd->last_dscan=0;

		for (i=idevSize(KBD_NUM_KEYS)-1;i>=0;i--) {
			kbd->brepeatable[i] = ~0;
			kbd->bactive[i] = ~0;
			kbd->bstate[i] = 0;
		}

#if defined(LBOOT_SGI_PC)
		kbd->info.bInfo.nBtn = _force_pc2sgi?SGI_NBUTTONS:KBD_NUM_KEYS;
#else
		kbd->info.bInfo.nBtn = KBD_NUM_KEYS;
#endif
		kbd->info.bInfo.active = kbd->bactive;
		kbd->info.bInfo.state = kbd->bstate;

		kbd->repeating= 0;

		kbd->info.sInfo.shmiqid.devminor = 0;
		kbd->info.sInfo.shmiqid.index = 0;
		kbd->info.sInfo.rq = rq;
		kbd->info.sInfo.wq = WR(rq);
		kbd->info.sInfo.readData = kbd_intr;
		kbd->info.sInfo.writeIoctl = kbd_wioctl;
		WR(rq)->q_ptr =	rq->q_ptr = (caddr_t) &kbd->info;
	}

	return 0;
}

/* ARGSUSED */
static int
kbd_close( register queue_t *rq, int flag, struct cred *crp )
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
 * routine to intercept writes and get the kebyoard device minor and
 * index so that the NDS pseudo keyboard driver can use them
 */
int
pckidev_wput(register queue_t *wq, mblk_t *mp)
{

   if (mp->b_datap->db_type == M_PCPROTO) {
      /*
       * we need to save the device number &
       * index numbers for pseudo dual-head drivers
       */
       if (((struct shmqntc *)mp->b_rptr)->mtype == SHMIQ_NOTICE) {
           kbd_minor = ((struct shmqntc *)mp->b_rptr)->id.devminor;
           kbd_index = ((struct shmqntc *)mp->b_rptr)->id.index;

           }
       }

    /* just pass to idev_wput... */
    idev_wput(wq, mp);

    return 0;
}


	/*
	 * stream module definition
	 */

static struct module_info pckm_mod_info = {
	0,				/* module ID */
	KBD_NAME,			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	16,				/* lo-water mark */
};

static struct qinit pckbd_rinit = {
	idev_rput, NULL, kbd_open, kbd_close,
	NULL, &pckm_mod_info, NULL
};

static struct qinit pckbd_winit = {
	pckidev_wput, NULL, NULL, NULL,
	NULL, &pckm_mod_info, NULL
} ;

/* ********************* Only visible STREAMS structure ********************* */
struct streamtab keyboardinfo = {
	&pckbd_rinit, &pckbd_winit, 0, 0
} ;
