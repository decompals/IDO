#ifndef KBD_H
#define KBD_H 1

#define	KBD_NAME		"keyboard"
#define	KBD_TYPE		"KEYBOARD"
#define	KBD_NUM_KEYS		128

#define	KBD_OFFSET_MASK		0xf

#define	KBD_NUM_LEDS		4

#define	KBD_NUM_BELLS		0
#define	KBD_FLAGS		IDEV_HAS_KEYMAP
#define	KBD_KEYMAP_NAME		"SGI Standard"

#define	KBD_UPSTROKE		0x80

/* enforced by the driver */
#define	KBD_MIN_REPEAT_DELAY	(HZ/30)
#define	KBD_MAX_REPEAT_RATE	(HZ/50)

/* these defaults are meant to closely match the rate the hardware repeats */
/* hardware delay is .65 seconds, and repeat is 28 times per second */
#define KBD_DFLT_REPEAT_DELAY (HZ*2/3)  /* after ~2/3 a second */
#define KBD_DFLT_REPEAT_RATE  (HZ/25)  /* ~ 25 times a second */

#ifdef _KERNEL
typedef struct kbd_state {
	idevInfo		info;

	unsigned short int	rep_delay;
	unsigned short int	rep_rate;
	unsigned		reptid;
	int			seqid;
	long			last_bell;
	unsigned char		rep_on;
	unsigned char		lastchar;
	unsigned char		cntrl0;
	unsigned char		cntrl1;

	unsigned char		brepeatable[idevSize(KBD_NUM_KEYS)];
	unsigned char		bactive[idevSize(KBD_NUM_KEYS)];
	unsigned char		bstate[idevSize(KBD_NUM_KEYS)];
	
	unsigned char		map[IDEV_KEYMAP_NAME_LEN+1];

	unsigned char		ledstate;
	unsigned char		repeating; /* 1 if autorepeat is scheduled */

	/* Misc. Std. Driver stuff */
	unsigned char initialized;/* true if data structures are set */
				/* and init sequence has been sent to */
				/* device */
	unsigned char ready;	/* true if we've received confirmation */
				/* of the init sequence from the device */
} kbd_state_t;
#endif /* _KERNEL */

#endif
