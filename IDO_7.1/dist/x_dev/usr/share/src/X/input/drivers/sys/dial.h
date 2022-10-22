#ifndef DIAL_H
#define DIAL_H 1

#define	DIAL_NAME		"dial+buttons"
#define	DIAL_TYPE		"KNOB_BOX"
#define	DIAL_NUM_BUTTONS	32

#define	DIAL_NUM_VALUATORS		8
#define	DIAL_VALUATOR_MIN		((short)0x8000)
#define	DIAL_VALUATOR_MAX		((short)0x7fff)
#define	DIAL_VALUATOR_RESOLUTION	200
#define	DIAL_VALUATOR_MIN_RESOLUTION	8
#define	DIAL_VALUATOR_MAX_RESOLUTION	1024

#define	DIAL_NUM_LEDS		32

#define	DIAL_NUM_STR_DPYS	1
#define	DIAL_STR_DPY_MAX_LEN	8
#define	DIAL_STR_DPY_SYMBOL_SET	"dialbox"
#define	DIAL_STR_DPY_ALL_SAME	1

#ifdef _KERNEL
typedef struct dial_state {
	idevInfo		info;

	idevValuatorDesc	vdesc[DIAL_NUM_VALUATORS];
	idevTransform		vtrans[DIAL_NUM_VALUATORS];

	unsigned char		vactive[idevSize(DIAL_NUM_VALUATORS)];
	int			hwvstate[DIAL_NUM_VALUATORS];
	int			sysvstate[DIAL_NUM_VALUATORS];

	unsigned char		bactive[idevSize(DIAL_NUM_BUTTONS)];
	unsigned char		bstate[idevSize(DIAL_NUM_BUTTONS)];

	unsigned char		ledstate[idevSize(DIAL_NUM_LEDS)];

	short			sdpy_on;
	short			sdpy_len;
	char			sdpystate[DIAL_STR_DPY_MAX_LEN+1];

	/* state machine stuff */
	char			 state;
	char			 which;
	short			 value;

	unsigned		 checkinit_id;

	/* Misc. Std. Driver stuff */
	unsigned char initialized;/* true if data structures are set */
				/* and init sequence has been sent to */
				/* device */
	unsigned char ready;	/* true if we've received confirmation */
				/* of the init sequence from the device */
	unsigned char errorCount;/* count of bad messages for recovery */
} dial_state_t;

#endif /* _KERNEL */

#endif
